package builddefinition

import java.io.{File, FileInputStream, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

/** The build definition a checkout's compiled output was produced under (ISS-13194, ISS-34021, ISS-165963).
  *
  * WHAT SBT READS TO DECIDE THE CLASSPATH, as one value: every file whose content can change a subproject's
  * dependencies (`build.sbt`, `project/build.properties`, `project/<name>.sbt`, `project/<name>.scala`), in a fixed order, with
  * the names included so that adding or renaming one is a change too. The value is the POSIX `cksum` of that stream,
  * `<crc>-<bytes>`, so the id a marker written by an earlier build's shell holds is the id this computes for the same
  * tree. `project/target/` and `project/project/target/` are not read: they are sbt's own compiled meta-build, which
  * sbt rebuilds itself.
  *
  * THE MARKER, `target/.ci-build-definition`, says what the compiled output in this checkout was produced under. An
  * incremental compile across a change of definition holds zinc's previous analysis beside the new compile and does
  * not fit the heap a cold compile does, so a CI build removes output compiled under another (or an unknown)
  * definition before it compiles ([[enforce]]); a hand-run sbt only ever writes the marker where that is true of
  * everything already here ([[claim]]); and a compile-cache push publishes only output the marker vouches for
  * ([[vouches]]). `BuildDefinitionMarkerPlugin` runs these on every sbt load; nothing else computes the id.
  *
  * No sbt type appears here on purpose: like `CacheEntries.scala`, this file is compiled by the meta-build and by
  * `testkit` as a test source, where `BuildDefinitionSpec` asserts it, so it stays inside the syntax Scala 2.12 and
  * Scala 3 share.
  */
object BuildDefinition {

  val Marker = "target/.ci-build-definition"

  /** The definition's files under `root`, relative and in the order the id reads them: `build.sbt`,
    * `project/build.properties`, then `project/<name>.sbt` and `project/<name>.scala`, each sorted by name as a C-locale shell
    * glob sorts it and without names starting with a dot. Only regular files (a link to one counts).
    */
  def files(root: File): Seq[String] = {
    def glob(suffix: String): Seq[String] =
      Option(new File(root, "project").list())
        .map(_.toSeq)
        .getOrElse(Seq.empty)
        .filter(name => name.endsWith(suffix) && !name.startsWith("."))
        .sorted
        .map(name => s"project/$name")
    (Seq("build.sbt", "project/build.properties") ++ glob(".sbt") ++ glob(".scala"))
      .filter(path => new File(root, path).isFile)
  }

  /** The id of the definition `root` holds now. */
  def id(root: File): String = {
    val sum = new Cksum
    files(root).foreach { path =>
      sum.update((path + "\n").getBytes(StandardCharsets.UTF_8))
      val in = new FileInputStream(new File(root, path))
      try {
        val buffer = new Array[Byte](65536)
        var read = in.read(buffer)
        while (read >= 0) {
          sum.update(buffer, 0, read)
          read = in.read(buffer)
        }
      } finally in.close()
    }
    s"${sum.value}-${sum.length}"
  }

  /** What the marker names, trimmed as a shell's `$(cat ...)` reads it; [[None]] when there is none. */
  def marked(root: File): Option[String] = {
    val marker = new File(root, Marker)
    if (!marker.isFile) None
    else Some(new String(Files.readAllBytes(marker.toPath), StandardCharsets.UTF_8).trim).filter(_.nonEmpty)
  }

  /** Every subproject's compiled output: `<dir>/target/scala-*` and `target/scala-*`, sbt's own meta-build under
    * `project/` left out, since it is compiled before any load runs and is not output the marker speaks for.
    */
  def compiledOutput(root: File): Seq[File] = {
    def scalaDirs(target: File): Seq[File] =
      Option(target.listFiles())
        .map(_.toSeq)
        .getOrElse(Seq.empty)
        .filter(_.getName.startsWith("scala-"))
        .sortBy(_.getName)
    val subprojects = Option(root.listFiles())
      .map(_.toSeq)
      .getOrElse(Seq.empty)
      .filter(d => d.isDirectory && !d.getName.startsWith(".") && d.getName != "project")
      .sortBy(_.getName)
    (subprojects.map(new File(_, "target")) :+ new File(root, "target")).flatMap(scalaDirs)
  }

  /** What a load did to the marker. */
  sealed trait Outcome
  object Outcome {

    /** The marker already named this definition. */
    case object Current extends Outcome

    /** The marker now names this definition: no compiled output was here. */
    case object Claimed extends Outcome

    /** Output compiled under another or an unknown definition is here, and the marker was left as it was. */
    case object LeftAlone extends Outcome

    /** Output compiled under another or an unknown definition was removed, and the marker names this one. */
    final case class Removed(dirs: Seq[File]) extends Outcome
  }

  /** WHAT A HAND-RUN SBT WRITES WHEN IT LOADS. The marker is written only when it is about to be true of everything
    * here: it already names this definition, or no subproject has compiled output, so whatever this sbt compiles is
    * compiled under the definition it loaded (the fresh clone a fix session compiles in before a CI build runs in the
    * same tree). Output under another definition is left alone and so is the marker: removing it is [[enforce]]'s,
    * and only a CI build asks for that, because sbt deleting classes out from under its own running session (a
    * `reload` beside a live `run`) is not free.
    */
  def claim(root: File): Outcome = {
    val current = id(root)
    if (marked(root).contains(current)) Outcome.Current
    else if (compiledOutput(root).nonEmpty) Outcome.LeftAlone
    else {
      write(root, current)
      Outcome.Claimed
    }
  }

  /** WHAT A CI BUILD WRITES WHEN IT LOADS. No marker beside compiled output is a change too: a checkout that cannot say
    * what it was compiled under is not trusted. Either way the marker names this definition afterwards.
    */
  def enforce(root: File): Outcome = {
    val current = id(root)
    if (marked(root).contains(current)) Outcome.Current
    else {
      val output = compiledOutput(root)
      output.foreach(delete)
      write(root, current)
      if (output.isEmpty) Outcome.Claimed else Outcome.Removed(output)
    }
  }

  /** Whether the marker says the output here was compiled under the definition `root` holds now: what a compile-cache
    * push requires, so it never publishes output compiled under another definition, or none at all, which
    * `pushRemoteCache` would compile from cold.
    */
  def vouches(root: File): Boolean = marked(root).contains(id(root))

  private def write(root: File, value: String): Unit = {
    val marker = new File(root, Marker)
    marker.getParentFile.mkdirs()
    val _ = Files.write(marker.toPath, (value + "\n").getBytes(StandardCharsets.UTF_8))
  }

  private def delete(file: File): Unit = {
    if (file.isDirectory && !Files.isSymbolicLink(file.toPath))
      Option(file.listFiles()).map(_.toSeq).getOrElse(Seq.empty).foreach(delete)
    if (!file.delete() && file.exists()) throw new IOException(s"could not remove $file")
  }

  /** POSIX `cksum`: CRC-32 with polynomial 0x04C11DB7, most significant bit first, over the bytes and then the byte
    * count (least significant byte first, as few bytes as it takes), complemented.
    */
  final class Cksum {
    private var crc: Int = 0
    private var count: Long = 0L

    def update(bytes: Array[Byte]): Unit = update(bytes, 0, bytes.length)

    def update(bytes: Array[Byte], offset: Int, len: Int): Unit = {
      var i = offset
      while (i < offset + len) {
        step(bytes(i))
        i += 1
      }
      count += len
    }

    def length: Long = count

    def value: Long = {
      var c = crc
      var n = count
      while (n != 0) {
        c = (c << 8) ^ Cksum.Table(((c >>> 24) ^ (n & 0xff).toInt) & 0xff)
        n = n >>> 8
      }
      (~c).toLong & 0xffffffffL
    }

    private def step(b: Byte): Unit =
      crc = (crc << 8) ^ Cksum.Table(((crc >>> 24) ^ (b & 0xff)) & 0xff)
  }

  object Cksum {
    private val Polynomial = 0x04c11db7

    private val Table: Array[Int] = Array.tabulate(256) { i =>
      var c = i << 24
      var bit = 0
      while (bit < 8) {
        c = if ((c & 0x80000000) != 0) (c << 1) ^ Polynomial else c << 1
        bit += 1
      }
      c
    }
  }
}
