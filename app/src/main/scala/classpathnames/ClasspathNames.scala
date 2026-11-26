package classpathnames

import java.nio.file.{Path, Paths}

/** The machine-independent name of a compile classpath entry, which zinc's classpath hash is taken over
  * ([[PathIndependentClasspath]], ISS-44010, ISS-130704).
  *
  * An entry under the checkout is named from `<checkout>`, so two checkouts of one commit name it alike. An entry under
  * one of sbt's `rootPaths` (the coursier cache, the ivy home, sbt's boot directory, the JDK) is named from that root's
  * key, `${CSR_CACHE}`, the way sbt's own file converter names it in the analysis, so a runner whose coursier cache sits
  * at another path (a Mac's `~/Library/Caches/Coursier`, a Fly builder's `~/.cache/coursier`) names the same library
  * jar alike. Any other entry keeps its absolute path. The entry's content hash is kept beside its name, so a library
  * whose bytes differ still reads as changed.
  *
  * No sbt type appears here on purpose. This file is compiled twice: by the meta-build, where
  * [[PathIndependentClasspath]] runs it, and by `testkit` as a test source, where `ClasspathNamesSpec` asserts it. It
  * therefore stays inside the syntax Scala 2.12 and Scala 3 share.
  */
object ClasspathNames {

  /** The name every path under the checkout root is given in place of the root. */
  val CheckoutName: Path = Paths.get("<checkout>")

  /** The prefixes an entry is named from, longest first so the most specific root wins: the checkout as
    * [[CheckoutName]], then each of `roots` as `${KEY}`.
    */
  def prefixes(checkout: Path, roots: Map[String, Path]): Seq[(Path, Path)] = {
    val base = normal(checkout)
    val named = roots.toSeq.collect {
      case (key, path) if normal(path) != base => normal(path) -> Paths.get("${" + key + "}")
    }
    ((base -> CheckoutName) +: named).sortBy { case (path, name) => (-path.getNameCount, name.toString) }
  }

  /** `file` named from the first of `prefixes` it lies under, else unchanged. */
  def name(prefixes: Seq[(Path, Path)], file: Path): Path = {
    val path = normal(file)
    prefixes
      .collectFirst { case (root, name) if path.startsWith(root) => name.resolve(root.relativize(path)) }
      .getOrElse(file)
  }

  private def normal(path: Path): Path = path.toAbsolutePath.normalize
}
