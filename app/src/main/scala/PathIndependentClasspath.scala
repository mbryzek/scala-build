import java.nio.file.Path
import java.util.Optional

import classpathnames.ClasspathNames
import sbt.internal.inc.caching.ClasspathCache
import xsbti.{FileConverter, VirtualFile, VirtualFileRef}
import xsbti.compile.{Changes, CompileAnalysis, ExternalHooks, FileHash}

/** Zinc's classpath hash with every entry named independently of the machine and the checkout (ISS-44010,
  * ISS-130704).
  *
  * Zinc stores the hash of the compile classpath in a subproject's analysis, path and content per entry, and reads a
  * different one as a changed classpath. A shard pulls the analysis its build's Prepare compiled in another checkout,
  * often on another machine, so every sibling subproject's `target/.../classes` names another directory, and every
  * library jar sits in another coursier cache whenever the shard's machine keeps it at another path (a Mac's
  * `~/Library/Caches/Coursier` beside a Fly builder's `~/.cache/coursier`). On a changed classpath zinc looks every
  * class a library is recorded as providing up on the classpath, and Scala 3 records the anonymous classes play-json's
  * `Json.reads`, `Json.writes` and `Json.format` macros expand into (an `Outer$_$$anon`) as play-json's, a class no
  * classpath holds: play-json is read as changed, and every source of the subproject that uses it compiles again.
  * Measured on a platform shard, about 320 sources across core, assistant, family, integrations and idempotent before
  * its first suite; a shard on a machine that did not run its build's Prepare started 5 to 25 minutes later than one
  * that did.
  *
  * Named as [[ClasspathNames]] names them (`<checkout>/...`, `${CSR_CACHE}/...`), the entries are the same in every
  * checkout of one commit on every runner, so the hash compares equal and zinc takes its unchanged-classpath path,
  * which reads a library as changed only when its own stamp moved. Each entry's content hash is kept, so a library
  * whose bytes differ still changes the hash.
  */
object PathIndependentClasspath {

  def hash(prefixes: Seq[(Path, Path)], hashes: Array[FileHash]): Array[FileHash] =
    hashes.map(h => FileHash.of(ClasspathNames.name(prefixes, h.file), h.hash))

  /** `hooks` whose classpath hash names entries under `root` and under each of `roots` (sbt's `rootPaths`) from them:
    * the lookup sbt installed when there is one, every other answer left to it.
    */
  def hooks(hooks: ExternalHooks, root: Path, roots: Map[String, Path], converter: FileConverter): ExternalHooks = {
    val prefixes = ClasspathNames.prefixes(root, roots)
    val inner: Option[ExternalHooks.Lookup] =
      if (hooks.getExternalLookup.isPresent) Some(hooks.getExternalLookup.get) else None
    val lookup = new ExternalHooks.Lookup {
      override def getChangedSources(previous: CompileAnalysis): Optional[Changes[VirtualFileRef]] =
        inner.fold(Optional.empty[Changes[VirtualFileRef]])(_.getChangedSources(previous))
      override def getChangedBinaries(previous: CompileAnalysis): Optional[java.util.Set[VirtualFileRef]] =
        inner.fold(Optional.empty[java.util.Set[VirtualFileRef]])(_.getChangedBinaries(previous))
      override def getRemovedProducts(previous: CompileAnalysis): Optional[java.util.Set[VirtualFileRef]] =
        inner.fold(Optional.empty[java.util.Set[VirtualFileRef]])(_.getRemovedProducts(previous))
      override def shouldDoIncrementalCompilation(changed: java.util.Set[String], previous: CompileAnalysis): Boolean =
        inner.forall(_.shouldDoIncrementalCompilation(changed, previous))
      override def hashClasspath(classpath: Array[VirtualFile]): Optional[Array[FileHash]] = {
        val own = inner.map(_.hashClasspath(classpath)).filter(_.isPresent).map(_.get)
        val hashes = own.getOrElse(ClasspathCache.hashClasspath(classpath.toSeq.map(converter.toPath)))
        Optional.of(hash(prefixes, hashes))
      }
    }
    hooks.withExternalLookup(lookup)
  }
}
