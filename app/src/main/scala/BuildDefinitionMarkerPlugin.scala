import builddefinition.BuildDefinition
import builddefinition.BuildDefinition.Outcome
import sbt.*
import sbt.Keys.*

/** Keeps `target/.ci-build-definition` true of the checkout sbt loads, on every load, `reload` included (ISS-13194,
  * ISS-34021, ISS-165963). The id and every rule about the marker are [[BuildDefinition]]'s; this plugin only says
  * which rule a load runs, from the environment the caller set:
  *
  *   - `CI_BUILD_DEFINITION_CLEAN=1` -- a CI build about to compile: output compiled under another or an unknown
  *     definition is removed and the marker written ([[BuildDefinition.enforce]]). An incremental compile across a
  *     change of definition does not fit the heap the cold one does.
  *   - `CI_COMPILE_CACHE_PUSH` set (and no clean asked for) -- a compile-cache publish of a checkout another run
  *     compiled: the load fails unless the marker names this definition ([[BuildDefinition.vouches]]), so
  *     `pushRemoteCache` never publishes output compiled under another definition, or compiles one from cold.
  *   - neither -- a hand-run sbt: the marker is written only when no compiled output is here
  *     ([[BuildDefinition.claim]]), so a session's own compile in a fresh clone survives the CI build that follows
  *     it in the same tree, and a reload after an edit to `build.sbt` finds output compiled under the old definition
  *     and writes nothing.
  *
  * A failure of the first or the last warns and never fails the load: a wrong marker costs the next CI build a
  * compile, never a build.
  */
object BuildDefinitionMarkerPlugin extends AutoPlugin {

  override def trigger = allRequirements

  override def globalSettings: Seq[Setting[?]] = Seq(
    onLoad := onLoad.value.andThen(atLoad)
  )

  private def env(name: String): Option[String] = sys.env.get(name).map(_.trim).filter(_.nonEmpty)

  private def atLoad(state: State): State = {
    val root = state.baseDir
    if (env("CI_BUILD_DEFINITION_CLEAN").contains("1")) {
      attempt(state, "remove output compiled under another build definition") {
        BuildDefinition.enforce(root) match {
          case Outcome.Removed(dirs) =>
            state.log.info(
              "build definition differs from what this checkout last compiled; removed the incremental state: " +
                dirs.map(d => IO.relativize(root, d).getOrElse(d.getPath)).mkString(" ")
            )
          case _ => ()
        }
      }
    } else if (env("CI_COMPILE_CACHE_PUSH").nonEmpty) {
      val refusal =
        try {
          val current = BuildDefinition.id(root)
          val marked = BuildDefinition.marked(root)
          if (marked.contains(current)) None
          else Some(s"${BuildDefinition.Marker} names ${marked.getOrElse("nothing")}, the definition is $current")
        } catch { case e: Exception => Some(s"could not read the build definition: ${e.getMessage}") }
      refusal.fold(state) { why =>
        state.log.error(
          s"ci-cache: not published, this checkout's output was not compiled under its build definition ($why)"
        )
        state.fail
      }
    } else attempt(state, "claim the build definition marker")(BuildDefinition.claim(root))
  }

  private def attempt(state: State, what: String)(run: => Any): State = {
    try {
      val _ = run
    } catch { case e: Exception => state.log.warn(s"could not $what: ${e.getMessage}") }
    state
  }
}
