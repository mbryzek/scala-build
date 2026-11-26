package testkit

import java.nio.file.Paths

import classpathnames.ClasspathNames
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

/** How a classpath entry is named in zinc's classpath hash. scala-build-app's `ClasspathNames.scala` is compiled into
  * this subproject's tests as well as into the meta-build, so what is asserted here is the code
  * `PathIndependentClasspath` runs.
  */
class ClasspathNamesSpec extends AnyFunSpec with Matchers {

  // sbt's name for its coursier root, built so no compiler reads it as a missing interpolator.
  private val csr = "$" + "{CSR_CACHE}/"

  private val jar = "https/repo1.maven.org/maven2/org/playframework/play-json_3/3.0.6/play-json_3-3.0.6.jar"

  private def named(checkout: String, roots: Map[String, String], file: String): String =
    ClasspathNames
      .name(
        ClasspathNames.prefixes(Paths.get(checkout), roots.map { case (k, v) => k -> Paths.get(v) }),
        Paths.get(file)
      )
      .toString

  describe("ClasspathNames.name") {
    it("names a library jar alike on a Mac and a Fly builder, whose coursier caches sit at different paths") {
      val mac = named(
        "/Users/runner/ai/a/platform",
        Map("CSR_CACHE" -> "/Users/runner/Library/Caches/Coursier/v1"),
        s"/Users/runner/Library/Caches/Coursier/v1/$jar"
      )
      val fly = named(
        "/home/runner/ci/tree-1/platform",
        Map("CSR_CACHE" -> "/home/runner/.cache/coursier/v1"),
        s"/home/runner/.cache/coursier/v1/$jar"
      )
      mac mustBe csr + jar
      fly mustBe mac
    }

    it("names an entry under the checkout from <checkout>, in any checkout") {
      named("/a/platform", Map.empty, "/a/platform/core/target/classes") mustBe "<checkout>/core/target/classes"
      named("/b/platform", Map.empty, "/b/platform/core/target/classes") mustBe "<checkout>/core/target/classes"
    }

    it("names the checkout, not a root at the same path, which sbt's BASE is") {
      named("/a/platform", Map("BASE" -> "/a/platform"), "/a/platform/core/x") mustBe "<checkout>/core/x"
    }

    it("names an entry from the most specific root that holds it") {
      named(
        "/a/platform",
        Map("HOME" -> "/home/runner", "CSR_CACHE" -> "/home/runner/.cache/coursier/v1"),
        s"/home/runner/.cache/coursier/v1/$jar"
      ) mustBe csr + jar
      named("/home/runner/ci/platform", Map("HOME" -> "/home/runner"), "/home/runner/ci/platform/core/x") mustBe
        "<checkout>/core/x"
    }

    it("keeps the absolute path of an entry under no root") {
      named("/a/platform", Map("CSR_CACHE" -> "/c"), "/opt/lib/x.jar") mustBe "/opt/lib/x.jar"
    }

    it("does not read a sibling directory sharing a root's name as under it") {
      named("/a/platform", Map("CSR_CACHE" -> "/c/v1"), "/c/v10/x.jar") mustBe "/c/v10/x.jar"
    }
  }
}
