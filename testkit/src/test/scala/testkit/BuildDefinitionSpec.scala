package testkit

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import builddefinition.BuildDefinition
import builddefinition.BuildDefinition.Outcome
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

/** The build definition id and the marker rules. scala-build-app's `BuildDefinition.scala` is compiled into this
  * subproject's tests as well as into the meta-build, so what is asserted here is the code
  * `BuildDefinitionMarkerPlugin` runs on every sbt load.
  */
class BuildDefinitionSpec extends AnyFunSpec with Matchers {

  private def withRoot(test: File => Unit): Unit = {
    val root = Files.createTempDirectory("build-definition").toFile
    try test(root)
    finally delete(root)
  }

  private def delete(file: File): Unit = {
    Option(file.listFiles).foreach(_.foreach(delete))
    val _ = file.delete()
  }

  private def write(root: File, path: String, content: String): File = {
    val file = new File(root, path)
    file.getParentFile.mkdirs()
    Files.write(file.toPath, content.getBytes(StandardCharsets.UTF_8))
    file
  }

  private def run(dir: File, command: String*): String = {
    val process = new ProcessBuilder(command*).directory(dir).redirectErrorStream(true).start()
    val out = new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    process.waitFor() mustBe 0
    out.trim
  }

  private def marker(root: File): Option[String] = BuildDefinition.marked(root)

  private def definition(root: File): Unit = {
    val _ = write(root, "build.sbt", "name := \"x\"\nlazy val core = project\n")
    val _ = write(root, "project/build.properties", "sbt.version=1.13.1\n")
    val _ = write(root, "project/plugins.sbt", "addSbtPlugin(\"x\" % \"y\" % \"1\")\n")
    val _ = write(root, "project/Zeta.scala", "object Zeta\n")
    val _ = write(root, "project/alpha.scala", "object alpha\n")
  }

  describe("Cksum") {

    it("is POSIX cksum: the value and the byte count `cksum` prints") {
      withRoot { root =>
        val inputs = Seq(
          Array.emptyByteArray,
          "a".getBytes(StandardCharsets.UTF_8),
          "hello world\n".getBytes(StandardCharsets.UTF_8),
          Array.tabulate[Byte](70000)(i => (i * 31 + 7).toByte)
        )
        inputs.zipWithIndex.foreach { case (bytes, i) =>
          val file = new File(root, s"in-$i")
          Files.write(file.toPath, bytes)
          val sum = new BuildDefinition.Cksum
          sum.update(bytes)
          withClue(s"input $i: ") {
            s"${sum.value} ${sum.length}" mustBe run(root, "/bin/sh", "-c", s"cksum < '${file.getPath}'")
          }
        }
      }
    }
  }

  describe("id") {

    it("is the id the shell function that computed it before it moved here gives the same tree") {
      // The function ci/lib/build-definition.sh held in platform and playbook, word for word, so a marker
      // an earlier build wrote names the same definition this computes, and no checkout recompiles for the move.
      withRoot { root =>
        definition(root)
        val shell =
          """for f in build.sbt project/build.properties project/*.sbt project/*.scala; do
            |  [ -f "$f" ] || continue
            |  printf '%s\n' "$f"
            |  cat "$f"
            |done | cksum | awk '{print $1 "-" $2}'""".stripMargin
        BuildDefinition.id(root) mustBe run(root, "/usr/bin/env", "LC_ALL=C", "bash", "-c", shell)
      }
    }

    it("reads the definition's files in a fixed order, a dotfile and sbt's own target left out") {
      withRoot { root =>
        definition(root)
        val _ = write(root, "project/.hidden.sbt", "x\n")
        val _ = write(root, "project/target/Gen.scala", "x\n")
        val _ = write(root, "core/src/main/scala/A.scala", "x\n")
        BuildDefinition.files(root) mustBe Seq(
          "build.sbt",
          "project/build.properties",
          "project/plugins.sbt",
          "project/Zeta.scala",
          "project/alpha.scala"
        )
      }
    }

    it("changes with a file's content and with a file's name") {
      withRoot { root =>
        definition(root)
        val before = BuildDefinition.id(root)
        val _ = write(root, "build.sbt", "name := \"y\"\nlazy val core = project\n")
        val edited = BuildDefinition.id(root)
        edited must not be before
        new File(root, "project/alpha.scala").renameTo(new File(root, "project/beta.scala")) mustBe true
        BuildDefinition.id(root) must not be edited
      }
    }
  }

  describe("claim, what a hand-run sbt does") {

    it("writes the marker in a checkout with no compiled output") {
      withRoot { root =>
        definition(root)
        BuildDefinition.claim(root) mustBe Outcome.Claimed
        marker(root) mustBe Some(BuildDefinition.id(root))
        BuildDefinition.claim(root) mustBe Outcome.Current
      }
    }

    it("counts sbt's own meta-build as no compiled output") {
      withRoot { root =>
        definition(root)
        val _ = write(root, "project/target/scala-2.12/sbt-1.0/classes/A.class", "x")
        BuildDefinition.claim(root) mustBe Outcome.Claimed
      }
    }

    it("leaves output compiled under another definition, and its marker, alone") {
      withRoot { root =>
        definition(root)
        val _ = write(root, "target/.ci-build-definition", "1-1\n")
        val classes = write(root, "core/target/scala-3.10.1/classes/A.class", "x")
        BuildDefinition.claim(root) mustBe Outcome.LeftAlone
        marker(root) mustBe Some("1-1")
        classes.exists mustBe true
      }
    }

    it("leaves unmarked output alone") {
      withRoot { root =>
        definition(root)
        val _ = write(root, "target/scala-3.10.1/classes/A.class", "x")
        BuildDefinition.claim(root) mustBe Outcome.LeftAlone
        marker(root) mustBe None
      }
    }
  }

  describe("enforce, what a CI build does") {

    it("removes output compiled under another definition and marks this one, sbt's meta-build kept") {
      withRoot { root =>
        definition(root)
        val _ = write(root, "target/.ci-build-definition", "1-1\n")
        val core = write(root, "core/target/scala-3.10.1/classes/A.class", "x")
        val top = write(root, "target/scala-3.10.1/classes/B.class", "x")
        val streams = write(root, "core/target/streams/keep", "x")
        val meta = write(root, "project/target/scala-2.12/sbt-1.0/classes/C.class", "x")
        BuildDefinition.enforce(root) mustBe Outcome.Removed(
          Seq(new File(root, "core/target/scala-3.10.1"), new File(root, "target/scala-3.10.1"))
        )
        core.exists mustBe false
        top.exists mustBe false
        streams.exists mustBe true
        meta.exists mustBe true
        marker(root) mustBe Some(BuildDefinition.id(root))
      }
    }

    it("removes unmarked output: a checkout that cannot say what it was compiled under is not trusted") {
      withRoot { root =>
        definition(root)
        val classes = write(root, "core/target/scala-3.10.1/classes/A.class", "x")
        BuildDefinition.enforce(root) mustBe a[Outcome.Removed]
        classes.exists mustBe false
      }
    }

    it("keeps output compiled under this definition") {
      withRoot { root =>
        definition(root)
        val _ = write(root, "target/.ci-build-definition", BuildDefinition.id(root) + "\n")
        val classes = write(root, "core/target/scala-3.10.1/classes/A.class", "x")
        BuildDefinition.enforce(root) mustBe Outcome.Current
        classes.exists mustBe true
      }
    }

    it("marks a checkout with nothing compiled") {
      withRoot { root =>
        definition(root)
        BuildDefinition.enforce(root) mustBe Outcome.Claimed
        marker(root) mustBe Some(BuildDefinition.id(root))
      }
    }
  }

  describe("vouches, what a compile-cache push requires") {

    it("only for a marker naming the definition the tree holds now") {
      withRoot { root =>
        definition(root)
        BuildDefinition.vouches(root) mustBe false
        val _ = write(root, "target/.ci-build-definition", "999-999\n")
        BuildDefinition.vouches(root) mustBe false
        val _ = write(root, "target/.ci-build-definition", BuildDefinition.id(root) + "\n")
        BuildDefinition.vouches(root) mustBe true
        val _ = write(root, "build.sbt", "name := \"changed\"\n")
        BuildDefinition.vouches(root) mustBe false
      }
    }
  }
}
