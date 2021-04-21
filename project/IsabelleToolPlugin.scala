import com.typesafe.sbt.packager.Keys.stage
import com.typesafe.sbt.packager.archetypes.JavaAppPackaging
import sbt.Keys._
import sbt._
import sbt.complete.DefaultParsers._
import sbt.io.IO

/** Plugin for sbt projects defining a tool as Isabelle component. */
object IsabelleToolPlugin extends AutoPlugin {
  override def requires: Plugins = JavaAppPackaging

  object autoImport {
    lazy val isabelleProject = settingKey[Project]("isabelle project")
    lazy val isabelleCommand =
      settingKey[String]("isabelle tool command")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      stage := {
        val deps = (stage.value ** "*.jar").get()
        val toolClass = (mainClass in (Compile, run)).value

        // Write settings file
        val file = (target in Compile).value / "etc" / "settings"
        val contents = deps.map(dep => "classpath \"" + dep.getAbsolutePath + "\"\n").mkString +
          toolClass.map("isabelle_scala_service \"" + _ + "\"\n").getOrElse("")
        IO.write(file, contents)

        file
      },
      run := {
        // Execute isabelle run config for custom tool
        Def.inputTaskDyn {
          // Build tool assembly
          stage.value
          // Parse tool args
          val args = spaceDelimited("<arg>").parsed
          // Run
          (run in isabelleProject.value).toTask(" " + (isabelleCommand.value +: args).mkString(" "))
        }.evaluated
      }
    )
}
