import sbt.Keys._
import sbt._
import sbt.complete.DefaultParsers._
import sbt.io.IO
import sbtassembly.AssemblyPlugin
import sbtassembly.AssemblyPlugin.autoImport._

/** Plugin for sbt projects defining a tool as Isabelle component. */
object IsabelleToolPlugin extends AutoPlugin {
  override def requires: Plugins = AssemblyPlugin

  object autoImport {
    lazy val isabelleProject = settingKey[Project]("isabelle project")
    lazy val isabelleCommand =
      settingKey[String]("isabelle tool command")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      assemblyMergeStrategy in assembly := {
        case PathList("META-INF", "versions", "9", "module-info.class") => MergeStrategy.first
        case x => (assemblyMergeStrategy in assembly).value.apply(x)
      },
      assembly := {
        // Assemble fat jar for isabelle tool
        val fatJarName = assembly.value.getName
        val toolClass = (mainClass in (Compile, run)).value

        // Write settings file
        val file = (target in Compile).value / "etc" / "settings"
        val contents = "classpath \"$COMPONENT/" + crossTarget.value.getName + "/" + fatJarName + "\"\n" +
          toolClass.map("isabelle_scala_service \"" + _ + "\"\n").getOrElse("")
        IO.write(file, contents)

        file
      },
      run := {
        // Execute isabelle run config for custom tool
        Def.inputTaskDyn {
          // Build tool assembly
          assembly.value
          // Parse tool args
          val args = spaceDelimited("<arg>").parsed
          // Run
          (run in isabelleProject.value).toTask(" " + (isabelleCommand.value +: args).mkString(" "))
        }.evaluated
      }
    )
}
