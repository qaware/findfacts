import scala.sys.process.Process

import IsabellePlugin.autoImport.{isabelleCommand, isabelleExecutable}
import sbt.Keys._
import sbt._
import sbt.complete.DefaultParsers._
import sbt.io.IO
import sbtassembly.AssemblyPlugin
import sbtassembly.AssemblyPlugin.autoImport._

/** Plugin for sbt projects defining a tool as Isabelle component. */
object IsabelleToolPlugin extends AutoPlugin {
  override def requires: Plugins = IsabellePlugin && AssemblyPlugin

  object autoImport {
    lazy val isabelleSettings = settingKey[Seq[String]]("isabelle tool additional settings")
    lazy val isabelleComponentAssembly = taskKey[File]("isabelle component assembly task")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      assemblyMergeStrategy in assembly := {
        case PathList("META-INF", "versions", "9", "module-info.class") => MergeStrategy.first
        case x => (assemblyMergeStrategy in assembly).value.apply(x)
      },
      isabelleComponentAssembly := {
        // Assemble fat jar for isabelle tool
        val fatJarName = assembly.value.getName
        val toolClass = (mainClass in (Compile, run)).value
          .getOrElse(throw new IllegalArgumentException("No tool (main) class specified!"))

        // Write settings file
        val file = (crossTarget in Compile).value / "etc" / "settings"
        val contents = "classpath \"$COMPONENT/" + fatJarName + "\"\n" +
          "isabelle_scala_service \"" + toolClass + "\"\n" +
          isabelleSettings.value.mkString("\n")
        IO.write(file, contents)

        file
      },
      run := {
        isabelleComponentAssembly.value

        // Parse tool args
        val args = spaceDelimited("<arg>").parsed

        // Run isabelle process
        val logger = streams.value.log
        logger.info("Running isabelle " + isabelleCommand.value + " " + args.mkString(" "))

        val resultCode = Process(isabelleExecutable.value.getAbsolutePath, isabelleCommand.value +: args).!(logger)

        if (resultCode != 0) {
          throw new IllegalStateException("Running isabelle tool failed with exit code " + resultCode)
        }
      }
    )
}
