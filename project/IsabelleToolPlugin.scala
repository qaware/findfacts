import scala.sys.process._

import sbt.Keys.{mainClass, crossTarget, run}
import sbt.complete.DefaultParsers._
import sbt.io.IO
import sbt.io.syntax.{File, _}
import sbt.{AutoPlugin, Compile, Def, InputKey, Plugins, SettingKey, TaskKey, inputKey, taskKey}
import sbtassembly.AssemblyPlugin
import sbtassembly.AssemblyPlugin.autoImport.assembly

object IsabelleToolPlugin extends AutoPlugin {
  override def requires: Plugins = AssemblyPlugin

  object autoImport {
    lazy val isabelleExecutable: SettingKey[File] = SettingKey[File]("isabelle executable")
    lazy val isabelleTool: SettingKey[String] = SettingKey[String]("isabelle task defined by project")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    assembly := {
      // Assemble fat jar for isabelle tool
      val fatJarName = assembly.value.getName
      val toolClass = (mainClass in (Compile, run)).value
        .getOrElse(throw new IllegalArgumentException("No tool (main) class specified!"))

      // Write settings file
      val file = (crossTarget in Compile).value / "etc" / "settings"
      val contents = "classpath \"$COMPONENT/" + fatJarName + "\"\nisabelle_scala_tools \"" + toolClass + "\""
      IO.write(file, contents)

      file
    },
    run := {
      // Make sure task is assembled
      assembly.value

      // Parse tool args
      val args = spaceDelimited("<arg>").parsed

      // Run isabelle process
      val resultCode = Process(isabelleExecutable.value.getAbsolutePath, isabelleTool.value +: args).!

      if (resultCode != 0) {
        throw new IllegalStateException("Running isabelle tool failed")
      }
    }
  )
}
