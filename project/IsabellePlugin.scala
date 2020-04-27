import scala.sys.process.Process

import sbt.Keys._
import sbt._
import sbt.complete.DefaultParsers._

/**
 * Isabelle plugin wrapper. Preparation of the Isabelle instance isn't done here
 * as that lifecycle is different from the lifecycle of the SBT session.
 */
object IsabellePlugin extends AutoPlugin {

  object autoImport {
    lazy val isabelleExecutable = settingKey[File]("Compile isabelle jars")
    lazy val isabelleCommand = settingKey[String]("isabelle command for run task")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      run := {
        // Parse tool args
        val args = spaceDelimited("<arg>").parsed

        // Run isabelle process
        val logger = streams.value.log
        logger.info("Running isabelle " + isabelleCommand.value + " " + args.mkString(" "))

        val resultCode = Process(isabelleExecutable.value.getAbsolutePath, isabelleCommand.value +: args).!(logger)

        if (resultCode != 0) {
          throw new IllegalStateException(s"Running isabelle command failed with exit code $resultCode")
        }
      }
    )
}
