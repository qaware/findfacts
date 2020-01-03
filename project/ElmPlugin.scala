import com.typesafe.sbt.web.SbtWeb.autoImport.Assets
import com.typesafe.sbt.web.SbtWeb.autoImport.WebKeys._
import com.typesafe.sbt.web.incremental.{OpFailure, OpInputHash, OpInputHasher, OpSuccess, syncIncremental}
import com.typesafe.sbt.web.{CompileProblems, GeneralProblem, SbtWeb}
import sbt.Keys._
import sbt._

import scala.collection.mutable.ArrayBuffer
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Random

object ElmPlugin extends AutoPlugin {

  /** SbtWeb for incremental compilation. */
  override def requires: Plugins = SbtWeb

  object autoImport {
    val elmMake = taskKey[Seq[File]]("Compile an Elm project.")

    val elmProjectJson = settingKey[File]("Elm project description.")
    val elmOutput = settingKey[File]("Elm output file.")
  }

  import autoImport._

  override lazy val projectSettings: Seq[Def.Setting[_]] = Seq(
    // Elm project layout
    sourceDirectory in elmMake := baseDirectory.value / "src",
    elmProjectJson in elmMake := baseDirectory.value / "elm.json",
    elmOutput in elmMake := (webTarget in elmMake).value / "elmMain.js",
    // Make task, using incremental compilation
    elmMake := {
      val srcs = (unmanagedSources in Assets).value :+ (elmProjectJson in elmMake).value
      val hash =
        OpInputHash.hashString((srcs :+ (elmOutput in elmMake).value).mkString("\u0000") + Random.nextString(3))
      implicit val opInputHasher: OpInputHasher[Unit] = OpInputHasher[Unit](_ => hash)

      val rep = (reporter in elmMake).value

      val (outs, ()) = syncIncremental(streams.value.cacheDirectory / "run", Seq(())) {
        case Seq() => (Map.empty, ())
        case _ =>
          val command = "elm make src/Main.elm --output=" + (elmOutput in elmMake).value
          // Buffer logging to be able to report elm output should there be errors
          val logger = bufferingLogger
          val exitStatus = Process(command, (elmProjectJson in elmMake).value.getParentFile) ! logger

          val result = if (exitStatus == 0) {
            OpSuccess(srcs.toSet, Set((elmOutput in elmMake).value))
          } else {
            CompileProblems.report(rep, Seq(new GeneralProblem(logger.buffer.mkString("\n"), file("src/Main.elm"))))
            OpFailure
          }

          (Map(() -> result), ())
      }

      outs.toSeq
    },
    // General project settings
    unmanagedSourceDirectories in Assets += (sourceDirectory in elmMake).value,
    unmanagedSources in Assets := ((sourceDirectory in elmMake).value ** ("*.elm" -- (excludeFilter in elmMake).value)).get,
    resourceGenerators in Assets += elmMake.taskValue,
  )

  def bufferingLogger = new ProcessLogger {
    val buffer = new ArrayBuffer[String]
    override def out(s: => String): Unit = buffer += s
    override def err(s: => String): Unit = buffer += s
    override def buffer[T](f: => T): T = f
  }
}
