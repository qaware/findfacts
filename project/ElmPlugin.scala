import scala.collection.mutable.ArrayBuffer
import scala.sys.process.{Process, ProcessLogger}
import scala.util.Random

import com.typesafe.sbt.uglify.Import.{uglify, uglifyCompress, uglifyCompressOptions, uglifyMangle}
import com.typesafe.sbt.uglify.SbtUglify
import com.typesafe.sbt.web.Import.pipelineStages
import com.typesafe.sbt.web.SbtWeb.autoImport.Assets
import com.typesafe.sbt.web.SbtWeb.autoImport.WebKeys._
import com.typesafe.sbt.web.incremental.{OpFailure, OpInputHash, OpInputHasher, OpSuccess, syncIncremental}
import com.typesafe.sbt.web.pipeline.Pipeline
import com.typesafe.sbt.web.{CompileProblems, GeneralProblem, SbtWeb}
import sbt.Keys._
import sbt._

/** Plugin for the elm ui sub-project */
object ElmPlugin extends AutoPlugin {

  /** SbtWeb for incremental compilation. */
  override def requires: Plugins = SbtWeb && SbtUglify

  object autoImport {
    val elmMake = taskKey[Seq[File]]("Compile an Elm project.")

    val elmProjectJson = settingKey[File]("Elm project description.")
    val elmOutput = settingKey[File]("Elm output file.")
    val elmUglifyCompress = taskKey[Pipeline.Stage]("compress elm-generated js")
    val elmUglifyMangle = taskKey[Pipeline.Stage]("mangle elm-generated js")
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
          val command = "elm make src/Main.elm --optimize --output=" + (elmOutput in elmMake).value
          // Buffer logging to be able to report elm output should there be errors
          val logger = bufferingLogger
          val exitStatus = Process(command, (elmProjectJson in elmMake).value.getParentFile) ! logger

          val result =
            if (exitStatus == 0) {
              OpSuccess(srcs.toSet, Set((elmOutput in elmMake).value))
            } else {
              CompileProblems.report(rep, Seq(new GeneralProblem(logger.buffer.mkString("\n"), file("src/Main.elm"))))
              OpFailure
            }

          (Map(() -> result), ())
      }

      outs.toSeq
    },
    // First compress pure functions. This needs to be a different step from mangling according to elm doc. Then mangle.
    uglifyCompressOptions in elmUglifyCompress := Seq(
      "pure_funcs=\"F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9\"",
      "pure_getters",
      "keep_fargs=false",
      "unsafe_comps",
      "unsafe"
    ),
    uglifyMangle in elmUglifyCompress := false,
    elmUglifyCompress := uglify.value,
    uglifyCompress in elmUglifyMangle := false,
    elmUglifyMangle := uglify.value,
    // General project settings
    pipelineStages in Assets := Seq(elmUglifyCompress, elmUglifyMangle),
    unmanagedSourceDirectories in Assets += (sourceDirectory in elmMake).value,
    unmanagedSources in Assets := ((sourceDirectory in elmMake).value ** ("*.elm" -- (excludeFilter in elmMake).value)).get,
    resourceGenerators in Assets += elmMake.taskValue
  )

  /** Logger to buffer output, so compiler errors can be retrieved. */
  private def bufferingLogger =
    new ProcessLogger {
      val buffer = new ArrayBuffer[String]

      override def out(s: => String): Unit = buffer += s

      override def err(s: => String): Unit = buffer += s

      override def buffer[T](f: => T): T = f
    }
}
