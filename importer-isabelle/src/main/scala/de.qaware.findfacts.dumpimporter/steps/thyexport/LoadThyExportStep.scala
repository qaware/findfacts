package de.qaware.findfacts.dumpimporter.steps.thyexport

import scala.language.postfixOps

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.utils.ProgressLogger.withProgress
import de.qaware.findfacts.dumpimporter.dataaccess.RepositoryReader
import de.qaware.findfacts.dumpimporter.steps.{ImportStep, StepContext}
import isabelle.Export.Provider
import isabelle.{Export_Theory, Isabelle_System, Path}

final case class LoadThyException(msg: String) extends Exception

/** Import step to load a stable theory export.
  *
  * @param dump repository reader for the importer
  */
class LoadThyExportStep(dump: RepositoryReader, mapper: ThyMapper) extends ImportStep {

  private val logger = Logger[LoadThyExportStep]

  final val MarkupFile = ".*markup.yxml".r

  override def apply(implicit ctx: StepContext): Unit = {
    logger.info("Starting up isabelle system...")
    Isabelle_System.init()

    val baseDir = Path.explode(dump.rootdir.canonicalPath)

    val markupFiles = dump.readAll(MarkupFile)

    logger.info(s"Found ${markupFiles.size} theories. Extracting content...")

    withProgress(markupFiles) foreach { file =>
      val (sessionName, theoryName) = file.file.parent.name.split('.') match {
        case Array(name) => (name, name)
        case Array(s, t) => (s, t)
        case _ => throw LoadThyException(s"Unrecognized name format: ${file.file.parent.name}")
      }
      val provider = Provider.directory(baseDir, sessionName, theoryName)
      val theory = Export_Theory.read_theory(provider, sessionName, theoryName)

      mapper.mapTheory(theory)
    }

    logger.info(s"Extracted ${ctx.consts.size} constants, ${ctx.types.size} types, ${ctx.facts.size} facts.")
  }
}
