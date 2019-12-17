package de.qaware.findfacts.dumpimporter

import scala.util.Try

import better.files.File
import com.softwaremill.macwire.wire
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.utils.ProgressLogger.withProgress
import de.qaware.findfacts.dumpimporter.dataaccess.RepositoryReader
import de.qaware.findfacts.dumpimporter.steps.pide.LoadPideMarkupStep
import de.qaware.findfacts.dumpimporter.steps.{ImportStep, SanityCheckStep, StepContext, TranslateNameStep}

/** DI module for the importer. */
trait ImporterModule {
  // Internals
  private val repositoryReader = wire[RepositoryReader]
  private val logger = Logger[ImporterModule]

  // Steps
  lazy val pideImportStep: ImportStep = wire[LoadPideMarkupStep]
  lazy val translateNameStep: ImportStep = wire[TranslateNameStep]
  lazy val sanityCheckStep: ImportStep = wire[SanityCheckStep]

  implicit lazy val context: StepContext = StepContext()

  /**  */
  def thyImportStep: ImportStep

  /** Base directory for import has to be provided. */
  def baseDirectory: File

  /** Configured index writer has to be provided. */
  def indexWriterStep: ImportStep

  /** Runs the importer.
   *
   * @return an empty try indicating success or failure
   */
  def execute(): Try[Unit] = Try {
    logger.info("Starting import...")

    withProgress(Seq(
      thyImportStep,
      pideImportStep,
      translateNameStep,
      sanityCheckStep,
      indexWriterStep
    )).foreach(_.apply)

    logger.info("Finished importing.")
  }
}
