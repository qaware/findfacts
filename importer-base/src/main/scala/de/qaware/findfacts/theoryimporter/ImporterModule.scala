package de.qaware.findfacts.theoryimporter

import com.softwaremill.macwire.wire
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.theoryimporter.TheoryView.Theory
import de.qaware.findfacts.theoryimporter.steps.impl.thy.{
  NameExtractor,
  ProofExtractor,
  PropExtractor,
  TermExtractor,
  TypExtractor
}
import de.qaware.findfacts.theoryimporter.steps.impl.util.IdBuilder
// scalastyle:off
import de.qaware.findfacts.theoryimporter.steps.impl._
// scalastyle:on
import de.qaware.findfacts.theoryimporter.steps.{ImportStep, StepContext}

/** DI module for the importer. */
trait ImporterModule {
  private val logger = Logger[ImporterModule]

  // internals
  private lazy val nameExtractor = wire[NameExtractor]
  private lazy val typeExtractor = wire[TypExtractor]
  private lazy val termExtractor = wire[TermExtractor]
  private lazy val propExtractor = wire[PropExtractor]
  private lazy val proofExtractor = wire[ProofExtractor]
  private lazy val idBuilder = wire[IdBuilder]

  /** Steps of an import. */
  lazy val steps: Seq[ImportStep] = Seq(
    wire[ExtractConstantsStep],
    wire[ExtractFactsStep],
    wire[ExtractTypesStep],
    wire[ExtractBlocksStep],
    wire[JoinBlockEntitiesStep],
    wire[SanityCheckStep],
    indexWriterStep
  )

  /** Configured index writer has to be provided. */
  def indexWriterStep: ImportStep

  /** Runs the importer for a session.
    *
    * @param theories to import in a session
    * @return import errors, if any
    */
  def importSession(theories: Seq[Theory]): List[ImportError] = {
    logger.info("Starting import...")

    val errors = for {
      theory <- theories
      context = StepContext()
      _ = logger.info(s"Processing theory ${theory.name}")
      step <- steps
      error <- step.apply(theory)(context)
    } yield {
      logger.warn(s"Error during import: $error")
      logger.debug(s"Details: ${error.getDebugInfo}")
      error
    }

    logger.info("Finished importing.")
    errors.toList
  }
}
