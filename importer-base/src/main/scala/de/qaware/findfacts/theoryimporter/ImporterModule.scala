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

import scala.language.postfixOps
// scalastyle:off
import de.qaware.findfacts.theoryimporter.steps.impl._
// scalastyle:on
import de.qaware.findfacts.theoryimporter.steps.{ImportStep, StepContext}

/** DI module for the importer. */
trait ImporterModule {
  private val logger = Logger[ImporterModule]

  // internals
  lazy val nameExtractor: NameExtractor = wire[NameExtractor]
  lazy val typeExtractor: TypExtractor = wire[TypExtractor]
  lazy val termExtractor: TermExtractor = wire[TermExtractor]
  lazy val propExtractor: PropExtractor = wire[PropExtractor]
  lazy val proofExtractor: ProofExtractor = wire[ProofExtractor]
  lazy val idBuilder: IdBuilder = wire[IdBuilder]

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

    theories flatMap { theory =>
      logger.info(s"Processing theory ${theory.name}")

      implicit val ctx: StepContext = StepContext()
      val errors = steps.flatMap(_(theory))

      errors foreach { error =>
        logger.warn(s"Error during import: $error")
        logger.debug(s"Details: ${error.getDebugInfo}")
      }
      errors
    } toList
  }
}
