package de.qaware.findfacts.theoryimporter

import com.softwaremill.macwire.wire
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.theoryimporter.TheoryView.Theory
import de.qaware.findfacts.theoryimporter.steps.ImportError
import de.qaware.findfacts.theoryimporter.steps.impl.thy.{NameExtractor, TermExtractor, TypExtractor}
// scalastyle:off
import de.qaware.findfacts.theoryimporter.steps.impl._
// scalastyle:on
import de.qaware.findfacts.theoryimporter.steps.{ImportStep, StepContext}

/** DI module for the importer. */
trait ImporterModule {
  private val logger = Logger[ImporterModule]

  // internals
  private val nameExtractor = wire[NameExtractor]
  private val termExtractor = wire[TermExtractor]
  private val typeExtractor = wire[TypExtractor]

  /** Steps of an import. */
  lazy val steps: Seq[ImportStep] = Seq(
    wire[LoadTheoryStep],
    wire[TranslateNameStep],
    wire[LoadDocumentationStep],
    wire[SanityCheckStep],
    indexWriterStep
  )

  /** Configured index writer has to be provided. */
  val indexWriterStep: ImportStep

  /** Runs the importer for a given theory.
    *
    * @param theories to import in a session
    * @return an empty try indicating success or failure
    */
  def importSession(theories: Seq[Theory]): List[ImportError] = {
    logger.info("Starting import...")

    implicit lazy val context: StepContext = StepContext()
    val result = steps.flatMap(_.apply(theories)(context)).toList

    logger.info("Finished importing.")
    result
  }
}
