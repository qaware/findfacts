package de.qaware.findfacts.importer

import com.softwaremill.macwire.wire
import de.qaware.findfacts.importer.TheoryView.Theory
import de.qaware.findfacts.importer.steps.impl.{
  ExtractBlocksStep,
  ExtractConstantsStep,
  ExtractFactsStep,
  ExtractTypesStep,
  JoinBlockEntitiesStep,
  SanityCheckStep
}
import de.qaware.findfacts.importer.steps.impl.thy.{
  NameExtractor,
  ProofExtractor,
  PropExtractor,
  TermExtractor,
  TypExtractor
}
import de.qaware.findfacts.importer.steps.impl.util.IdBuilder

import de.qaware.findfacts.importer.steps.ImportStep

/** DI module for the importer. */
trait ImporterModule {
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
  )

  /** Runs the importer for a session.
    *
    * @param index to import into
    * @param theories to import in a session
    * @return import errors, if any
    */
  def importSession(index: String, theories: Seq[Theory]): List[ImportError]
}
