package de.qaware.findfacts.theoryimporter.steps.impl

import scala.language.postfixOps

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.theoryimporter.TheoryView.Theory
import de.qaware.findfacts.theoryimporter.steps.{ImportError, ImportStep, StepContext}

/** Importer step that loads theory data from PIDE config.
  *
  */
class LoadPideMarkupStep extends ImportStep {

  private val logger = Logger[LoadPideMarkupStep]

  override def apply(theories: Seq[Theory])(implicit ctx: StepContext): List[ImportError] = {
    ???
  }
}
