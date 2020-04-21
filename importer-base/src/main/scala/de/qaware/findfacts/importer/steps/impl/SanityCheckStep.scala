package de.qaware.findfacts.importer.steps.impl

import com.typesafe.scalalogging.Logger

import de.qaware.findfacts.importer.ImportError
import de.qaware.findfacts.importer.TheoryView.Theory
import de.qaware.findfacts.importer.steps.{ImportStep, StepContext}

/** Performs some basic sanity checks on final data. */
final class SanityCheckStep extends ImportStep {
  private val logger = Logger[SanityCheckStep]

  override def apply(theory: Theory)(implicit ctx: StepContext): List[ImportError] = {
    logger.debug(s"Checking sanity for ${ctx.allEts.size} entities")
    // Check unique IDs
    val duplicateIDs = ctx.allEts.groupBy(_.id).filter(_._2.size > 1)

    // Check that uses are distinct
    val duplicateUses = ctx.theoryEts.filter(e => e.uses.distinct.size != e.uses.size)

    logger.debug(
      s"Finished checking sanity, found ${duplicateIDs.size + duplicateUses.size} potential issues")

    duplicateIDs.map(e => ImportError(this, e._1, "Duplicate id", e._2.mkString(","))).toList ++
      duplicateUses.map(e => ImportError(this, e.name, "Duplicate uses in entity", e.toString))
  }
}
