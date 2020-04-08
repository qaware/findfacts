package de.qaware.findfacts.importer.steps.impl

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.importer.ImportError
import de.qaware.findfacts.importer.TheoryView.Theory
import de.qaware.findfacts.importer.steps.{ImportStep, StepContext}

/** Performs some basic sanity checks on final data. */
class SanityCheckStep extends ImportStep {
  private val logger = Logger[SanityCheckStep]

  override def apply(theory: Theory)(implicit ctx: StepContext): List[ImportError] = {
    logger.debug(s"Checking sanity for ${ctx.allEts.size} entities")
    // Check unique IDs
    val duplicateIDs = ctx.allEts.groupBy(_.id).filter(_._2.size > 1)

    // Check that names of each kind are unique
    val duplicateNames = ctx.theoryEts.groupBy(e => (e.name, e.getClass)).filter(_._2.size > 1)

    logger.debug(s"Finished checking sanity, found ${duplicateIDs.size + duplicateNames.size} potential issues")

    duplicateIDs.map(e => ImportError(this, e._1, "Dumplicate id", e._2.mkString(","))).toList ++
      duplicateNames.map(e => ImportError(this, s"${e._1._1}:${e._1._2}", "Duplicate name", e._2.mkString(",")))
  }
}
