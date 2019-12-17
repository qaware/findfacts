package de.qaware.findfacts.dumpimporter.steps

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.dumpimporter.Config

/** Performs some basic sanity checks on final data. */
class SanityCheckStep extends ImportStep {
  private val logger = Logger[SanityCheckStep]

  override def apply(implicit ctx: StepContext): Unit = {
    // Check unique IDs
    val duplicateIDs = ctx.allEntities.groupBy(_.id).filter(_._2.size > 1)
    if (duplicateIDs.nonEmpty) {
      logger.error(s"Duplicate ids: \n\t${duplicateIDs.mkString("\n\t")}")
    }

    // Check that names of each kind are unique
    val duplicateNames = ctx.theoryEntities.groupBy(e => (e.name, e.kind)).filter(_._2.size > 1)
    if (duplicateNames.nonEmpty) {
      logger.error(s"Duplicate names: \n\t${duplicateNames.mkString("\n\t")}")
    }

    if (duplicateIDs.isEmpty && duplicateNames.isEmpty) {
      logger.info("Sanity checks passed")
    }
  }
}
