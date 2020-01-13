package de.qaware.findfacts.theoryimporter.steps.impl

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.utils.LoggingUtils.doDebug
import de.qaware.findfacts.theoryimporter.TheoryView.Theory
import de.qaware.findfacts.theoryimporter.steps.{ImportError, ImportStep, StepContext}

/** Performs some basic sanity checks on final data. */
class SanityCheckStep extends ImportStep {
  private val logger = Logger[SanityCheckStep]

  override def apply(theories: Seq[Theory])(implicit ctx: StepContext): List[ImportError] = {
    // Check unique IDs
    val duplicateIDs = ctx.allEntities.groupBy(_.id).filter(_._2.size > 1)
    logger.whenDebugEnabled {
      if (duplicateIDs.nonEmpty) {
        logger.debug(s"Duplicate ids: \n\t${duplicateIDs.mkString("\n\t")}")
      }
    }

    // Check that names of each kind are unique
    val duplicateNames = ctx.theoryEntities.groupBy(e => (e.name, e.kind)).filter(_._2.size > 1)
    logger.whenDebugEnabled {
      if (duplicateNames.nonEmpty) {
        logger.debug(s"Duplicate names: \n\t${duplicateNames.mkString("\n\t")}")
      }
    }

    if (duplicateIDs.isEmpty && duplicateNames.isEmpty) {
      logger.info("Sanity checks passed")
    } else {
      logger.warn("Sanity checks failed")
    }

    duplicateIDs.map(e => doDebug(ImportError(this, e._1, "Dumplicate id", e._2.mkString(",")))).toList ++
      duplicateNames.map(e =>
        doDebug(ImportError(this, s"${e._1._1}:${e._1._2}", "Duplicate name", e._2.mkString(","))))
  }
}
