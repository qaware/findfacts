package de.qaware.dumpimporter.steps

import com.typesafe.scalalogging.Logger
import de.qaware.dumpimporter.Config

/** Performs some basic sanity checks on final data.
  *
  * @param config of the importer
  */
class SanityCheckStep(override val config: Config) extends ImportStep {
  private val logger = Logger[SanityCheckStep]

  override def apply(context: StepContext): Unit = {
    // Check unique IDs
    val duplicateIDs = (context.types ++ context.facts ++ context.consts).groupBy(_.id).filter(_._2.size > 1)
    if (duplicateIDs.nonEmpty) {
      logger.error(s"Duplicate ids: \n\t${duplicateIDs.mkString("\n\t")}")
    }

    // Check unique names of named entities
    val duplicateNames =
      (context.types.groupBy(_.typeName) ++ context.facts.groupBy(_.name) ++ context.consts.groupBy(_.name))
        .filter(_._2.size > 1)
    if (duplicateNames.nonEmpty) {
      logger.error(s"Duplicate names: \n\t${duplicateNames.mkString("\n\t")}")
    }

    if (duplicateIDs.isEmpty && duplicateNames.isEmpty) {
      logger.info("Sanity checks passed")
    }
  }
}
