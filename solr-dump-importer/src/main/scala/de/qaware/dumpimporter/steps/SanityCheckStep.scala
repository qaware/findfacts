package de.qaware.dumpimporter.steps

import com.typesafe.scalalogging.Logger
import de.qaware.dumpimporter.Config

/** Performs some basic sanity checks on final data.
  *
  * @param config of the importer
  */
class SanityCheckStep(override val config: Config) extends ImportStep {
  private val logger = Logger[SanityCheckStep]

  override def apply(ctx: StepContext): Unit = {
    // Check unique IDs
    val duplicateIDs = (ctx.types ++ ctx.facts ++ ctx.consts ++ ctx.doc).groupBy(_.id).filter(_._2.size > 1)
    if (duplicateIDs.nonEmpty) {
      logger.error(s"Duplicate ids: \n\t${duplicateIDs.mkString("\n\t")}")
    }

    // Check unique names of named entities
    val duplicateNames =
      (ctx.types.groupBy(_.typeName) ++ ctx.facts.groupBy(_.name) ++ ctx.consts.groupBy(_.name))
        .filter(_._2.size > 1)
    if (duplicateNames.nonEmpty) {
      logger.error(s"Duplicate names: \n\t${duplicateNames.mkString("\n\t")}")
    }

    if (duplicateIDs.isEmpty && duplicateNames.isEmpty) {
      logger.info("Sanity checks passed")
    }
  }
}
