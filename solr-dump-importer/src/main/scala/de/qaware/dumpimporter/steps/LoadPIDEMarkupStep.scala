package de.qaware.dumpimporter.steps

import com.typesafe.scalalogging.Logger
import de.qaware.dumpimporter.Config

/** Importer step that loads theory data from PIDE config.
 *
 * @param config of the importer
 */
class LoadPIDEMarkupStep(override val config: Config) extends ImportStep {
  /** Name of files containing PIDE markup. */
  val MARKUP_FILE: String = "markup.yxml"

  private val logger = Logger[LoadPIDEMarkupStep]

  override def apply(ctx: StepContext): Unit = {
    // 1. Find all markup.yxml
    config.dump.list(_.name == MARKUP_FILE).foreach(file => {
      logger.info("Reading {}", file)
    })
  }
}
