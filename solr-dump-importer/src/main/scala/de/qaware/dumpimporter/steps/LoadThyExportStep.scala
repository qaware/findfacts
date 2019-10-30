package de.qaware.dumpimporter.steps

import com.typesafe.scalalogging.Logger
import de.qaware.dumpimporter.Config
import de.qaware.dumpimporter.dataaccess.RepositoryReader
import de.qaware.yxml.YxmlParser

class LoadThyExportStep(override val config: Config) extends ImportStep {
  private val logger = Logger[LoadThyExportStep]

  override def apply(context: StepContext): Unit = {
    RepositoryReader(config.dump).readAll("consts".r).foreach {file =>
      YxmlParser(file.content) match {
        case Right(yxml) => logger.info("const: {}", yxml)
        case Left(error) => logger.warn("Could not parse export: {}", error)
      }
    }
  }
}
