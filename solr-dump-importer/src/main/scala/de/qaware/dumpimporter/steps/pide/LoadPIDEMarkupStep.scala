package de.qaware.dumpimporter.steps.pide

import com.typesafe.scalalogging.Logger
import de.qaware.dumpimporter.Config
import de.qaware.dumpimporter.dataaccess.RepositoryReader
import de.qaware.dumpimporter.steps.{ImportStep, StepContext}
import de.qaware.dumpimporter.steps.pide.PIDENode.yxmlTree
import de.qaware.yxml.{YXMLParseError, YXMLParser}

/** Importer step that loads theory data from PIDE config.
  *
  * @param config of the importer
  */
class LoadPIDEMarkupStep(override val config: Config) extends ImportStep {

  /** Name of files containing PIDE markup. */
  final val MARKUP_FILE: String = "markup.yxml"

  private val logger = Logger[LoadPIDEMarkupStep]

  override def apply(ctx: StepContext): Unit = {
    // 1. Find all markup.yxml
    RepositoryReader(config.dump)
      .readAll(MARKUP_FILE.r)
      .foreach(file => {
        logger.info("Reading {}", file.sourceFile)
        val yxml = YXMLParser(file.content).toTry(implicitly[YXMLParseError <:< Throwable]).get
        val definitions = PIDEQuery
          .keyValue(PIDEField.NAME, PIDEField.DEFINITION)
          .parent()
          .parent()
          .find(yxml.trees)

        definitions.foreach(definition => {
          val uses = PIDEQuery.tag(PIDEField.ENTITY).and(PIDEQuery.key(PIDEField.REF)).find(Seq(definition))

        })

        logger.info("{} definitions found!", definitions.size)
      })
  }
}
