package de.qaware.dumpimporter.steps.pide

import com.typesafe.scalalogging.Logger
import de.qaware.common.solr.dt.ConstEntity
import de.qaware.dumpimporter.Config
import de.qaware.dumpimporter.dataaccess.{RepositoryFile, RepositoryReader}
import de.qaware.dumpimporter.steps.{ImportStep, StepContext}
import de.qaware.dumpimporter.steps.pide.PIDEField.{CONSTANT, DEF, DEFINITION, ENTITY, KIND, NAME, REF}
import de.qaware.dumpimporter.steps.pide.PIDENode.yxmlTree
import de.qaware.dumpimporter.steps.pide.PIDEQuery.{body, key, keyValue, tag}
import de.qaware.yxml.{YXML, YXMLParseError, YXMLParser}

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
        var start = System.currentTimeMillis()
        val yxml = YXMLParser(file.content).toTry(implicitly[YXMLParseError <:< Throwable]).get
        logger.info("\tParsed in {}", System.currentTimeMillis() - start)
        start = System.currentTimeMillis()
        val constants = findConstants(yxml, file)
        logger.info("\t{} constants found in {}", constants.size, System.currentTimeMillis() - start)
      })
  }

  private def findConstants(yxml: YXML, file: RepositoryFile): Seq[ConstEntity] = {
    keyValue(NAME, DEFINITION)
      .parent()
      .parent()
      .find(yxml.trees.map(yxmlTree))
      .map(definition => {
        val constEntity = tag(ENTITY)
          .and(key(DEF))
          .and(keyValue(KIND, CONSTANT))
          .and(key(NAME))
          .first()
          .findSingle(definition)
        logger.debug("{} constant entities found", constEntity)
        val constName = body().findSingle(constEntity).getBody
        val constId = constEntity.getValue(DEF)
        val uses = tag(ENTITY)
          .and(key(REF))
          .find(definition)
          .map(_.getValue(REF))
          .toSeq
          .distinct

        ConstEntity(constId, file.sourceFile, 0, 0, constName, "TODO", "TODO", uses.toArray)
      })
      .toSeq
  }
}
