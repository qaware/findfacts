package de.qaware.dumpimporter.steps.pide

import scala.language.postfixOps

import com.typesafe.scalalogging.Logger
import de.qaware.common.solr.dt.{ConstEntity, DocumentationEntity, DocumentationType}
import de.qaware.dumpimporter.Config
import de.qaware.dumpimporter.dataaccess.{RepositoryFile, RepositoryReader}
import de.qaware.dumpimporter.dataaccess.treequery._
import de.qaware.dumpimporter.dataaccess.treequery.QueryDSL._
import de.qaware.dumpimporter.steps.{ImportStep, StepContext}
import de.qaware.dumpimporter.steps.pide.PIDEField._
import de.qaware.dumpimporter.steps.pide.PIDENode.fromInner
import de.qaware.dumpimporter.steps.pide.PIDEQuery._
import de.qaware.yxml.{Body, YXMLParser}

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

        // Try to parse file
        val yxml = YXMLParser(file.content) match {
          case Right(yxml) => yxml.trees.map(fromInner)
          case Left(parseError) =>
            logger.error("Could not parse {}: {}", file.sourceFile, parseError)
            throw parseError
        }
        logger.info("\tParsed in {}", System.currentTimeMillis() - start)
        start = System.currentTimeMillis()

        val constants = findConstants(yxml, file)
        logger.info("\t{} constants found in {}", constants.size, System.currentTimeMillis() - start)
      })
  }

  private def findConstants(yxml: Seq[PIDENode], file: RepositoryFile): Seq[ConstEntity] = {
    val consts = all parent of parent of first ofOne thats (key(DEF) and key(NAME) and keyValue(KIND, CONSTANT)) in yxml
    logger.debug("{} constant entities found", consts.size)

    consts map { const =>
      val entity = single first ofOne thats (tag(ENTITY) and key(DEF) and key(NAME) and keyValue(KIND, CONSTANT)) in const

      val constName = (single thats body in entity).getBody
      val constId = entity.getValue(DEF)

      val uses = all thats (tag(ENTITY) and key(REF)) in const

      val defBlock = single next ofOne thats (tag(KEYWORD2) and keyValue(KIND, WHERE)) in entity
      val defCode = (all thats body in (single thats tag(STRING) in defBlock)) collect {
        case PIDENode(Body(name)) => name
      } mkString

      // Constant type is not necessarily present in the markup
      ConstEntity(constId, file.sourceFile, 0, 0, constName, "", defCode, uses.map(_.getValue(REF)).toArray.distinct)
    }
  }

  private def findDocumentation(yxml: Seq[PIDENode], file: RepositoryFile): Seq[DocumentationEntity] = {
    (all thats tag(COMMENT) in yxml) map { comment =>
      val (text, docType) = (all thats not(tag(DELETE)) in comment).toSeq match {
        // Meta-language comment tags, i.e. '(*...*)', only contain <deleted> and body
        case Seq(PIDENode(Body(text))) => (text, DocumentationType.Meta)
        // Inline comment, extract all text
        case multiple =>
          ((all thats body in multiple) collect { case PIDENode(Body(text)) => text } mkString, DocumentationType.Inline)
      }
      DocumentationEntity(null, null, 0, 0, text, docType)
    }

    // TODO Latex markup
    // has (key value pair: KIND, COMMAND) and (next is (parent of (parent of (has tag PLAIN_TEXT)))
    //all parent of parent ofOne thats tag(PLAIN_TEXT) previous ofOne thats tag(KEYWORD1) in yxml
    Seq()
  }
}
