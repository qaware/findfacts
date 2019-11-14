package de.qaware.dumpimporter.steps.pide

import scala.language.postfixOps

import com.typesafe.scalalogging.Logger
import de.qaware.common.solr.dt.DocumentationEntity
import de.qaware.dumpimporter.Config
import de.qaware.dumpimporter.dataaccess.RepositoryReader
import de.qaware.dumpimporter.steps.{ImportStep, StepContext}
import de.qaware.yxml.YxmlParser

/** Importer step that loads theory data from PIDE config.
  *
  * @param config of the importer
  */
class LoadPideMarkupStep(override val config: Config) extends ImportStep {

  /** Name of files containing PIDE markup. */
  final val MarkupFileName: String = "markup.yxml"

  private val logger = Logger[LoadPideMarkupStep]

  override def apply(ctx: StepContext): Unit = {
    val markupFiles = RepositoryReader(config.dump).readAll(s".*/$MarkupFileName".r)
    logger.info("Found {} markup files", markupFiles.size)

    markupFiles foreach { file =>
      logger.info("Reading {}", file.relativeName)
      val start = System.currentTimeMillis()

      // Try to parse file
      YxmlParser(file.file.contentAsString) match {
        case Left(parseError) =>
          logger.error("Could not parse {}: {}", file.relativeName, parseError)
          Seq.empty
        case Right(yxml) =>
          logger.info("\tParsed in {}", System.currentTimeMillis() - start)

          val sourceTheory = file.relativeName.drop(1).dropRight(MarkupFileName.length + 1)

          PideLexer(yxml) match {
            case Left(error) => logger.error(s"Could not lex pide tokens in $sourceTheory: $error")
            case Right(pideTokens) =>
              updateConst(ctx, pideTokens, sourceTheory)
              findComments(ctx, pideTokens, sourceTheory)
          }
      }
    }
  }

  protected def updateConst(ctx: StepContext, tokens: List[PideToken], file: String): Unit = {
    ctx.consts.filter(_.sourceFile == file) foreach { const =>
      val serials = ctx.serialsByEntity(const)
      serials foreach { s =>
        tokens collect { case dt @ DefToken(_, serial) if serial == s => dt } match {
          case List() => logger.whenDebugEnabled { logger.debug(s"Did not find entity for serial $s") }
          case List(defToken) =>
            val defBegin = tokens.indexOf(defToken) + 1
            PideParser.constantDef(tokens.drop(defBegin)) match {
              case Left(error) =>
                val context = tokens.drop(defBegin - 3).filter(_.getClass != classOf[WhitespaceToken]).take(10)
                logger.error(s"Could not parse definition for sid $s, at token $defBegin: ${error.msg}")
                logger.error(s"    Error location: '${context.map(_.data).mkString(" ")}'")
                logger.whenDebugEnabled { logger.debug(s"    Tokens: ${context.map(_.toString).mkString(" ")}") }
              case Right(res) =>
                ctx.updateEntity(
                  const,
                  const.copy(definitions = Array(res.token.data), endPos = const.endPos + res.endOffset))
            }
          case defs => logger.error(s"Found multiple definitions: $defs")
        }
      }
    }
  }

  protected def findComments(ctx: StepContext, tokens: List[PideToken], file: String): Unit = {
    PideParser.comments(tokens) match {
      case Left(error) => logger.error(s"Could not parse comments for file $file: $error")
      case Right(results) =>
        val commentEntities = results collect {
          case PosToken(CommentToken(data, commentType), offset) =>
            DocumentationEntity(file, offset - data.length, offset, data, commentType)
        }
        ctx.doc ++= commentEntities
    }
  }
}
