package de.qaware.findfacts.dumpimporter.steps.pide

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.solr.{ConstRecord, DocRecord, TheoryRecord}
import de.qaware.findfacts.dumpimporter.Config
import de.qaware.findfacts.dumpimporter.dataaccess.RepositoryReader
import de.qaware.findfacts.dumpimporter.steps.{ImportStep, StepContext}
import de.qaware.findfacts.scalautils.ProgressLogger.withProgress
import de.qaware.findfacts.yxml.YxmlParser

import scala.collection.mutable
import scala.language.postfixOps

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
    logger.info(s"Found ${markupFiles.size} markup files")

    val allSources = ctx.allEntities.map(_.sourceFile).to[mutable.Set]

    // Calculate this view only once
    val consts = ctx.consts

    withProgress(markupFiles) foreach { file =>
      val sourceTheory = file.relativeName.drop(1).dropRight(MarkupFileName.length + 1)
      allSources -= sourceTheory
      val start = System.currentTimeMillis()

      // Try to parse file
      YxmlParser(file.file.contentAsString) match {
        case Left(parseError) =>
          logger.error("Could not parse {}: {}", file.relativeName, parseError)
          Seq.empty
        case Right(yxml) =>
          logger.info(s"Parsed ${file.relativeName} in ${System.currentTimeMillis() - start}")

          PideLexer(yxml) match {
            case Left(error) => logger.error(s"Could not lex pide tokens in $sourceTheory: $error")
            case Right(pideTokens) =>
              // Find constant source text
              updateConst(ctx, consts, pideTokens, sourceTheory)
              // Find comments in markup
              findComments(ctx, pideTokens, sourceTheory)
          }
      }
    }
    // For each theory, markup should have been processed
    if (allSources.nonEmpty) {
      logger.error(s"Did not process markup of: $allSources")
    }
  }

  private def updateConst(ctx: StepContext, consts: Set[ConstRecord], tokens: List[PideToken], file: String): Unit = {
    consts.filter(_.sourceFile == file) foreach { const =>
      findDefStart(tokens, const, ctx) map { defBegin =>
        PideParser.constantDef(tokens.drop(defBegin)) match {
          case Left(error) => logErrorContext(tokens, defBegin, error)
          case Right(res) =>
            ctx.updateEntity(
              const,
              const.copy(sourceText = res.token.data, endPosition = const.endPosition + res.endOffset))
        }
      }
    }
  }

  private def updateThms(ctx: StepContext, tokens: List[PideToken], file: String): Unit = {
    ctx.facts.filter(_.sourceFile == file) foreach { thm =>
      findDefStart(tokens, thm, ctx) map { defBegin =>
        PideParser.thmDef(tokens.drop(defBegin)) match {
          case Left(error) => logErrorContext(tokens, defBegin, error)
          case Right(res) =>
            ctx.updateEntity(thm, thm.copy(sourceText = res.token.data, endPosition = thm.endPosition + res.endOffset))
        }
      }
    }
  }

  private def logErrorContext(tokens: List[PideToken], defStart: Int, error: PideParseError): Unit = {
    // scalastyle:off magic.number
    val context = tokens.drop(defStart - 3).filter(_.getClass != classOf[WhitespaceToken]).take(10)
    // scalastyle:on magic.number
    logger.whenDebugEnabled {
      logger.debug(s"Could not parse definition for at token $defStart: ${error.msg}")
      logger.debug(s"    Error location: '${context.map(_.data).mkString(" ")}'")
      logger.debug(s"    Tokens: ${context.map(_.toString).mkString(" ")}")
    }
  }

  private def findDefStart(tokens: List[PideToken], entity: TheoryRecord, ctx: StepContext): Option[Int] = {
    val serials = ctx.serialsById(entity.id)
    serials flatMap { s =>
      tokens collect { case dt @ DefToken(_, serial) if serial == s => dt } match {
        case List() =>
          logger.whenDebugEnabled { logger.debug(s"Did not find entity for serial $s") }
          None
        case List(defToken) => Some(tokens.indexOf(defToken) + 1)
        case defs: Any =>
          logger.error(s"Found multiple definitions: $defs")
          None
      }
    } toSeq match {
      case Seq(start) => Some(start)
      case Seq() =>
        logger.whenDebugEnabled { logger.debug(s"Did not find definition for ${entity.kind} ${entity.name}") }
        None
      case defs: Any =>
        logger.error(s"Found multiple definitions for entity $entity: $defs")
        None
    }
  }

  private def findComments(ctx: StepContext, tokens: List[PideToken], theory: String): Unit = {
    PideParser.comments(tokens) match {
      case Left(error) => logger.error(s"Could not parse comments for file $theory: $error")
      case Right(results) =>
        results foreach {
          case PosToken(CommentToken(data, commentType), offset) =>
            ctx.addEntity(DocRecord(theory, offset - data.length, offset, data, commentType.toString))
          case _ =>
        }
    }
  }
}
