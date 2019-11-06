package de.qaware.dumpimporter.steps.pide

import scala.language.postfixOps
import com.typesafe.scalalogging.Logger
import de.qaware.common.solr.dt.{ConstEntity, DocumentationEntity, DocumentationType}
import de.qaware.dumpimporter.Config
import de.qaware.dumpimporter.dataaccess.{RepositoryFile, RepositoryReader}
import de.qaware.dumpimporter.dataaccess.treequery.QueryDSL._
import de.qaware.dumpimporter.dataaccess.treequery.QueryNode
import de.qaware.dumpimporter.steps.{ImportStep, StepContext}
import de.qaware.dumpimporter.steps.pide.PIDEField._
import de.qaware.dumpimporter.steps.pide.PIDENode.fromInner
import de.qaware.dumpimporter.steps.pide.PIDEQuery._
import de.qaware.yxml.{Text, YxmlParser}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/** Importer step that loads theory data from PIDE config.
  *
  * @param config of the importer
  */
class LoadPIDEMarkupStep(override val config: Config) extends ImportStep {

  /** Name of files containing PIDE markup. */
  final val MARKUP_FILE: String = "markup.yxml"

  private val logger = Logger[LoadPIDEMarkupStep]

  override def apply(ctx: StepContext): Unit = {
    val markupFiles = RepositoryReader(config.dump).readAll(MARKUP_FILE.r)
    logger.info("Found {} markup files", markupFiles.size)

    val consts = markupFiles.flatMap(file => {
      logger.info("Reading {}", file.relativeName)
      var start = System.currentTimeMillis()

      // Try to parse file
      val yxml = YxmlParser(file.file.contentAsString) match {
        case Right(yxml) => yxml.elems.map(fromInner)
        case Left(parseError) =>
          logger.error("Could not parse {}: {}", file.relativeName, parseError)
          throw parseError
      }
      logger.info("\tParsed in {}", System.currentTimeMillis() - start)
      start = System.currentTimeMillis()
      val yxmlContent = all first ofOne thats not(tag(ACCEPTED) or tag(RUNNING) or tag(FINISHED) or tag(TIMING)) in yxml
      val sourceTheory = file.relativeName.drop(1).dropRight(MARKUP_FILE.length + 1)
      val constants = PIDEParser(yxmlContent) map {_.copy(sourceFile = sourceTheory)}

      //val constants = findConstants(yxmlContent, file)
      logger.info("\t{} constants found in {}: {}", constants.size, System.currentTimeMillis() - start, constants)
      constants
    })
    ctx.consts ++= consts
  }

  private def findConstants(yxml: Seq[PIDENode], file: RepositoryFile): Seq[ConstEntity] = {
    val consts = all parent of parent of first ofOne thats (key(DEF) and key(NAME) and keyValue(KIND, CONSTANT)) in yxml
    logger.debug("{} constant entities found", consts.size)

    consts flatMap { const =>
      val entity = single firstInOrder ofOne thats (tag(ENTITY) and key(DEF) and key(NAME) and keyValue(KIND, CONSTANT)) in const

      // Check if this is a top-level definition, i.e. contains a 'where'
      all firstInOrder ofOne thats tag(STRING) anyNext ofOne thats (tag(KEYWORD2) and keyValue(KIND, KEYWORD)) parent ofOne thats body(
        WHERE) in const match {
        case Seq() => None
        case Seq(defBlock) =>
          val constName = (single thats body in entity).getBody
          val constId = entity.getValue(DEF)

          val uses = all thats (tag(ENTITY) and key(REF)) in const
          val defCode = (all thats body in (single thats tag(STRING) in defBlock)) collect {
            case PIDENode(Text(name)) => name
          } mkString

          // Constant type is not necessarily present in the markup
          Some(
            ConstEntity(
              constId,
              file.relativeName,
              0,
              0,
              constName,
              "",
              defCode,
              uses.map(_.getValue(REF)).toArray.distinct))
        case elems => throw new IllegalStateException(s"Found too many constant def blocks: $elems in $const")
      }
    }
  }

  private def findDocumentation(yxml: Seq[PIDENode], file: RepositoryFile): Seq[DocumentationEntity] = {
    (all thats tag(COMMENT) in yxml) map { comment =>
      val (text, docType) = (all thats not(tag(DELETE)) in comment).toSeq match {
        // Meta-language comment tags, i.e. '(*...*)', only contain <deleted> and body
        case Seq(PIDENode(Text(text))) => (text, DocumentationType.Meta)
        // Inline comment, extract all text
        case multiple: Seq[PIDENode] =>
          ((all thats body in multiple) collect { case PIDENode(Text(text)) => text } mkString, DocumentationType.Inline)
      }
      DocumentationEntity(null, null, 0, 0, text, docType)
    }

    // TODO Latex markup
    // has (key value pair: KIND, COMMAND) and (next is (parent of (parent of (has tag PLAIN_TEXT)))
    // all parent of parent ofOne thats tag(PLAIN_TEXT) previous ofOne thats tag(KEYWORD1) in yxml
    Seq()
  }
}

class PIDENodeReader(nodes: Seq[PIDENode]) extends Reader[PIDENode] {
  override def first: PIDENode = nodes.head
  override def rest: Reader[PIDENode] = new PIDENodeReader(nodes.tail)
  override def pos: Position = NoPosition
  override def atEnd: Boolean = nodes.isEmpty
}

object PIDEParser extends Parsers {
  override type Elem = PIDENode

  private val logger = Logger[PIDEParser.type]

  def apply(nodes: Seq[PIDENode]): Seq[ConstEntity] = {
    val reader = new PIDENodeReader(nodes)
    entities(reader) match {
      case NoSuccess(msg, _) =>
        logger.warn("Could not parse file: {}", msg)
        Seq.empty
      case Success(result, _) => result
    }
  }

  def entities: Parser[List[ConstEntity]] = {
    ignoreAnyUntil(constant)*
  }

  // Technical Stuff
  def ignoreUntil[U, T](ignore: Parser[T], parser: Parser[U]): Parser[U] = {
    (((ignore - parser)*) ~ parser) ^^ { case _ ~ r => r}
  }
  def ignoreAnyUntil[U](p: Parser[U]): Parser[U] = (((elem("", _ => true) - p) *) ~ p) ^^ { case _ ~ r => r }
  def ws: Parser[PIDENode] = matches(all root ofOne thats body("\\s+".r))
  def ignoreWSUntil[U](p: Parser[U]): Parser[U] = (((ws - p) *) ~ p) ^^ {case _ ~ r => r}
  def matches(query: QueryNode[PIDENode, Seq[PIDENode]]): Parser[PIDENode] = {
    elem("TODO", { e =>
      (query in e).size == 1
    })
  }
  /** Allows whitespace before a parser */
  def ` `[U](p: Parser[U]): Parser[U] = (ws*) ~> p


  // Application-view of entities
  def constant: Parser[ConstEntity] = ` `(defToken) ~ opt(` `(typeDef?) <~ ` `(where)) ~ ` `(defBlock) ^^ {
    case defN ~ Some(Some(defT)) ~ defB => ConstEntity(defN.id, null, defN.startPos, defB.endPos, defN.name, defT, defB.code, Array())
    case defN ~ _ ~ defB => ConstEntity(defN.id, null, defN.startPos, defB.endPos, defN.name, null, defB.code, Array())
  }

  case class Definition(name: String, id: String, startPos: Int)

  def defToken: Parser[Definition] = Parser { input =>
    if (input.atEnd) {
      Failure("Empty", input)
    } else {
      all thats (key(DEF) and key(NAME) and keyValue(KIND, CONSTANT)) in input.first match {
        case Seq(entity) =>
          Success(
            Definition((single thats body in entity).getBody, entity.getValue(DEF), entity.inner.pos.line),
            input.rest)
        case _ => Failure("Was no definition", input)
      }
    }
  }

  def typeDef: Parser[String] = ` `(`::`) ~>  ` `(defLine) ^^ (_.code)


  def where: Parser[PIDENode] = {
    matches(all root ofOne thats (tag(KEYWORD2) and keyValue(KIND, KEYWORD)) parent ofOne thats body(WHERE))
  }

  case class DefinitionCode(code: String, endPos: Int)
  def defBlock: Parser[DefinitionCode] = defLine ~ ((ignoreUntil(ws, `|`) ~> ignoreAnyUntil(defLine)) *) ^^ {
    case l ~ ls => DefinitionCode(l.code + ls.map(_.code).mkString("|"), (l +: ls).last.endPos)
  }

  def `::`: Parser[PIDENode] = {
    matches(all root ofOne thats tag(DELIMITER) parent ofOne thats tag(NO_COMPLETION) parent ofOne thats body("::"))
  }

  def `|`: Parser[PIDENode] = {
    matches(all root ofOne thats tag(DELIMITER) parent ofOne thats tag(NO_COMPLETION) parent ofOne thats body("|"))
  }
  def defLine: Parser[DefinitionCode] = Parser { input =>
    if (input.atEnd) {
      Failure("Empty", input)
    } else {

      // XML_BODY tags are generated by isabelle and do not belong to the original source text
      all thats body without(tag(XML_BODY)) in (
        all root ofOne thats tag(STRING) in input.first) match {
        case Seq() => Failure("Was no definition line", input)
        case elems => Success(DefinitionCode(elems.map(_.getBody).mkString(""), elems.last.inner.pos.line), input.rest)
      }
    }
  }
}
