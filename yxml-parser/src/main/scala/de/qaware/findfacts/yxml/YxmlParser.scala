package de.qaware.findfacts.yxml

import scala.language.postfixOps

import com.typesafe.scalalogging.Logger
import fastparse.NoWhitespace.noWhitespaceImplicit
import fastparse.Parsed.{Failure, Success}
import fastparse.{End, P, Parsed, internal, parseInputRaw}

/** Parser error.
  *
  * @param offset location of the error
  * @param msg error message
  */
final case class YxmlParserError(override val offset: Int, override val msg: String) extends YxmlParseError

/** Parser for yxml token stream. */
object YxmlParser extends YxmlLexer {
  private val logger = Logger[YxmlParser.type]
  private implicit val parseLogger: internal.Logger = internal.Logger(logger.info(_))

  // scalastyle:off scaladoc
  /** Whole file. */
  protected def file[_: P]: P[Yxml] = P(markup ~/ yxml ~/ End).log map {
    case (root, remain) => new Yxml(root +: remain.elems)
  }

  /** Single yxml containing forest. */
  protected def yxml[_: P]: P[Yxml] = P(inner.rep).log.map(new Yxml(_))

  /** Inner 'tree' of forest. */
  protected def inner[_: P]: P[Inner] = P(markup | text).log

  /** Single markup element, containing data and a yxml element. */
  protected def markup[_: P]: P[Markup] = P(x ~ y ~ text ~/ kvPair.rep ~/ x ~/ yxml ~/ x ~/ y ~/ x).log map {
    case (_, _, tag, kvs, _, content, _, _, _) => Markup(tag.str, kvs, content)
  }

  /** Single key-value pair. Not parsed into [[YxmlAst]] element as pairs are stored directly. */
  protected def kvPair[_: P]: P[(String, String)] = P(y ~/ ts ~/ eq ~/ text.?).log map {
    case (_, key, _, valueOption) => (key.str, valueOption.map(_.str).getOrElse(""))
  }

  /** Body text element. */
  protected def text[_: P]: P[Text] = P((eq | ts).rep(1).!).log.map(new Text(_))
  // scalastyle:on scaladoc

  /** Parses tokens into yxml forest structure.
    *
    * @param yxml to parse
    * @return yxml forest or parse error
    */
  def apply(yxml: String): Either[YxmlParserError, Yxml] = {
    val start = System.currentTimeMillis()

    val result = Parsed.fromParsingRun(parseInputRaw(yxml, file(_), enableLogging = false))
    logger.debug("Token parsing took {} ms", System.currentTimeMillis() - start)

    result match {
      case f: Failure =>
        val trace = f.trace(true)
        Left(YxmlParserError(trace.index, trace.longMsg))
      case Success(result, _) => Right(result)
    }
  }
}
