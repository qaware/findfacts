package de.qaware.yxml

import scala.util.parsing.combinator.Parsers
import com.typesafe.scalalogging.Logger

import scala.language.postfixOps

/** Parser error.
 *
 * @param location location of the error
 * @param msg      error message
 */
case class YxmlParserError(location: Location, msg: String) extends YxmlParseError

/** Parser for yxml token stream. */
object YxmlTokenParser extends Parsers {
  override type Elem = YxmlToken

  // scalastyle:off scaladoc
  // justification: parsing rules are documented in antlr grammar file
  protected def file: Parser[Yxml] = positioned {
    phrase(markup ~ yxml) ^^ { case root ~ remaining => Yxml(root +: remaining.elems) }
  }

  @inline protected def yxml: Parser[Yxml] = positioned {
    rep(inner) ^^ Yxml
  }

  @inline protected def inner: Parser[Inner] = positioned {
    (markup | text) ^^ identity
  }

  @inline protected def markup: Parser[Markup] = positioned {
    (x ~ y ~ text ~ (kvpair *) ~ x ~ yxml ~ x ~ y ~ x) ^^ {
      case _ ~ _ ~ name ~ kvs ~ _ ~ inner ~ _ ~ _ ~ _ =>
        Markup(name.str, kvs.map(e => (e.key, e.value)), inner)
    }
  }

  @inline protected def kvpair: Parser[KVPair] = positioned {
    (y ~ ts ~ eq ~ (text ?)) ^^ {
      case _ ~ key ~ _ ~ None => KVPair(key.str, "")
      case _ ~ key ~ _ ~ Some(value) => KVPair(key.str, value.str)
    }
  }

  @inline protected def text: Parser[Text] = positioned {
    ((eq | ts) +) ^^ { ts =>
      Text(mkTokenString(ts))
    }
  }

  // scalastyle:on scaladoc

  // Text token helper
  @inline private def mkTokenString(in: Seq[TextToken]): String = in match {
    case Seq(t) => t.str
    case Seq(t1, t2) => t1.str + t2.str
    case s => s.map(_.str).foldLeft(new StringBuilder)((sb, str) => sb.append(str)).toString()
  }

  // Lift lexer definitions to use here
  @inline private def x = accept("x", { case x: X => x })

  @inline private def y = accept("y", { case y: Y => y })

  @inline private def eq = accept("eq", { case e: EQ => e })

  @inline private def ts = accept("ts", { case t: TS => t })

  private val logger = Logger[YxmlTokenParser.type]

  /** Parses tokens into yxml forest structure.
   *
   * @param tokens to parse
   * @return yxml forest or parse error
   */
  def apply(tokens: Seq[YxmlToken]): Either[YxmlParserError, Yxml] = {
    val start = System.currentTimeMillis()
    val reader = new YXMLTokenReader(tokens)
    file(reader) match {
      case NoSuccess(msg, next) =>
        Left(YxmlParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) =>
        logger.debug("Token parsing took {} ms", System.currentTimeMillis() - start)
        Right(result)
    }
  }
}

/** Combined YXML lexer and parser. */
object YxmlParser {

  private val logger = Logger[YxmlParser.type]

  /** Parses yxml string into forest.
   *
   * @param yxml string to parse
   * @return yxml forest or parse error
   */
  def apply(yxml: String): Either[YxmlParseError, Yxml] = {
    val start = System.currentTimeMillis()
    for {
      tokens <- YxmlLexer(yxml)
      ast <- YxmlTokenParser(tokens)
    } yield {
      logger.debug("Total parsing took {} ms", System.currentTimeMillis() - start)
      ast
    }
  }
}
