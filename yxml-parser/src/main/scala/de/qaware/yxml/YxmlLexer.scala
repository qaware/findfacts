package de.qaware.yxml

import scala.language.implicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

import com.typesafe.scalalogging.Logger

/** Wraps errors while lexing/parsing. */
trait YxmlParseError extends Throwable

/** Location of parse error.
  *
  * @param line of parse error
  * @param column of parse error
  */
final case class Location(line: Int, column: Int) {
  override def toString: String = s"$line:$column"
}

/** Lexer error.
  *
  * @param location error location
  * @param msg error message
  */
final case class YxmlLexerError(location: Location, msg: String) extends YxmlParseError

/** Positional (if possible) input reader for YXML tokens.
  *
  * @param tokens list of token to read
  */
protected class YXMLTokenReader(tokens: Seq[YxmlToken]) extends Reader[YxmlToken] {
  @SuppressWarnings(Array("TraversableHead")) // Justification: External reader interface is unsafe
  @inline override def first: YxmlToken = tokens.head
  @inline override def rest: Reader[YxmlToken] = new YXMLTokenReader(tokens.tail)
  @inline override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
  @inline override def atEnd: Boolean = tokens.isEmpty
}

/** Lexer for yxml. */
object YxmlLexer extends RegexParsers {
  override def skipWhitespace: Boolean = false

  // scalastyle:off scaladoc
  // justification: parsing rules are documented in antlr grammar file
  protected def x: Parser[X] = positioned { "\u0005" ^^ (_ => X()) }
  protected def y: Parser[Y] = positioned { "\u0006" ^^ (_ => Y()) }
  protected def eq: Parser[EQ] = positioned { "=" ^^ (_ => EQ()) }
  protected def ts: Parser[TS] = positioned { "[^\u0005\u0006=]+".r ^^ TS }
  protected def tokens: Parser[List[YxmlToken]] = phrase(rep(x | y | eq | ts)) ^^ (tokens => tokens)
  // scalastyle:on scaladoc

  private val logger = Logger[YxmlLexer.type]

  /** Lexes input string into token stream.
    *
    * @param yxml the input to lex
    * @return the corresponding tokens stream
    */
  def apply(yxml: String): Either[YxmlParseError, List[YxmlToken]] = {
    val start = System.currentTimeMillis()
    parse(tokens, yxml) match {
      case NoSuccess(msg, next) =>
        Left(YxmlLexerError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) =>
        logger.debug("Tokenization took {} ms", System.currentTimeMillis() - start)
        Right(result)
    }
  }
}
