package de.qaware.yxml

import scala.language.implicitConversions
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/** Wraps errors while lexing/parsing. */
trait YXMLParseError extends Throwable

/** Location of parse error.
  *
  * @param line of parse error
  * @param column of parse error
  */
case class Location(line: Int, column: Int) {
  override def toString: String = s"$line:$column"
}

/** Lexer error.
  *
  * @param location error location
  * @param msg error message
  */
case class YXMLLexerError(location: Location, msg: String) extends YXMLParseError

/** Positional (if possible) input reader for YXML tokens.
  *
  * @param tokens list of token to read
  */
protected class YXMLTokenReader(tokens: Seq[YXMLToken]) extends Reader[YXMLToken] {
  override def first: YXMLToken = tokens.head
  override def rest: Reader[YXMLToken] = new YXMLTokenReader(tokens.tail)
  override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)
  override def atEnd: Boolean = tokens.isEmpty
}

/** Lexer for yxml. */
object YXMLLexer extends RegexParsers {
  override def skipWhitespace: Boolean = false

  // scalastyle:off scaladoc
  // justification: parsing rules are documented in antlr grammar file
  protected def x: Parser[X] = positioned { "\u0005" ^^ (_ => X()) }
  protected def y: Parser[Y] = positioned { "\u0006" ^^ (_ => Y()) }
  protected def equal: Parser[EQUAL] = positioned { "=" ^^ (_ => EQUAL()) }
  protected def text: Parser[TEXT] = positioned { "[^\u0005\u0006=\\s](\\s*[^\u0005\u0006=\\s])*".r ^^ TEXT }
  protected def ws: Parser[WS] = positioned { "\\s+".r ^^ WS }
  protected def tokens: Parser[List[YXMLToken]] = phrase(rep(x | y | equal | text | ws)) ^^ (tokens => tokens)
  // scalastyle:on scaladoc

  /** Lexes input string into token stream.
    *
    * @param yxml the input to lex
    * @return the corresponding tokens stream
    */
  def apply(yxml: String): Either[YXMLParseError, List[YXMLToken]] = parse(tokens, yxml) match {
    case NoSuccess(msg, next) =>
      Left(YXMLLexerError(Location(next.pos.line, next.pos.column), msg))
    case Success(result, _) =>
      Right(result)
  }
}
