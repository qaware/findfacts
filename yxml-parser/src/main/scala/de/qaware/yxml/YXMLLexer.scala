package de.qaware.yxml

import scala.language.implicitConversions
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/** Tokens in yxml. */
sealed trait YXMLToken

/** 'X' char, i.e. ascii char 5 */
case object X extends YXMLToken

/** 'Y' char, i.e. ascii char 6 */
case object Y extends YXMLToken

/** Equal sign. */
case object EQUAL extends YXMLToken {
  override def toString: String = "="
}

/** Arbitrary text.
  *
  * @param str text string
  */
case class TEXT(str: String) extends YXMLToken {
  override def toString: String = str
}

/** Wraps errors while lexing/parsing. */
trait YXMLParseError

/** Lexer error.
  *
  * @param msg error message
  */
case class YXMLLexerError(msg: String) extends YXMLParseError

protected class YXMLTokenReader(tokens: Seq[YXMLToken]) extends Reader[YXMLToken] {
  override def first: YXMLToken = tokens.head
  override def rest: Reader[YXMLToken] = new YXMLTokenReader(tokens.tail)
  override def pos: Position = NoPosition
  override def atEnd: Boolean = tokens.isEmpty
}

/** Lexer for yxml. */
object YXMLLexer extends RegexParsers {
  override val whiteSpace: Regex = "[ \t\r\n\f]+".r

  // scalastyle:off scaladoc
  // justification: parsing rules are documented in antlr grammar file
  protected def x: Parser[X.type] = '\u0005' ^^ (_ => X)
  protected def y: Parser[Y.type] = '\u0006' ^^ (_ => Y)
  protected def equal: Parser[EQUAL.type] = '=' ^^ (_ => EQUAL)
  protected def value: Parser[TEXT] = "[^\u0005\u0006=]+".r ^^ TEXT
  protected def tokens: Parser[List[YXMLToken]] = phrase(rep(x | y | equal | value))
  // scalastyle:on scaladoc

  /** Lexes input string into token stream.
    *
    * @param yxml the input to lex
    * @return the corresponding tokens stream
    */
  def apply(yxml: String): Either[YXMLParseError, List[YXMLToken]] = parse(tokens, yxml) match {
    case NoSuccess(msg, _) => Left(YXMLLexerError(msg))
    case Success(result, _) => Right(result)
  }
}
