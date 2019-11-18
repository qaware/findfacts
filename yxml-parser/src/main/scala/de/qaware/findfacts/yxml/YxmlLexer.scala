package de.qaware.findfacts.yxml

import scala.language.implicitConversions

import com.typesafe.scalalogging.Logger
import fastparse.NoWhitespace.noWhitespaceImplicit
import fastparse.Parsed.{Failure, Success}
import fastparse.{AnyChar, End, P, parse}

/** Wraps errors while lexing/parsing. */
trait YxmlParseError extends Throwable {

  /** Error offset, in total characters. */
  val offset: Int

  /** Error message */
  val msg: String
}

/** Lexer error.
  *
  * @param offset error location
  * @param msg error message
  */
final case class YxmlLexerError(override val offset: Int, override val msg: String) extends YxmlParseError

/** Lexer for yxml. */
class YxmlLexer {

  // scalastyle:off scaladoc
  /** 'X' token */
  protected def x[_: P]: P[X.type] = P("\u0005").map(_ => X)

  /** 'Y' token */
  protected def y[_: P]: P[Y.type] = P("\u0006").map(_ => Y)

  /** '=' token */
  protected def eq[_: P]: P[EQ.type] = P("=").map(_ => EQ)

  /** Text literal token */
  protected def ts[_: P]: P[TS] = P((!(x | y | eq) ~ AnyChar).rep(1).!).map(new TS(_))

  /** All tokens */
  protected def tokens[_: P]: P[Seq[YxmlToken]] = P((x | y | eq | ts).rep ~/ End)
  // scalastyle:on scaladoc
}

/** Companion object. */
object YxmlLexer {
  private val logger = Logger[YxmlLexer.type]

  /** Lexes input string into token stream.
    *
    * @param yxml the input to lex
    * @return the corresponding tokens stream or parse error
    */
  def apply(yxml: String): Either[YxmlParseError, List[YxmlToken]] = {
    val start = System.currentTimeMillis()

    val result = parse(yxml, new YxmlLexer().tokens(_))
    logger.debug("Tokenization took {} ms", System.currentTimeMillis() - start)

    result match {
      case f: Failure =>
        val trace = f.trace(true)
        Left(YxmlLexerError(trace.index, trace.longMsg))
      case Success(result, _) =>
        Right(result.toList)
    }
  }
}
