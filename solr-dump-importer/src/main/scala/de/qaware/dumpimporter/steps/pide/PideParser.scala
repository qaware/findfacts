package de.qaware.dumpimporter.steps.pide

import scala.language.postfixOps
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

/** Wrap Pide token with positional information.
  *
  * @param token to wrap
  * @param endOffset total number of characters from start  to the end of this token
  */
final case class PosToken(token: PideToken, endOffset: Int)

protected final class PideTokenReader(tokens: List[PideToken], startOffset: Int = 0) extends Reader[PosToken] {
  @SuppressWarnings(Array("TraversableHead")) // Justification: External reader interface is unsafe
  override def first: PosToken =
    PosToken(tokens.head, startOffset)
  override def rest: Reader[PosToken] = new PideTokenReader(tokens.tail, startOffset + tokens.head.data.length)
  override def pos: Position = NoPosition // Offset is not a relevant parser position
  override def atEnd: Boolean = tokens.isEmpty
}

/** Error during parsing.
  *
  * @param msg error message
  */
case class PideParserError(msg: String) extends PideParseError

/** Parser for Pide structure. */
object PideParser extends Parsers {
  override type Elem = PosToken

  protected def constantDef: Parser[PosToken] = (typDef ?) ~> (where ?) ~> defBlock

  protected def typDelim: Parser[PosToken] = (comment *) ~> (ws ?) ~> typDelimToken
  protected def typDef: Parser[PosToken] = typDelim ~> string

  protected def where: Parser[PosToken] = (comment *) ~> (ws ?) ~> whereToken

  protected def defDelim: Parser[PosToken] = (comment *) ~> (ws ?) ~> defDelimToken
  protected def defLine: Parser[PosToken] = opt(name) ~> string
  protected def defBlock: Parser[PosToken] = defLine ~ rep(defDelim ~> defLine) ^^ {
    case d ~ ds => PosToken(new Code((d :: ds).map(_.token.data).mkString(" | ")), ds.lastOption.getOrElse(d).endOffset)
  }

  protected def nameDelim: Parser[PosToken] = (comment *) ~> (ws ?) ~> nameDelimToken
  protected def name: Parser[PosToken] = string <~ nameDelim

  protected def string: Parser[PosToken] = (comment *) ~> (ws ?) ~> stringToken

  protected def comment: Parser[PosToken] = (ws ?) ~> commentToken

  // Lift lexed tokens for use here
  private def ws = accept("ws", { case ws @ PosToken(_: WhitespaceToken, _) => ws })
  private def defToken = accept("def", { case d @ PosToken(_: DefToken, _) => d })
  private def stringToken = accept("string", { case str @ PosToken(_: StringToken, _) => str })
  private def defDelimToken = accept("defDelimiter", { case delim @ PosToken(DefDelimToken, _) => delim })
  private def typDelimToken = accept("typDef", { case delim @ PosToken(TypeDelimToken, _) => delim })
  private def whereToken = accept("where", { case w @ PosToken(WhereToken, _) => w })
  private def commentToken = accept("comment", { case c @ PosToken(_: CommentToken, _) => c })
  private def nameDelimToken = accept("nameDelimiter", { case n @ PosToken(NameDelimToken, _) => n })

  /** Parses definition text for a constant.
    *
    * @param tokens list of tokens starting after the definition of the constant.
    * @return token containing source code and length of definition, or error if unsuccessful
    */
  def constantDef(tokens: List[PideToken]): Either[PideParseError, PosToken] = {
    val reader = new PideTokenReader(tokens)
    constantDef(reader) match {
      case NoSuccess(msg, _) => Left(PideParserError(msg))
      case Success(res, _) => Right(res)
    }
  }
}
