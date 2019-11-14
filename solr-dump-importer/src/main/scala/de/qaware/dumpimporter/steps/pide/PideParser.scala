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

/** Token reader that stores offset from start.
  *
  * @param tokens to read
  * @param startOffset initial offset of the reader
  */
protected final class PideTokenReader(tokens: List[PideToken], startOffset: Int = 0) extends Reader[PosToken] {
  @SuppressWarnings(Array("TraversableHead")) // Justification: External reader interface is unsafe
  override def first: PosToken =
    PosToken(tokens.head, startOffset + tokens.head.data.length)
  override def rest: Reader[PosToken] =
    new PideTokenReader(tokens.tail, startOffset + tokens.headOption.map(_.data.length).getOrElse(0))
  override def pos: Position = NoPosition // Offset is not a relevant parser position
  override def atEnd: Boolean = tokens.isEmpty
}

/** Error during parsing.
  *
  * @param msg error message
  */
final case class PideParserError(override val msg: String) extends PideParseError

/** Parser for Pide structure (outer syntax). */
object PideParser extends Parsers {
  override type Elem = PosToken

  // scalastyle:off scaladoc
  /** Top-level parser for comments */
  protected def comments: Parser[List[PosToken]] = {
    (rep(ws | defToken | stringToken | defDelimToken | typDelimToken | whereToken | forToken | nameDelimToken)
      ~> comment) *
  }

  /** Top-level parser for definition code of (most) constants. */
  protected def constantDef: Parser[PosToken] = (typDef ?) ~> (forDef ?) ~> (whereDelim ?) ~> defBlock

  /** Pares top-level entity tags. */
  protected def entityDef: Parser[PosToken] = (comment *) ~> (ws ?) ~> defToken

  /** Parses type delimiters. */
  protected def typDelim: Parser[PosToken] = (comment *) ~> (ws ?) ~> typDelimToken

  /** Parses type definitions. */
  protected def typDef: Parser[PosToken] = typDelim ~> string

  /** Parses for-clauses. */
  @SuppressWarnings(Array("TraversableLast")) // Justification: parser rules ensure there is at least one element
  protected def forDef: Parser[PosToken] = (comment *) ~> (ws ?) ~> forToken ~> (entityDef +) ^^ { es =>
    PosToken(Code(es.map(_.token.data).mkString(",")), es.last.endOffset)
  }

  /** Parses where delimiter */
  protected def whereDelim: Parser[PosToken] = (comment *) ~> (ws ?) ~> whereToken

  /** Parses delimiter between lines of definitions. */
  protected def defDelim: Parser[PosToken] = (comment *) ~> (ws ?) ~> defDelimToken

  /** Parses single definition lines. */
  protected def defLine: Parser[PosToken] = opt(name) ~> string

  /** Parses blocks of definition */
  @SuppressWarnings(Array("TraversableLast")) // Justification: parser rules ensure there is at least one element
  protected def defBlock: Parser[PosToken] = rep1sep(defLine, defDelim) ^^ { ds =>
    PosToken(Code(ds.map(_.token.data).mkString(" | ")), ds.last.endOffset)
  }

  /** Parses deflimiters for name bindings. */
  protected def nameDelim: Parser[PosToken] = (comment *) ~> (ws ?) ~> nameDelimToken

  /** Parses name bindings */
  protected def name: Parser[PosToken] = entityDef <~ nameDelim

  /** Parses Content of inner strings */
  protected def string: Parser[PosToken] = (comment *) ~> (ws ?) ~> stringToken

  /** Parses single comments */
  protected def comment: Parser[PosToken] = (ws ?) ~> commentToken
  // scalastyle:on scaladoc

  // Lift lexed tokens for use here
  private def ws = accept("ws", { case ws @ PosToken(_: WhitespaceToken, _) => ws })
  private def defToken = accept("def", { case d @ PosToken(_: DefToken, _) => d })
  private def stringToken = accept("string", { case str @ PosToken(_: StringToken, _) => str })
  private def defDelimToken = accept("defDelimiter", { case delim @ PosToken(DefDelimToken, _) => delim })
  private def typDelimToken = accept("typDef", { case delim @ PosToken(TypeDelimToken, _) => delim })
  private def whereToken = accept("where", { case w @ PosToken(WhereToken, _) => w })
  private def forToken = accept("for", { case f @ PosToken(ForToken, _) => f })
  private def commentToken = accept("comment", { case c @ PosToken(_: CommentToken, _) => c })
  private def nameDelimToken = accept("nameDelimiter", { case n @ PosToken(NameDelimToken, _) => n })

  private def parse[A](tokens: List[PideToken], parser: Parser[A]) = {
    val reader = new PideTokenReader(tokens)
    parser(reader) match {
      case NoSuccess(msg, _) => Left(PideParserError(msg))
      case Success(res, _) => Right(res)
    }
  }

  /** Parses definition text for a constant.
    *
    * @param tokens list of tokens starting after the definition of the constant.
    * @return token containing source code and length of definition, or error if unsuccessful
    */
  def constantDef(tokens: List[PideToken]): Either[PideParseError, PosToken] = parse(tokens, constantDef)

  /** Parses all comments.
    *
    * @param tokens all tokens of source file
    * @return parsed comment tokens with offsets stored as [[PosToken]]s
    */
  def comments(tokens: List[PideToken]): Either[PideParseError, List[PosToken]] = parse(tokens, comments)
}
