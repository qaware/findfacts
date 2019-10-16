package de.qaware.yxml

import scala.util.parsing.combinator.Parsers

/** Parser error.
  *
  * @param location location of the error
  * @param msg error message
  */
case class YXMLParserError(location: Location, msg: String) extends YXMLParseError

/** Parser for yxml token stream. */
object YXMLTokenParser extends Parsers {
  override type Elem = YXMLToken

  // scalastyle:off scaladoc
  // justification: parsing rules are documented in antlr grammar file
  protected def yxml: Parser[YXML] = positioned {
    phrase(rep1(tree)) ^^ (tree => YXML(tree))
  }
  protected def tree: Parser[Tree] = positioned {
    (opt(ws) ~ tag ~ opt(ws) ~ rep(inner) ~ opt(ws) ~ close ~ opt(ws)) ^^ {
      case _ ~ tag ~ _ ~ inner ~ _ ~ _ ~ _ => Tree(tag, inner)
    }
  }
  protected def tag: Parser[Tag] = positioned {
    (x ~ y ~ name ~ rep(kvpair) ~ x) ^^ {
      case _ ~ _ ~ name ~ kvs ~ _ => Tag(name, kvs)
    }
  }
  protected def kvpair: Parser[KVPair] = positioned {
    (y ~ key ~ equal ~ value) ^^ {
      case _ ~ key ~ _ ~ value => KVPair(key, value)
    }
  }
  protected def inner: Parser[Inner] = positioned {
    (body | tree) ^^ identity
  }
  protected def close: Parser[Close] = positioned {
    (x ~ y ~ x) ^^ (_ => Close())
  }
  // Lower-level value parsers
  protected def name: Parser[Value] = positioned {
    rep1(ws | equal | text) ^^ (t => Value(mkTokenString(t)))
  }
  protected def key: Parser[Value] = positioned {
    rep1(ws | text) ^^ (t => Value(mkTokenString(t)))
  }
  protected def value: Parser[Value] = positioned {
    rep(ws | equal | text) ^^ (t => Value(mkTokenString(t)))
  }
  protected def body: Parser[Body] = positioned {
    ((text | equal) ~ rep(eqWsText)) ^^ { case t ~ ts => Body(mkTokenString(t +: ts)) }
  }
  // parse text with equal signs and whitespaces
  protected def eqWsText: Parser[TEXT] = positioned {
    (rep(ws) ~ (text | equal)) ^^ { case ts ~ t => TEXT(mkTokenString(ts :+ t)) }
  }
  // scalastyle:on scaladoc

  // Text token helper
  private def mkTokenString(in: Seq[TextToken]): String =
    in.map(_.str).foldLeft(new StringBuilder)((sb, str) => sb.append(str)).toString()

  // Lift lexer definitions to use here
  private def x = accept("x", { case x: X => x })
  private def y = accept("y", { case y: Y => y })
  private def equal = accept("equal", { case e: EQUAL => e })
  private def text = accept("text", { case t: TEXT => t })
  private def ws = accept("ws", { case w: WS => w })

  /** Parses tokens into yxml forest structure.
    *
    * @param tokens to parse
    * @return yxml forest or parse error
    */
  def apply(tokens: Seq[YXMLToken]): Either[YXMLParserError, YXML] = {
    val reader = new YXMLTokenReader(tokens)
    yxml(reader) match {
      case NoSuccess(msg, next) =>
        Left(YXMLParserError(Location(next.pos.line, next.pos.column), msg))
      case Success(result, _) =>
        Right(result)
    }
  }
}

/** Combined YXML lexer and parser. */
object YXMLParser {

  /** Parses yxml string into forest.
    *
    * @param yxml string to parse
    * @return yxml forest or parse error
    */
  def apply(yxml: String): Either[YXMLParseError, YXML] = {
    for {
      tokens <- YXMLLexer(yxml)
      ast <- YXMLTokenParser(tokens)
    } yield ast
  }
}
