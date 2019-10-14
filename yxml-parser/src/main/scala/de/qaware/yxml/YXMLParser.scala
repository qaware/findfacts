package de.qaware.yxml

import scala.util.parsing.combinator.Parsers

/** Trait for the sum type of the yxml abstract syntax tree. */
sealed trait YXMLAST

/** A single yxml tree.
  *
  * @param open the opening tag
  * @param inner elements contained in the tree, i.e. text and more trees
  */
case class Tree(open: Open, inner: Seq[Inner]) extends YXMLAST with Inner

/** An opening tag.
  *
  * @param name of the element
  * @param kvPairs key=value pairs in the tag
  */
case class Open(name: String, kvPairs: Seq[KVPair]) extends YXMLAST

/** A single key=value pair.
  *
  * @param key of the pair
  * @param value of the pair
  */
case class KVPair(key: String, value: String) extends YXMLAST

/** Trait for the sum type of the inner tree elements (text and more trees). */
sealed trait Inner

/** Text body, to be contained in an yxml tree.
  *
  * @param str body text
  */
case class Body(str: String) extends Inner

/** Parser error.
  *
  * @param msg error message
  */
case class YXMLParserError(msg: String) extends YXMLParseError

/** Parser for yxml token stream. */
object YXMLTokenParser extends Parsers {
  override type Elem = YXMLToken

  // scalastyle:off scaladoc
  // justification: parsing rules are documented in antlr grammar file
  protected def yxml: Parser[Seq[Tree]] = phrase(rep(tree)) ^^ (e => e)
  protected def tree: Parser[Tree] = ((open ~ rep(inner) ~ close) ^^ { case o ~ b ~ _ => Tree(o, b) })
  protected def open: Parser[Open] = (x ~ y ~ tag ~ rep(kvpair) ~ x) ^^ {
    case _ ~ _ ~ name ~ kvs ~ _ => Open(name, kvs)
  }
  protected def kvpair: Parser[KVPair] = (y ~ key ~ equal ~ value) ^^ { case _ ~ k ~ _ ~ v => KVPair(k, v) }
  protected def inner: Parser[Inner] = (body | tree) ^^ (inner => inner)
  protected def close: Parser[Unit] = (x ~ y ~ x) ^^ (_ => ())

  protected def tag: Parser[String] = rep1(text | equal) ^^ (tokens => tokens.map(_.toString).reduce(_ + _))
  protected def key: Parser[String] = text ^^ (t => t.toString)
  protected def value: Parser[String] = opt(text) ^^ { _.map(_.toString).getOrElse("") }
  protected def body: Parser[Body] = rep1(text | equal) ^^ (tokens => Body(tokens.map(_.toString).reduce(_ + _)))
  // scalastyle:on scaladoc

  // Lift lexer definitions to use here
  private def x = accept("x", { case X => X })
  private def y = accept("y", { case Y => Y })
  private def equal = accept("equal", { case EQUAL => EQUAL })
  private def text = accept("text", { case t: TEXT => t })

  /** Parses tokens into yxml tree structure.
    *
    * @param tokens to parse
    * @return yxml tree or parse error
    */
  def apply(tokens: Seq[YXMLToken]): Either[YXMLParserError, Seq[Tree]] = {
    val reader = new YXMLTokenReader(tokens)
    yxml(reader) match {
      case NoSuccess(msg, next) => Left(YXMLParserError(msg))
      case Success(result, next) => Right(result)
    }
  }
}

/** Combined YXML lexer and parser. */
object YXMLParser {

  /** Parses yxml string into tree.
    *
    * @param yxml string to parse
    * @return yxml tree or parse error
    */
  def apply(yxml: String): Either[YXMLParseError, Seq[Tree]] = {
    for {
      tokens <- YXMLLexer(yxml)
      ast <- YXMLTokenParser(tokens)
    } yield ast
  }
}
