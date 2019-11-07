package de.qaware.yxml

import scala.util.parsing.input.Positional

/** Trait for pretty-printing of yxml asts. */
trait YxmlAstFormat {
  override def toString: String = format(0)

  /** Format AST element with indentation.
    *
    * @param indentLevel of this element
    * @return pretty-printed element
    */
  def format(indentLevel: Int): String
}

/** Trait for the sum type of the yxml abstract syntax tree. */
sealed trait YxmlAST extends Positional {

  /** String literal for one indentation level. */
  final val INDENT: String = "  "
}

/** Single-node wrapper for yxml forest.
  *
  * @param elems inner elements of this tree
  */
final case class Yxml(elems: Seq[Inner] = Seq.empty) extends YxmlAST with YxmlAstFormat {
  override def toString: String = format(0)
  override def format(indentLevel: Int): String = {
    elems.map(_.format(indentLevel)).mkString("\n")
  }
}

/** Union type for Markup and text. */
sealed trait Inner extends YxmlAST with YxmlAstFormat

/** Markup element.
  *
  * @param tag name of this element
  * @param kvs key-value pairs
  * @param inner yxml forest
  */
final case class Markup(tag: String, kvs: Seq[(String, String)] = Seq.empty, inner: Yxml = Yxml(Seq.empty))
    extends YxmlAST
    with Inner {
  override def format(indentLevel: Int): String = {
    val kvsFmt = kvs map { kv =>
      s"${kv._1}=${kv._2}"
    } mkString (" ")
    val innerFmt = inner.elems match {
      case Seq() => ""
      case Seq(Text(t)) => t
      case _ => "\n" + inner.format(indentLevel + 1) + "\n" + INDENT.repeat(indentLevel)
    }
    INDENT.repeat(indentLevel) + s"<$tag $kvsFmt $innerFmt>"
  }
}

/** Single key-value pair
  *
  * @param key of the pair
  * @param value of the pair
  */
final case class KVPair(key: String, value: String) extends YxmlAST

/** Inner body text
  *
  * @param str representation of text
  */
final case class Text(str: String) extends Inner {
  override def format(indentLevel: Int): String = {
    "  ".repeat(indentLevel) + str
  }
}
