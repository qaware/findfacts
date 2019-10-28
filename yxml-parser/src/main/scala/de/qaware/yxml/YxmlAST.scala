package de.qaware.yxml

import scala.util.parsing.input.Positional

trait YxmlAstFormat {
  override def toString: String = format(0)
  def format(indentLevel: Int): String
}

/** Trait for the sum type of the yxml abstract syntax tree. */
sealed trait YxmlAST extends Positional

final case class Yxml(elems: Seq[Inner] = Seq.empty) extends YxmlAST with YxmlAstFormat {
  override def toString: String = format(0)
  override def format(indentLevel: Int): String = {
    elems.map(_.format(indentLevel + 1)).mkString("\n")
  }
}

sealed trait Inner extends YxmlAST with YxmlAstFormat

final case class Markup(tag: String, kvs: Seq[(String, String)] = Seq.empty, inner: Yxml = Yxml(Seq.empty))
    extends YxmlAST
    with Inner {
  override def format(indentLevel: Int): String = {
    " ".repeat(indentLevel) + s"<$tag ${kvs map { kv =>
      kv._1 + "=" + kv._2
    } mkString (" ")}" + "\n" + inner.format(indentLevel + 1) + "\n" + " ".repeat(indentLevel) + ">"
  }
}

final case class KVPair(key: String, value: String) extends YxmlAST {
  override def toString: String = s"$key=$value"
}

final case class Text(str: String) extends Inner {
  override def format(indentLevel: Int): String = {
    " ".repeat(indentLevel) + str
  }
}
