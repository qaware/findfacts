package de.qaware.yxml

import scala.util.parsing.input.Positional

trait YxmlAstFormat {
  override def toString: String = format(0)
  def format(indentLevel: Int): String
}

/** Trait for the sum type of the yxml abstract syntax tree. */
sealed trait YxmlAST extends Positional {
  final val INDENT: String = "  "
}

final case class Yxml(elems: Seq[Inner] = Seq.empty) extends YxmlAST with YxmlAstFormat {
  override def toString: String = format(0)
  override def format(indentLevel: Int): String = {
      elems.map(_.format(indentLevel)).mkString("\n")
  }
}

sealed trait Inner extends YxmlAST with YxmlAstFormat

final case class Markup(tag: String, kvs: Seq[(String, String)] = Seq.empty, inner: Yxml = Yxml(Seq.empty))
    extends YxmlAST
    with Inner {
  override def format(indentLevel: Int): String = {
    val kvsFmt = kvs map { kv => s"${kv._1}=${kv._2}"} mkString (" ")
    val innerFmt = inner.elems match {
      case Seq() => ""
      case Seq(Text(t)) => t
      case _ => "\n" + inner.format(indentLevel + 1) + "\n" + INDENT.repeat(indentLevel)
    }
    INDENT.repeat(indentLevel) + s"<$tag $kvsFmt $innerFmt>"
  }
}

// Intermediate node only
final case class KVPair(key: String, value: String) extends YxmlAST

final case class Text(str: String) extends Inner {
  override def format(indentLevel: Int): String = {
    "  ".repeat(indentLevel) + str
  }
}
