package de.qaware.findfacts.yxml

/** Trait for pretty-printing of yxml asts. */
trait YxmlAstFormat extends Any {
  override def toString: String = format(0)

  /** Format AST element with indentation.
    *
    * @param indentLevel of this element
    * @return pretty-printed element
    */
  def format(indentLevel: Int): String
}
object YxmlAstFormat {

  /** String literal for one indentation level. */
  final val Indent: String = "  "
}

/** Trait for the sum type of the yxml abstract syntax tree. */
sealed trait YxmlAst extends Any

/** Single-node wrapper for yxml forest.
  *
  * @param elems inner elements of this tree
  */
final class Yxml(val elems: Seq[Inner] = Seq.empty) extends AnyVal with YxmlAst with YxmlAstFormat {
  override def toString: String = format(0)
  override def format(indentLevel: Int): String =
    elems.map(_.format(indentLevel)).mkString("\n")
}

/** Companion object to pattern match value class. */
object Yxml {
  def unapply(arg: Yxml): Option[Seq[Inner]] = Some(arg.elems) // scalastyle:ignore
}

/** Union type for Markup and text. */
sealed trait Inner extends Any with YxmlAst with YxmlAstFormat

/** Markup element.
  *
  * @param tag name of this element
  * @param kvs key-value pairs
  * @param inner yxml forest
  */
final case class Markup(tag: String, kvs: Seq[(String, String)] = Seq.empty, inner: Yxml = new Yxml()) extends Inner {
  override def format(indentLevel: Int): String = {
    val kvsFmt = kvs.map(kv => s"${kv._1}=${kv._2}").mkString(" ")
    val innerFmt = inner.elems match {
      case Seq() => ""
      case Seq(Text(t)) => t
      case _ => "\n" + inner.format(indentLevel + 1) + "\n" + YxmlAstFormat.Indent.repeat(indentLevel)
    }
    YxmlAstFormat.Indent.repeat(indentLevel) + s"<$tag $kvsFmt $innerFmt>"
  }
}

/** Inner body text
  *
  * @param str representation of text
  */
final class Text(val str: String) extends AnyVal with Inner {
  override def format(indentLevel: Int): String = "  ".repeat(indentLevel) + str
}

/** Companion object to pattern match value class. */
object Text {
  def unapply(arg: Text): Option[String] = Some(arg.str) // scalastyle:ignore
}
