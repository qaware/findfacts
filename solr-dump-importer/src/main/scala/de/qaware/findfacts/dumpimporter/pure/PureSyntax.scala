package de.qaware.findfacts.dumpimporter.pure

import enumeratum.{Enum, EnumEntry}

sealed abstract class SyntaxElement(val name: String) extends EnumEntry

/** Elements of the isabelle pure language  */
case object PureSyntax extends Enum[SyntaxElement] {
  /** Set of all values. */
  final val values = findValues

  // scalastye:off scaladoc Justification: Explained in isabelle_implementation manual
  /** Pure keywords */
  case object Prop extends SyntaxElement("prop")
  case object Fun extends SyntaxElement("fun")
  case object Itself extends SyntaxElement("itself")

  /** Pure connectives. */
  case object Eq extends SyntaxElement("Pure.eq")
  case object Imp extends SyntaxElement("Pure.imp")
  case object All extends SyntaxElement("Pure.all")

  /** Auxiliary connectives */
  case object Conj extends SyntaxElement("Pure.conjunction")
  case object Term extends SyntaxElement("Pure.term")
  case object Type extends SyntaxElement("Pure.type")
  // scalastyle:on scaladoc
}
