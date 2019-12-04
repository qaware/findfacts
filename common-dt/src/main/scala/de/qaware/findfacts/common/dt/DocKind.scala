package de.qaware.findfacts.common.dt

/** Types of documentation. */
object DocKind extends Enumeration {

  /** Comments in the meta-language, (* ... *) */
  final val Meta = Value("Meta")

  /** Latex documentation: sections, etc. */
  final val Latex = Value("Latex")

  /** Inline comments, usually in cartouches. */
  final val Inline = Value("Inline")
}
