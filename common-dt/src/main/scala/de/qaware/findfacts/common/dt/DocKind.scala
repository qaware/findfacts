package de.qaware.findfacts.common.dt

import scala.util.Try

import de.qaware.findfacts.common.utils.FromString

/** Types of documentation. */
object DocKind extends Enumeration {

  /** Comments in the meta-language, (* ... *) */
  final val Meta = Value("Meta")

  /** Latex documentation: sections, etc. */
  final val Latex = Value("Latex")

  /** Inline comments, usually in cartouches. */
  final val Inline = Value("Inline")

  /** [[FromString]] for this enum.
    *
    * @return new [[FromString]]
    */
  implicit def fromString: FromString[this.Value] = FromString.instance { s =>
    Try(this.values.find(_.toString == s).getOrElse(throw new IllegalArgumentException(s"No such enum value: $s")))
  }
}
