package de.qaware.findfacts.common.dt

import scala.util.Try

import de.qaware.findfacts.common.utils.FromString

/** Kinds of entities. */
object EtKind extends Enumeration {

  /** Type definition. */
  final val Type = Value("Type")

  /** Constants, including constructors. */
  final val Constant = Value("Constant")

  /** Some propositions. */
  final val Fact = Value("Fact")

  /** Comments, sections, titles etc. */
  final val Documentation = Value("Documentation")

  /** [[FromString]] for this enum.
    *
    * @return new [[FromString]]
    */
  implicit def fromString: FromString[this.Value] = FromString.instance { s =>
    Try(EtKind.values.find(_.toString == s).getOrElse(throw new IllegalArgumentException(s"Enum does not contain $s")))
  }
}
