package de.qaware.findfacts.common.dt

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

  def fromString(s: String): EtKind.Value = {
    EtKind.values.find(_.toString == s).getOrElse(throw new IllegalArgumentException(s"Enum does not contain $s"))
  }
}
