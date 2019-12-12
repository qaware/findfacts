package de.qaware.findfacts.common.dt

import enumeratum.EnumEntry

/** Seal field types so only existing fields can be used. */
sealed trait EtField extends Field with EnumEntry

/** Typelevel representation of entity fields. */
object EtField extends DefaultEnum[EtField] {
  override final val values = findValues

  /** Unique id. */
  case object Id extends SingleValuedField[String] with Value { override val implicits = FieldImplicits() }

  /** Kind of the entity. Possible values in [[Kind]]. */
  case object Kind extends SingleValuedField[EtKind] with EtField { override val implicits = FieldImplicits() }

  /** Source theory file containing the entity. */
  case object SourceFile extends SingleValuedField[String] with EtField { override val implicits = FieldImplicits() }

  /** Start position of entity definition, in absolute isabelle characters. */
  case object StartPosition extends SingleValuedField[Int] with EtField { override val implicits = FieldImplicits() }

  /** End position of entity definition, in absolute isabelle characters. */
  case object EndPosition extends SingleValuedField[Int] with EtField { override val implicits = FieldImplicits() }

  /** Long (in most cases fully-qualified) name. */
  case object Name extends SingleValuedField[String] with EtField { override val implicits = FieldImplicits() }

  /** Proposition, of a constants, fact, or type entity. */
  case object Proposition extends SingleValuedField[String] with EtField { override val implicits = FieldImplicits() }

  /** Source text in isabelle thy. */
  case object SourceText extends OptionalField[String] with EtField { override val implicits = FieldImplicits() }

  /** Type of a constant entity. */
  case object ConstantType extends SingleValuedField[String] with EtField { override val implicits = FieldImplicits() }

  /** Other entities that this entity uses in its propositions. */
  case object PropositionUses extends MultiValuedField[String] with EtField {
    override val implicits = FieldImplicits()
  }

  /** Other entities that this entity uses in its types. */
  case object TypeUses extends MultiValuedField[String] with EtField { override val implicits = FieldImplicits() }

  /** Other entities that are generated from the same source position as this entity. */
  case object Related extends MultiValuedField[String] with EtField { override val implicits = FieldImplicits() }

  /** Other entities that this entity uses in its proofs. */
  case object ProofUses extends MultiValuedField[String] with EtField { override val implicits = FieldImplicits() }

  /** Kind of a documentation entity. Possible values in [[DocumentationKind]]. */
  case object DocumentationKind extends SingleValuedField[DocKind] with EtField {
    override val implicits = FieldImplicits()
  }
}
