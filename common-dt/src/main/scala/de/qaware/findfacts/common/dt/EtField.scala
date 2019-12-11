package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.utils.FromString
import enumeratum.{Enum, EnumEntry}
import io.circe.{KeyDecoder, KeyEncoder}

import scala.collection.immutable
import scala.util.Try

/** Seal field types so only existing fields can be used. */
sealed trait EtField extends Field with EnumEntry

/** Typelevel representation of entity fields. */
object EtField extends Enum[EtField] {

  /** Set of all fields. */
  override val values: immutable.IndexedSeq[EtField] = findValues

  /** Unique id. */
  case object Id extends SingleValuedField with EtField {
    type BaseType = String
    override val implicits = FieldImplicits()
  }

  /** Kind of the entity. Possible values in [[Kind]]. */
  case object Kind extends SingleValuedField with EtField {
    type BaseType = EtKind.Value
    override val implicits = FieldImplicits()
  }

  /** Source theory file containing the entity. */
  case object SourceFile extends SingleValuedField with EtField {
    type BaseType = String
    override val implicits = FieldImplicits()
  }

  /** Start position of entity definition, in absolute isabelle characters. */
  case object StartPosition extends SingleValuedField with EtField {
    type BaseType = Int
    override val implicits = FieldImplicits()
  }

  /** End position of entity definition, in absolute isabelle characters. */
  case object EndPosition extends SingleValuedField with EtField {
    type BaseType = Int
    override val implicits = FieldImplicits()
  }

  /** Long (in most cases fully-qualified) name. */
  case object Name extends SingleValuedField with EtField {
    type BaseType = String
    override val implicits = FieldImplicits()
  }

  /** Proposition, of a constants, fact, or type entity. */
  case object Proposition extends SingleValuedField with EtField {
    type BaseType = String
    override val implicits = FieldImplicits()
  }

  /** Source text in isabelle thy. */
  case object SourceText extends OptionalField with EtField {
    type BaseType = String
    override val implicits = FieldImplicits()
  }

  /** Type of a constant entity. */
  case object ConstantType extends SingleValuedField with EtField {
    type BaseType = String
    override val implicits = FieldImplicits()
  }

  /** Other entities that this entity uses in its propositions. */
  case object PropositionUses extends MultiValuedField with EtField {
    type BaseType = String
    override val implicits = FieldImplicits()
  }

  /** Other entities that this entity uses in its types. */
  case object TypeUses extends MultiValuedField with EtField {
    type BaseType = String
    override val implicits = FieldImplicits()
  }

  /** Other entities that are generated from the same source position as this entity. */
  case object Related extends MultiValuedField with EtField {
    type BaseType = String
    override val implicits = FieldImplicits()
  }

  /** Other entities that this entity uses in its proofs. */
  case object ProofUses extends MultiValuedField with EtField {
    type BaseType = String
    override val implicits = FieldImplicits()
  }

  /** Kind of a documentation entity. Possible values in [[DocumentationKind]]. */
  case object DocumentationKind extends SingleValuedField with EtField {
    type BaseType = DocKind.Value
    override val implicits = FieldImplicits()
  }

  /** Implicit to get objects from string. */
  implicit val fromString: FromString[EtField] = FromString.instance { s =>
    Try(this.values.find(_.toString == s).getOrElse(throw new IllegalArgumentException(s"No such enum value: $s")))
  }

  /** Implicit for json decoding. */
  implicit val keyDecoder: KeyDecoder[EtField] = KeyDecoder.instance(fromString(_).toOption)

  /** Implicit for json encoding. */
  implicit val keyEncoder: KeyEncoder[EtField] = KeyEncoder.instance(_.toString)
}
