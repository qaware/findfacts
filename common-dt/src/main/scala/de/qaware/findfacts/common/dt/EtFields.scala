package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.utils.FromString
import shapeless.tag.{@@, Tagged}

/** Base entity field. */
sealed trait EtField {

  /** Type of a single underlying element. */
  type BaseType

  /** Total type of field, tagged with the field object. */
  type FieldType <: Tagged[this.type]

  /** Wrapper for all implicit fields that have to be supplied at construction time.
    *
    * @param fromString [[FromString]] implicit for base type
    * @tparam A base type. Needs to be a parameter since base type gets restricted in subclasses
    */
  protected case class Find[A]()(implicit val fromString: FromString[A])

  /** Member for implicit wrapper. */
  protected val implicits: Find[BaseType]

  /** Public [[FromString]] for construction of underlying base from string. */
  def fromString: FromString[BaseType] = implicits.fromString
}

/** Single-valued fields.
  *
  * @tparam A base type as outer param
  */
sealed trait SingleValuedEtField[A] extends EtField {
  override type BaseType = A
  override type FieldType = BaseType @@ this.type
}

/** Multi-valued fields. Represented as list of base types.
  *
  * @tparam A base type as outer param
  */
sealed trait MultiValuedEtField[A] extends EtField {
  override type BaseType = A
  override type FieldType = List[BaseType] @@ this.type
}

/** Typelevel representation of entity fields. */
object EtFields {

  /** Unique id. */
  object Id extends SingleValuedEtField[String] { override val implicits = Find() }

  /** Kind of the entity. Possible values in [[Kind]]. */
  object Kind extends SingleValuedEtField[EtKind.Value] { override val implicits = Find() }

  /** Source theory file containing the entity. */
  object SourceFile extends SingleValuedEtField[String] { override val implicits = Find() }

  /** Start position of entity definition, in absolute isabelle characters. */
  object StartPosition extends SingleValuedEtField[Int] { override val implicits = Find() }

  /** End position of entity definition, in absolute isabelle characters. */
  object EndPosition extends SingleValuedEtField[Int] { override val implicits = Find() }

  /** Long (in most cases fully-qualified) name. */
  object Name extends SingleValuedEtField[String] { override val implicits = Find() }

  /** Proposition, of a constants, fact, or type entity. */
  object Proposition extends SingleValuedEtField[String] { override val implicits = Find() }

  /** Source text in isabelle thy. */
  object SourceText extends SingleValuedEtField[String] { override val implicits = Find() }

  /** Type of a constant entity. */
  object ConstantType extends SingleValuedEtField[String] { override val implicits = Find() }

  /** Other entities that this entity uses in its propositions. */
  object PropositionUses extends MultiValuedEtField[Id.BaseType] { override val implicits = Find() }

  /** Other entities that this entity uses in its types. */
  object TypeUses extends MultiValuedEtField[Id.BaseType] { override val implicits = Find() }

  /** Other entities that are generated from the same source position as this entity. */
  object Related extends MultiValuedEtField[Id.BaseType] { override val implicits = Find() }

  /** Other entities that this entity uses in its proofs. */
  object ProofUses extends MultiValuedEtField[Id.BaseType] { override val implicits = Find() }

  /** Kind of a documentation entity. Possible values in [[DocumentationKind]]. */
  object DocumentationKind extends SingleValuedEtField[DocKind.Value] { override val implicits = Find() }
}
