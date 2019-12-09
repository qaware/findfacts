package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.utils.FromString
import play.api.libs.json.Writes
import shapeless.tag.{@@, Tagged}

/** Thin technical layer for fields. */
trait Field {
  /** Type of a single underlying element. */
  type BaseType

  /** Total type of field, tagged with the field case object. */
  type FieldType <: Tagged[this.type]

  /** Wrapper for all implicit fields that have to be supplied at construction time.
   *
   * @param fromString [[FromString]] implicit for base type
   * @param toJson [[Writes]] implicit for base type
   * @tparam A base type. Needs to be a parameter since base type gets restricted in subclasses
   */
  protected case class FieldImplicits[A]()(
    implicit val fromString: FromString[A],
    implicit val toJson: Writes[A]
  )

  /** Member for implicit wrapper. */
  val implicits: FieldImplicits[BaseType]
}

/** Single-valued fields. */
trait SingleValuedField extends Field {
  type FieldType = BaseType @@ this.type
}

/** Multi-valued fields. Represented as list of base types. */
trait MultiValuedField extends Field {
  type FieldType = List[BaseType] @@ this.type
}
