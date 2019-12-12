package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.utils.FromString
import io.circe.{Decoder, Encoder, KeyEncoder}
import shapeless.tag.{@@, Tagged}
import sun.reflect.generics.tree.BaseType

/** Thin technical layer for fields. */
trait Field {

  /** Type of a single underlying element. */
  type BaseType

  /** Type of the whole field, tagged with the field type. */
  type FieldType <: Tagged[this.type]

  /** Wrapper for all implicit fields that have to be supplied at construction time.
    *
    * @param fromString [[FromString]] implicit for base type
    * @param jsonDecoder [[Decoder]] implicit for base type
    * @param jsonEncoder [[Encoder]] implicit for base type
    * @tparam A base type. Needs to be a parameter since base type gets restricted in subclasses
    */
  protected case class FieldImplicits[A]()(
      implicit
      val fromString: FromString[A],
      val jsonDecoder: Decoder[A],
      val jsonEncoder: Encoder[A]
  )

  /** Member for implicit wrapper. Override with new instance of FindImplicits. */
  implicit def implicits: FieldImplicits[BaseType]

  /** Encoder for keys of base type. */
  implicit lazy val baseKeyEncoder: KeyEncoder[BaseType] = KeyEncoder.instance(v => implicits.jsonEncoder(v).toString())

  /** Encoder for field values. */
  implicit def valueEncoder: Encoder[FieldType]
}

/** Single-valued fields. */
trait SingleValuedField[A] extends Field {
  type BaseType = A
  type FieldType = BaseType @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] =
    implicits.jsonEncoder.contramap(_.asInstanceOf[BaseType])
}

/** Field that is not necessarily present */
trait OptionalField[A] extends Field {
  type BaseType = A
  type FieldType = Option[BaseType] @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] = {
    Encoder.encodeOption(implicits.jsonEncoder).contramap(_.asInstanceOf[Option[BaseType]])
  }
}

/** Multi-valued fields. Represented as list of base types. */
trait MultiValuedField[A] extends Field {
  type BaseType = A
  type FieldType = List[BaseType] @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] = {
    Encoder.encodeList(implicits.jsonEncoder).contramap(_.asInstanceOf[List[BaseType]])
  }
}
