package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.utils.FromString
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import shapeless.tag.{@@, Tagged}

/** Thin technical layer for fields. */
trait Field {

  /** Type of a single underlying element. */
  type BaseType

  /** Type of the whole field, tagged with the field type. */
  type FieldType <: Tagged[this.type]

  /** Wrapper for all implicit of basetype for fields (that have to be supplied at construction time!).
    *
    * @param fromString [[FromString]] implicit for base type
    * @param jsonDecoder [[Decoder]] implicit for base type
    * @param jsonKeyDecoder [[KeyDecoder]] implicit for base type used as keys
    * @param jsonEncoder [[Encoder]] implicit for base type
    * @param jsonKeyEncoder [[KeyEncoder]] implicit for base type used as keys
    *
    * @tparam A base type. Needs to be a parameter since base type gets restricted in subclasses
    */
  protected case class FieldImplicits[A]()(
      implicit
      val fromString: FromString[A],
      val jsonDecoder: Decoder[A],
      val jsonKeyDecoder: KeyDecoder[A],
      val jsonEncoder: Encoder[A],
      val jsonKeyEncoder: KeyEncoder[A]
  )

  /** Member for implicit wrapper. Override with new instance of FindImplicits. */
  implicit def implicits: FieldImplicits[BaseType]

  /** Decoder for field values. */
  implicit def valueDecoder: Decoder[FieldType] = implicitly[Decoder[FieldType]]

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

/** Field that is not necessarily present. */
trait OptionalField[A] extends Field {
  type BaseType = A
  type FieldType = Option[BaseType] @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] =
    Encoder.encodeOption(implicits.jsonEncoder).contramap(_.asInstanceOf[Option[BaseType]])
}

/** Multi-valued fields. Represented as list of base types. */
trait MultiValuedField[A] extends Field {
  type BaseType = A
  type FieldType = List[BaseType] @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] =
    Encoder.encodeList(implicits.jsonEncoder).contramap(_.asInstanceOf[List[BaseType]])
}
