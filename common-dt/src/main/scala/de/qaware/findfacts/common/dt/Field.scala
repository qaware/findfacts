package de.qaware.findfacts.common.dt

import scala.language.implicitConversions

import de.qaware.findfacts.common.utils.FromString
import io.circe.{Decoder, Encoder, Json}
import shapeless.tag.{@@, Tagged}

/** Thin technical layer for fields. */
trait Field {

  /** Type of a single underlying element. */
  type BaseType

  /** Type of the whole field, tagged with the field type. */
  type FieldType <: Tagged[this.type]

  /** Wrapper for all implicit of basetype for fields (that have to be supplied at construction time!).
    *
    * @param jsonDecoder [[Decoder]] implicit for base type
    * @param jsonEncoder [[Encoder]] implicit for base type
    *
    * @tparam A base type. Needs to be a parameter since base type gets restricted in subclasses
    */
  protected case class FieldImplicits[A]()(implicit val jsonDecoder: Decoder[A], val jsonEncoder: Encoder[A])

  /** Member for implicit wrapper. Override with new instance of FindImplicits. */
  implicit def implicits: FieldImplicits[BaseType]

  /** Decode base element from json string.
    *
    * @param str to decode
    * @return decoded base element
    */
  def fromJsonString(str: String): BaseType = {
    implicits.jsonDecoder
      .decodeJson(Json.fromString(str))
      .getOrElse(throw new IllegalArgumentException(s"Could not decode $str"))
  }

  /** Decoder for field values. */
  implicit lazy val valueDecoder: Decoder[FieldType] = implicitly[Decoder[FieldType]]

  /** Encoder for field values. */
  implicit def valueEncoder: Encoder[FieldType]
}

/** Single-valued fields. */
trait SingleValuedField[A] extends Field {
  type BaseType = A
  type FieldType = BaseType @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] = {
    implicitly[Encoder[BaseType]](implicits.jsonEncoder).contramap(apply)
  }

  implicit def apply(elem: BaseType): FieldType = elem.asInstanceOf[FieldType]
}

/** Field that is not necessarily present. */
trait OptionalField[A] extends Field {
  type BaseType = A
  type FieldType = Option[BaseType] @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] = {
    implicit val enc: Encoder[BaseType] = implicits.jsonEncoder
    implicitly[Encoder[Option[BaseType]]].contramap(apply)
  }

  implicit def apply(elem: Option[BaseType]): FieldType = elem.asInstanceOf[FieldType]
}

/** Multi-valued fields. Represented as list of base types. */
trait MultiValuedField[A] extends Field {
  type BaseType = A
  type FieldType = List[BaseType] @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] = {
    implicit val enc: Encoder[BaseType] = implicits.jsonEncoder
    implicitly[Encoder[List[BaseType]]].contramap(apply)
  }

  implicit def apply(elem: List[BaseType]): FieldType = elem.asInstanceOf[FieldType]
}

/** Children fields. */
trait ChildrenField[A] extends Field {
  type BaseType = A
  type FieldType = List[BaseType] @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] = {
    implicit val enc: Encoder[BaseType] = implicits.jsonEncoder
    implicitly[Encoder[List[BaseType]]].contramap(apply)
  }

  implicit def apply(elem: List[BaseType]): FieldType = elem.asInstanceOf[FieldType]
}
