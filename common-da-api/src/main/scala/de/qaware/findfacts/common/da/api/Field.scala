package de.qaware.findfacts.common.da.api

import scala.language.implicitConversions

import io.circe.{Decoder, Encoder, Json}
import shapeless.tag.{@@, Tagged}

/** Thin technical layer for fields. */
trait Field {

  /** Name of the field. */
  val name: String

  /** Type of a single underlying element. */
  type BaseType

  /** Type of the whole field, tagged with the field type. */
  type FieldType <: Tagged[this.type]

  /** Wrapper for all implicit of basetype for fields (that have to be supplied at construction time!).
    *
    * @param jsonDecoder [[Decoder]] implicit for base type
    * @param jsonEncoder [[Encoder]] implicit for base type
    * @tparam A base type. Needs to be a parameter since base type gets restricted in subclasses
    */
  protected final case class FieldImplicits[A]()(implicit val jsonDecoder: Decoder[A], val jsonEncoder: Encoder[A])

  /** Member for implicit wrapper. Override with new instance of FindImplicits.
    *
    * @return implicits that are guranteed to defined for the field
    */
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

  /** Encoder for field values.
    *
    * @return value encoder
    */
  implicit def valueEncoder: Encoder[FieldType]
}

/** Single-valued fields.
  *
  * @tparam A type of the field
  */
trait SingleValuedField[A] extends Field {
  override type BaseType = A
  override type FieldType = BaseType @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] = {
    implicitly[Encoder[BaseType]](implicits.jsonEncoder).contramap(apply)
  }

  /** Builds field from base value.
    *
    * @param elem base value
    * @return tagged field
    */
  @SuppressWarnings(Array("AsInstanceOf"))
  implicit def apply(elem: BaseType): FieldType = elem.asInstanceOf[FieldType]
}

/** Field that is not necessarily present.
  *
  * @tparam A type of the underlying value
  */
trait OptionalField[A] extends Field {
  override type BaseType = A
  override type FieldType = Option[BaseType] @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] = {
    implicit val enc: Encoder[BaseType] = implicits.jsonEncoder
    implicitly[Encoder[Option[BaseType]]].contramap(apply)
  }

  /** Builds field from base value.
    *
    * @param elem base value
    * @return tagged field
    */
  @SuppressWarnings(Array("AsInstanceOf"))
  implicit def apply(elem: Option[BaseType]): FieldType = elem.asInstanceOf[FieldType]
}

/** Multi-valued fields. Represented as list of base types.
  *
  * @tparam A base type
  */
trait MultiValuedField[A] extends Field {
  override type BaseType = A
  override type FieldType = List[BaseType] @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] = {
    implicit val enc: Encoder[BaseType] = implicits.jsonEncoder
    implicitly[Encoder[List[BaseType]]].contramap(apply)
  }

  /** Builds field from base value.
    *
    * @param elem base value
    * @return tagged field
    */
  @SuppressWarnings(Array("AsInstanceOf"))
  implicit def apply(elem: List[BaseType]): FieldType = elem.asInstanceOf[FieldType]
}

/** Children fields.
  *
  * @tparam A Children type
  */
trait ChildrenField[A] extends Field {
  override type BaseType = A
  override type FieldType = List[BaseType] @@ this.type

  override implicit lazy val valueEncoder: Encoder[FieldType] = {
    implicit val enc: Encoder[BaseType] = implicits.jsonEncoder
    implicitly[Encoder[List[BaseType]]].contramap(apply)
  }

  /** Builds field from base value.
    *
    * @param elem base value
    * @return tagged field
    */
  @SuppressWarnings(Array("AsInstanceOf"))
  implicit def apply(elem: List[BaseType]): FieldType = elem.asInstanceOf[FieldType]
}
