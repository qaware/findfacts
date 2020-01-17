package de.qaware.findfacts.common.utils

import scala.util.Try

import enumeratum.{Enum, EnumEntry}
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

/** Enum with default string/json conversions.
  *
  * @tparam E type of the entry, should be a sealed trait
  */
trait DefaultEnum[E <: EnumEntry] extends Enum[E] {

  /** Make type visible under common name. */
  type Value = E

  /** Implicit to get objects from string. */
  implicit val fromString: FromString[Value] = FromString.instance { s =>
    Try(this.values.find(_.toString == s).getOrElse(throw new IllegalArgumentException(s"No such enum value: $s")))
  }

  /** Implicit for json key decoding. */
  implicit val keyDecoder: KeyDecoder[Value] = KeyDecoder.instance(fromString(_).toOption)

  /** Implicit for json key encoding. */
  implicit val keyEncoder: KeyEncoder[Value] = KeyEncoder.encodeKeyString.contramap(_.toString)

  /** Implicit for json decoding. */
  implicit val decoder: Decoder[Value] = Decoder.decodeString.emapTry(fromString.apply)

  /** Implicit for json encoding. */
  implicit val encoder: Encoder[Value] = Encoder.encodeString.contramap(_.toString)
}
