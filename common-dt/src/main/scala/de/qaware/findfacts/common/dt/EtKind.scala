package de.qaware.findfacts.common.dt

import scala.util.Try

import de.qaware.findfacts.common.utils.FromString
import enumeratum.{Enum, EnumEntry}
import io.circe.{Decoder, Encoder}

/** Union type for entity kinds. */
sealed trait EtKind extends EnumEntry

/** Kinds of entities. */
object EtKind extends Enum[EtKind] {

  /** Set of all values. */
  final val values = findValues

  /** Type definition. */
  case object Type extends EtKind

  /** Constants, including constructors. */
  case object Constant extends EtKind

  /** Some propositions. */
  case object Fact extends EtKind

  /** Comments, sections, titles etc. */
  case object Documentation extends EtKind

  /** [[FromString]] for this enum. */
  implicit val fromString: FromString[EtKind] = FromString.instance { s =>
    Try(this.values.find(_.toString == s).getOrElse(throw new IllegalArgumentException(s"Enum does not contain $s")))
  }

  /** Json encoding for this enums variants. */
  implicit val jsonEncoder: Encoder[EtKind] = Encoder.encodeString.contramap[EtKind](_.toString)

  /** Json decoding for this enums variants. */
  implicit val jsonDecoder: Decoder[EtKind] = Decoder.decodeString.emapTry(fromString.apply)
}
