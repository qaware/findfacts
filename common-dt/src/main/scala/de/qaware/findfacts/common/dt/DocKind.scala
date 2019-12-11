package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.utils.FromString
import io.circe.{Decoder, Encoder}

import scala.util.Try

/** Types of documentation. */
object DocKind extends Enumeration {

  /** Comments in the meta-language, (* ... *) */
  final val Meta = Value("Meta")

  /** Latex documentation: sections, etc. */
  final val Latex = Value("Latex")

  /** Inline comments, usually in cartouches. */
  final val Inline = Value("Inline")

  /** [[FromString]] implicit for this enum. */
  implicit val fromString: FromString[this.Value] = FromString.instance { s =>
    Try(this.values.find(_.toString == s).getOrElse(throw new IllegalArgumentException(s"No such enum value: $s")))
  }

  /** Json encoding for this enums variants. */
  implicit val jsonEncoder: Encoder[this.Value] = Encoder.encodeString.contramap[this.Value](_.toString)

  /** Json decoding for this enums variants. */
  implicit val jsonDecoder: Decoder[this.Value] = Decoder.decodeString.emapTry(fromString.apply)
}
