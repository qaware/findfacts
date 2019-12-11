package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.utils.FromString
import io.circe.{Decoder, Encoder}

import scala.util.Try

/** Kinds of entities. */
object EtKind extends Enumeration {

  /** Type definition. */
  final val Type = Value("Type")

  /** Constants, including constructors. */
  final val Constant = Value("Constant")

  /** Some propositions. */
  final val Fact = Value("Fact")

  /** Comments, sections, titles etc. */
  final val Documentation = Value("Documentation")

  /** [[FromString]] for this enum. */
  implicit val fromString: FromString[this.Value] = FromString.instance { s =>
    Try(this.values.find(_.toString == s).getOrElse(throw new IllegalArgumentException(s"Enum does not contain $s")))
  }

  /** Json encoding for this enums variants. */
  implicit val jsonEncoder: Encoder[this.Value] = Encoder.encodeString.contramap[this.Value](_.toString)

  /** Json decoding for this enums variants. */
  implicit val jsonDecoder: Decoder[this.Value] = Decoder.decodeString.emapTry(fromString.apply)
}
