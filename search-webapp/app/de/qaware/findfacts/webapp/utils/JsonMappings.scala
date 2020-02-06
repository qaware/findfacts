package de.qaware.findfacts.webapp.utils

import de.qaware.findfacts.common.dt.{CmdKind, CodeblockEt, EtField, ShortCmdEt, ShortThyEt}
import de.qaware.findfacts.core.{FacetQuery, FilterQuery}
import io.circe.{Decoder, Encoder}
// scalastyle:off
import io.circe.generic.auto._
import io.circe.generic.semiauto._
// scalastyle:on

/** This component defines all json mapping implicits. This also encapsulates all automatic derivation. */
class JsonMappings {
  // scalastyle:off scaladoc

  // Encoding
  implicit val codeblockEtEncoder: Encoder[CodeblockEt] = deriveEncoder[CodeblockEt]

  // Custom encodings: encode trait values and not union type
  implicit val shortThyEncoder: Encoder[ShortThyEt] =
    Encoder.forProduct5("id", "kind", "name", "proposition", "description") { thyEt =>
      (thyEt.id, thyEt.kind, thyEt.name, thyEt.proposition, thyEt.shortDescription)
    }

  implicit val shortListEncoder: Encoder[List[ShortThyEt]] = Encoder.encodeList(shortThyEncoder)

  implicit val shortEncoder: Encoder[ShortCmdEt] = Encoder.forProduct5("id", "kind", "file", "src", "entities") {
    et: ShortCmdEt =>
      (et.id, et.kind, et.file, et.src, et.entities.asInstanceOf[List[ShortThyEt]])
  }(
    Encoder[EtField.Id.FieldType],
    Encoder[CmdKind],
    Encoder[EtField.SourceTheory.FieldType],
    Encoder[EtField.SourceText.FieldType],
    shortListEncoder)

  implicit val resultListEncoder: Encoder[List[ShortCmdEt]] = Encoder.encodeList(shortEncoder)

  // Decoding
  implicit val filterQueryDecoder: Decoder[FilterQuery] = deriveDecoder[FilterQuery]
  implicit val facetQueryDecoder: Decoder[FacetQuery] = deriveDecoder[FacetQuery]
}
