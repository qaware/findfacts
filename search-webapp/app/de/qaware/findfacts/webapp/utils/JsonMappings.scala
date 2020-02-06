package de.qaware.findfacts.webapp.utils

import de.qaware.findfacts.common.dt.{BaseEt, ShortCmdEt, ShortThyEt}
import de.qaware.findfacts.core.{FacetQuery, FilterQuery, ResultShortlist}
import io.circe.{Decoder, Encoder}
// scalastyle:off
import io.circe.generic.semiauto._
// scalastyle:on

/** This component defines all json mapping implicits. This also encapsulates all automatic derivation. */
class JsonMappings {
  // scalastyle:off scaladoc

  // Encoding
  implicit val baseEtEncoder: Encoder[BaseEt] = deriveEncoder[BaseEt]

  // Custom encodings: encode trait values and not full union types
  implicit val shortThyEncoder: Encoder[ShortThyEt] =
    Encoder.forProduct4("id", "kind", "name", "description") { thyEt =>
      (thyEt.id, thyEt.kind, thyEt.name, thyEt.shortDescription)
    }
  implicit val shortEncoder: Encoder[ShortCmdEt] = Encoder.forProduct5("id", "kind", "file", "src", "entities") {
    et: ShortCmdEt =>
      (et.id, et.kind, et.file, et.src, et.entities.asInstanceOf[List[ShortThyEt]])
  }
  implicit val resultShortlistEncoder: Encoder[ResultShortlist] = deriveEncoder[ResultShortlist]

  // Decoding
  implicit val filterQueryDecoder: Decoder[FilterQuery] = {
    import io.circe.generic.auto._
    deriveDecoder[FilterQuery]
  }
  implicit val facetQueryDecoder: Decoder[FacetQuery] = {
    import io.circe.generic.auto._
    deriveDecoder[FacetQuery]
  }
}
