package de.qaware.findfacts.webapp.utils

import de.qaware.findfacts.common.dt.BaseEt
import de.qaware.findfacts.core.QueryService.ResultList
import de.qaware.findfacts.core.dt.{
  ResolvedConstant,
  ResolvedFact,
  ResolvedThyEt,
  ResolvedType,
  ShortBlock,
  ShortCmd,
  ShortDocumentation
}
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
  implicit val baseEtEncoder: Encoder[BaseEt] = deriveEncoder
  private val resolvedConstantEncoder = deriveEncoder[ResolvedConstant]
  private val resolvedFactEncoder = deriveEncoder[ResolvedFact]
  private val resolvedTypeEncoder = deriveEncoder[ResolvedType]
  implicit val resolvedEncoder: Encoder[ResolvedThyEt] = Encoder.instance {
    // Remove top union type layer from json
    case c: ResolvedConstant => resolvedConstantEncoder(c)
    case f: ResolvedFact => resolvedFactEncoder(f)
    case t: ResolvedType => resolvedTypeEncoder(t)
  }
  implicit val shortCmdEtEncoder: Encoder[ShortCmd] = Encoder.instance {
    case e: ShortBlock =>
      Encoder.forProduct4("id", "theory", "src", "entities") { e: ShortBlock =>
        (e.id, e.theory, e.src, e.entities)
      } apply e
    case e: ShortDocumentation =>
      Encoder.forProduct4("id", "theory", "src", "docKind") { e: ShortDocumentation =>
        (e.id, e.theory, e.src, e.docKind)
      } apply e
  }
  implicit val resultShortlistEncoder: Encoder[ResultList[ShortCmd]] = deriveEncoder

  // Decoding
  implicit val filterQueryDecoder: Decoder[FilterQuery] = deriveDecoder
  implicit val facetQueryDecoder: Decoder[FacetQuery] = deriveDecoder
}
