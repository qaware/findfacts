package de.qaware.findfacts.webapp.utils

import de.qaware.findfacts.common.dt.{BaseEt, CodeblockEt, ConstantEt, FactEt, TheoryEt, TypeEt}
import de.qaware.findfacts.core.QueryService.ResultList
import de.qaware.findfacts.core.dt.{ResolvedConstant, ResolvedFact, ResolvedThyEt, ResolvedType, ShortBlock}
import de.qaware.findfacts.core.{FacetQuery, FilterQuery}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import io.circe.generic.auto._

/** This component defines all json mapping implicits. This also encapsulates all automatic derivation. */
class JsonMappings {
  // Encoding
  implicit val constantEtEncoder: Encoder[ConstantEt] = deriveEncoder[ConstantEt]
  implicit val factEtEncoder: Encoder[FactEt] = deriveEncoder[FactEt]
  implicit val typeEtEncoder: Encoder[TypeEt] = deriveEncoder[TypeEt]
  implicit val thyEtEncoder: Encoder[TheoryEt] = Encoder.instance {
    case c: ConstantEt => constantEtEncoder(c)
    case f: FactEt => factEtEncoder(f)
    case t: TypeEt => typeEtEncoder(t)
  }
  implicit val codeblockEtEncoder: Encoder[CodeblockEt] = deriveEncoder[CodeblockEt]
  implicit val baseEtEncoder: Encoder[BaseEt] = Encoder.instance {
    case c: CodeblockEt => codeblockEtEncoder(c)
    case t: TheoryEt => thyEtEncoder(t)
  }
  implicit val resolvedConstantEncoder: Encoder[ResolvedConstant] = deriveEncoder[ResolvedConstant]
  implicit val resolvedFactEncoder: Encoder[ResolvedFact] = deriveEncoder[ResolvedFact]
  implicit val resolvedTypeEncoder: Encoder[ResolvedType] = deriveEncoder[ResolvedType]
  implicit val resolvedEncoder: Encoder[ResolvedThyEt] = Encoder.instance {
    // Remove top union type layer from json
    case c: ResolvedConstant => resolvedConstantEncoder(c)
    case f: ResolvedFact => resolvedFactEncoder(f)
    case t: ResolvedType => resolvedTypeEncoder(t)
  }
  implicit val shortBlockEncoder: Encoder[ShortBlock] = deriveEncoder
  implicit val resultShortlistEncoder: Encoder[ResultList[ShortBlock]] = deriveEncoder

  // Decoding
  implicit val filterQueryDecoder: Decoder[FilterQuery] = deriveDecoder
  implicit val facetQueryDecoder: Decoder[FacetQuery] = deriveDecoder
}
