package de.qaware.findfacts.webapp.utils

import de.qaware.findfacts.common.dt.{BaseEt, CodeblockEt, ConstantEt, FactEt, TheoryEt, TypeEt}
import de.qaware.findfacts.core.QueryService.ResultList
import de.qaware.findfacts.core.dt.{ResolvedConstant, ResolvedFact, ResolvedThyEt, ResolvedType, ShortBlock}
import de.qaware.findfacts.core.{FacetQuery, FilterQuery}
import io.circe.{Decoder, Encoder}
// scalastyle:off
import io.circe.generic.semiauto._
import io.circe.generic.auto._
// scalastyle:on

/** This component defines all json mapping implicits. This also encapsulates all automatic derivation. */
class JsonMappings {
  // scalastyle:off scaladoc

  // Encoding
  private val constantEtEncoder = deriveEncoder[ConstantEt]
  private val factEtEncoder = deriveEncoder[FactEt]
  private val typeEtEncoder = deriveEncoder[TypeEt]
  private val thyEtEncoder: Encoder[TheoryEt] = Encoder.instance {
    case c: ConstantEt => constantEtEncoder(c)
    case f: FactEt => factEtEncoder(f)
    case t: TypeEt => typeEtEncoder(t)
  }
  private val codeblockEtEncoder = deriveEncoder[CodeblockEt]
  implicit val baseEtEncoder: Encoder[BaseEt] = Encoder.instance {
    case c: CodeblockEt => codeblockEtEncoder(c)
    case t: TheoryEt => thyEtEncoder(t)
  }
  private val resolvedConstantEncoder = deriveEncoder[ResolvedConstant]
  private val resolvedFactEncoder = deriveEncoder[ResolvedFact]
  private val resolvedTypeEncoder = deriveEncoder[ResolvedType]
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
