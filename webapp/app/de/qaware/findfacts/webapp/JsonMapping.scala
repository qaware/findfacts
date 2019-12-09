package de.qaware.findfacts.webapp

import scala.util.Try

import de.qaware.findfacts.common.dt.{BaseEt, ConstantEt, DocumentationEt, EtField, EtFields, FactEt, TheoryEt, TypeEt}
import de.qaware.findfacts.common.utils.FromString
import de.qaware.findfacts.core.{AbstractFQ, AllInResult, AnyInResult, FacetQuery, Filter, FilterComplement, FilterIntersection, FilterQuery, FilterTerm, FilterUnion, Id, InRange, Number, Query, StringExpression}
import play.api.libs.json.{JsError, JsResult, JsSuccess, JsValue, Json, OWrites, Reads, Writes}
import de.qaware.findfacts.common.utils.TryUtils._
import EtFields.fromString

object JsonMapping {
  // scalastyle:off
  implicit val etFieldReads: Reads[EtField] = (json: JsValue) => {
    val fieldName = json.as[String]
    EtFields.fromString(fieldName).map(JsSuccess(_)).getOrElse(JsError(s"Not a valid field: $fieldName"))
  }
  implicit val stringExpressionReads: Reads[StringExpression] = Json.reads[StringExpression]
  implicit val numberReads: Reads[Number] = Json.reads[Number]
  implicit val idReads: Reads[Id] = Json.reads[Id]
  implicit val inRangeReads: Reads[InRange] = Json.reads[InRange]


  implicit def mapReads[K, V](implicit kParse: FromString[K], untypedReads: Reads[Map[String, V]]): Reads[Map[K, V]] =
  (json: JsValue) => {
      val map: Try[Seq[(K, V)]] = json.as[Map[String, V]].toSeq map {
        case (kStr, v) => kParse(kStr).map(_ -> v)
      }
      map.map(m => JsSuccess(m.toMap)).getOrElse(JsError(s"Could not parse keys $json"))
    }
  implicit val abstractFQReads: Reads[AbstractFQ] = Json.reads[AbstractFQ]
  implicit val anyInResultReads: Reads[AnyInResult] = Json.reads[AnyInResult]
  implicit val allInResultReads: Reads[AllInResult] = Json.reads[AllInResult]
  implicit val filterTermReads: Reads[FilterTerm] = Json.reads[FilterTerm]
  implicit val filterMapReads: Reads[Map[EtField, FilterTerm]] = mapReads[EtField, FilterTerm]
  implicit val filterReads: Reads[Filter] = Json.reads[Filter]
  implicit val filterComplementReads: Reads[FilterComplement] = Json.reads[FilterComplement]
  implicit val filterUnionReads: Reads[FilterUnion] = Json.reads[FilterUnion]
  implicit val filterIntersectionReads: Reads[FilterIntersection] = Json.reads[FilterIntersection]
  implicit val facetQueryReads: Reads[FacetQuery] = Json.reads[FacetQuery]
  implicit val filterQueryReads: Reads[FilterQuery] = Json.reads[FilterQuery]
  implicit val queryReads: Reads[Query] = Json.reads[Query]

  implicit val theoryWrites: Writes[TheoryEt] = Json.writes[TheoryEt]
  implicit val constWrites: Writes[ConstantEt] = Json.writes[ConstantEt]
  implicit val documentationWrites: Writes[DocumentationEt] = Json.writes[DocumentationEt]
  implicit val factWrites: Writes[FactEt] = Json.writes[FactEt]
  implicit val typeWrites: Writes[TypeEt] = Json.writes[TypeEt]
  implicit val entityWrites: Writes[BaseEt] = Json.writes[BaseEt]
  // scalastyle:on
}
