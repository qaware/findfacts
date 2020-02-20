package de.qaware.findfacts.core.solrimpl

import java.util.regex.Matcher

import scala.collection.mutable
import scala.util.{Success, Try}

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.common.dt.EtField.{Children, Id}
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.core.QueryService.ResultList
import de.qaware.findfacts.core.{AbstractFQ, AllInResult, AnyInResult, Exact, FilterQuery, FilterTerm, InRange, Term}

/** Maps filter terms to solr query strings. */
class SolrFilterTermMapper {
  final val MaxChildren = 1000

  /** Children for id-only blocks. */
  case object IdChildren extends Children[Id.T] {
    override implicit val implicits: FieldImplicits[Id.T] = FieldImplicits()
  }

  /** Characters that need to be escaped. Special characters that may be used: * ? */
  final val SpecialCharacters =
    Set("\\+", "-", "&&", "\\|\\|", "!", "\\(", "\\)", "\\{", "\\}", "\\[", "\\]", "\\^", "\"", "~", ":", "\\\\", "\\/")

  /** Regex built from special characters */
  final private val EscapeRegex = SpecialCharacters.map(s => s"($s)").mkString("|").r

  private def innerQuery(fq: AbstractFQ, toQuery: ResultList[IdChildren.T] => String)(
      implicit queryService: SolrQueryService): Try[String] = {
    queryService.getResultList[IdChildren.T](FilterQuery(fq, MaxChildren))(FromSolrDoc[IdChildren.T]).map(toQuery)
  }

  private def allInResult(res: ResultList[IdChildren.T]): String = res match {
    case ResultList(Vector(), _, _) => SolrQueryLiterals.All
    case ResultList(values, _, _) => s"(${values.mkString(s" ${SolrQueryLiterals.And} ")})"
  }

  private def anyInResult(res: ResultList[IdChildren.T]): String = res match {
    case ResultList(Vector(), _, _) => s"(${SolrQueryLiterals.Not}${SolrQueryLiterals.All})"
    case ResultList(values, _, _) => s"(${values.mkString(" ")})"
  }

  /** Escapes a value string.
    *
    * @param value to escape
    * @param exact whether the value is used for exact mathing
    * @return escaped string, to be used in solr query
    */
  def escape(value: String, exact: Boolean): String = {
    val escaped = EscapeRegex.replaceAllIn(value, m => Matcher.quoteReplacement(s"\\$m"))
    if (exact) {
      "\"" + escaped + "\""
    } else if (escaped == "") {
      "\"\""
    } else {
      s"($escaped)"
    }
  }

  /** Builds a filter string for a field.
    *
    * @param field to filter on
    * @param term to filter for
    * @param queryService for recursive calls
    * @return query string or exception if rescursive call failed
    */
  def mapFilterTerm(field: EtField, term: FilterTerm)(
      implicit queryService: SolrQueryService,
      qParams: mutable.Map[String, Seq[String]]): Try[String] = {
    (term match {
      case Term(inner) => Success(escape(inner, exact = false))
      case Exact(inner) => Success(escape(inner, exact = true))
      case InRange(from, to) => Success(s"[$from TO $to]")
      case AnyInResult(fq) => innerQuery(fq, anyInResult)
      case AllInResult(fq) => innerQuery(fq, allInResult)
    }) map { filter =>
      val name = s"fq${qParams.size}"
      qParams.put(name, Seq(s"filter(${field.name}:$filter)"))
      "({!v=$" + s"$name})"
    }
  }
}
