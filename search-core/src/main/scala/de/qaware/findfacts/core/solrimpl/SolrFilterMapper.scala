package de.qaware.findfacts.core.solrimpl

import java.util.regex.Matcher

import scala.util.{Success, Try}

import de.qaware.findfacts.common.dt.EtField.{Children, Id}
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.common.utils.TryUtils._
import de.qaware.findfacts.core.QueryService.ResultList
import de.qaware.findfacts.core.solrimpl.SolrQueryLiterals.All
import de.qaware.findfacts.core.{And, Exact, FieldFilter, Filter, FilterQuery, InRange, InResult, Not, Or, Term}

/** Children for id-only blocks. */
case object IdChildren extends Children[Id.T] {
  override implicit val implicits: FieldImplicits[Id.T] = FieldImplicits()
}

/** Mapper to map filters to solr query strings. */
class SolrFilterMapper {

  /** Maximum number of children for recursive queries. */
  final val MaxChildren = 1000

  /** Characters that need to be escaped. Special characters that may be used: * ? */
  private final val SpecialChars =
    Set("\\+", "-", "&&", "\\|\\|", "!", "\\(", "\\)", "\\{", "\\}", "\\[", "\\]", "\\^", "\"", "~", ":", "\\\\", "\\/")

  /** Characters that need to be escaped in exact (quoted) queries. */
  private final val ExactSpecialChars = Set("\\*", "\\?")

  private final val EscapeRegex = SpecialChars.map(s => s"($s)").mkString("|").r

  private final val ExactEscapeRegex = (SpecialChars ++ ExactSpecialChars).map(s => s"($s)").mkString("|").r

  private def innerQuery(fq: List[FieldFilter], toQuery: ResultList[IdChildren.T] => String)(
      implicit queryService: SolrQueryService): Try[String] = {
    queryService.getResultList[IdChildren.T](FilterQuery(fq, MaxChildren))(FromSolrDoc[IdChildren.T]).map(toQuery)
  }

  private def anyInResult(res: ResultList[IdChildren.T]): String = res match {
    case ResultList(Vector(), _, _) => s"(${SolrQueryLiterals.Not}$All)"
    case ResultList(values, _, _) => s"(${values.flatten.mkString(" ")})"
  }

  /** Escapes a value string.
    *
    * @param value to escape
    * @param exact whether the value is used for exact mathing
    * @return escaped string, to be used in solr query
    */
  def escape(value: String, exact: Boolean): String = {
    if (exact) {
      "\"" + ExactEscapeRegex.replaceAllIn(value, m => Matcher.quoteReplacement(s"\\$m")) + "\""
    } else {
      val escaped = EscapeRegex.replaceAllIn(value, m => Matcher.quoteReplacement(s"\\$m"))
      if (escaped == "") "\"\"" else s"($escaped)"
    }
  }

  /** Maps a filter to a solr query string.
    *
    * @param filter to map
    * @param queryService to execute recursive queries
    * @return mapped filter or error if recursive querying failed
    */
  def mapFilter(filter: Filter)(implicit queryService: SolrQueryService): Try[String] = filter match {
    case Not(filter) => mapFilter(filter).map(f => s"(${SolrQueryLiterals.Not}$f)")
    case Or(f1, f2, fn @ _*) =>
      val filters: Try[Seq[String]] = (f1 +: f2 +: fn).map(mapFilter)
      filters.map(fs => s"(${fs.mkString(SolrQueryLiterals.Or)})")
    case And(f1, f2, fn @ _*) =>
      val filters: Try[Seq[String]] = (f1 +: f2 +: fn).map(mapFilter)
      filters.map(fs => s"(${fs.mkString(SolrQueryLiterals.And)})")
    case Term(inner) => Success(escape(inner, exact = false))
    case Exact(inner) => Success(escape(inner, exact = true))
    case InRange(from, to) => Success(s"[$from TO $to]")
    case InResult(of) => innerQuery(of, anyInResult)
  }
}
