package de.qaware.findfacts.core.solrimpl

import java.util.regex.Matcher

import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._
import de.qaware.findfacts.common.da.api.{MultiValuedField, OptionalField, SingleValuedField}
import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.solrimpl.SolrQueryLiterals.All
import de.qaware.findfacts.core.{And, Exact, FieldFilter, Filter, FilterQuery, InRange, InResult, Not, Or, Term}

import scala.util.{Failure, Success, Try}

/** Mapper to map filters to solr query strings. */
class SolrFilterMapper {

  /** Maximum number of children for recursive queries. */
  final val MaxInnerResult = 1000

  /** Default proximity for phrases. */
  final val DefaultPhraseProximity = 10

  /** Characters that need to be escaped. Special characters that may be used: * ? */
  private final val SpecialChars =
    Set("\\+", "-", "&&", "\\|\\|", "!", "\\(", "\\)", "\\{", "\\}", "\\[", "\\]", "\\^", "\"", "~", ":", "\\\\", "\\/", "\\s+")

  /** Characters that need to be escaped in exact (quoted) queries. */
  private final val ExactSpecialChars = Set("\\*", "\\?")

  private final val EscapeRegex = SpecialChars.map(s => s"($s)").mkString("|").r

  private final val ExactEscapeRegex = (SpecialChars ++ ExactSpecialChars).map(s => s"($s)").mkString("|").r

  private def innerQuery(field: EtField, fq: List[FieldFilter], toQuery: Seq[String] => String)(
      implicit index: String,
      queryService: SolrQueryService): Try[String] = {
    val results: Try[Seq[_]] = field match {
      case f: OptionalField[_] =>
        val results = queryService.getResults[f.T](FilterQuery(fq, MaxInnerResult))
        results.map(_.values.flatMap(_.asInstanceOf[Option[_]]))
      case f: MultiValuedField[_] =>
        val results = queryService.getResults[f.T](FilterQuery(fq, MaxInnerResult))
        results.map(_.values.flatMap(_.asInstanceOf[List[_]]))
      case f: SingleValuedField[_] =>
        queryService.getResults[f.T](FilterQuery(fq, MaxInnerResult)).map(_.values)
      case _ => return Failure(new IllegalArgumentException(s"Field ${field.name} not allowed in query"))
    }
    results.map(_.map(_.toString)).map(toQuery)
  }

  private def anyInResult(res: Seq[String]): String = {
    if (res.isEmpty) {
      s"(${SolrQueryLiterals.Not}$All)"
    } else {
      s"(${res.mkString(" ")})"
    }
  }

  /** Escapes a value string.
    *
    * @param value to escape
    * @param exact whether the value is used for exact matching
    * @return escaped string, to be used in solr query
    */
  def escape(value: String, exact: Boolean): String = {
    if (exact) {
      "\"" + ExactEscapeRegex.replaceAllIn(value, m => Matcher.quoteReplacement(s"\\$m")) + "\"~" + DefaultPhraseProximity
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
  def mapFilter(filter: Filter)(implicit index: String, queryService: SolrQueryService): Try[String] = filter match {
    case Not(filter) => mapFilter(filter).map(f => s"(${SolrQueryLiterals.Not}$f)")
    case Or(f1, f2, fn @ _*) =>
      (f1 +: f2 +: fn).map(mapFilter).toList.sequence.map(fs => s"(${fs.mkString(SolrQueryLiterals.Or)})")
    case And(f1, f2, fn @ _*) =>
      (f1 +: f2 +: fn).map(mapFilter).toList.sequence.map(fs => s"(${fs.mkString(SolrQueryLiterals.And)})")
    case Term(inner) => Success(escape(inner, exact = false))
    case Exact(inner) => Success(escape(inner, exact = true))
    case InRange(from, to) => Success(s"[$from TO $to]")
    case InResult(ofField, query) => innerQuery(ofField, query, anyInResult)
  }
}
