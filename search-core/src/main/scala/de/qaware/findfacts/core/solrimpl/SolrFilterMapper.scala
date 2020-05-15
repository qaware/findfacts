package de.qaware.findfacts.core.solrimpl

import java.util.regex.Matcher

import scala.util.{Failure, Success, Try}

import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._

import de.qaware.findfacts.common.da.api.{MultiValuedField, OptionalField, SingleValuedField}
import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core._
import de.qaware.findfacts.core.solrimpl.SolrQueryLiterals.{ALL, AND, NOT, OR}

/** Mapper to map filters to solr query strings. */
class SolrFilterMapper {

  private val EscapeRegex = SolrFilterMapper.SPECIAL_CHARS.map(s => s"($s)").mkString("|").r

  private val ExactEscapeRegex = {
    (SolrFilterMapper.SPECIAL_CHARS ++ SolrFilterMapper.EXACT_ADDITIONAL_SPECIAL_CHARS)
      .map(s => s"($s)").mkString("|").r
  }

  @SuppressWarnings(Array("AsInstanceOf"))
  private def innerQuery(field: EtField, fq: List[FieldFilter], toQuery: Seq[String] => String)(implicit
      index: String,
      queryService: SolrQueryService): Try[String] = {
    val results: Try[Seq[_]] = field match {
      case f: OptionalField[_] =>
        val results = queryService.getResults[f.T](FilterQuery(fq, SolrFilterMapper.MAX_INNER_RESULTS))
        results.map(_.values.flatMap(_.asInstanceOf[Option[_]]))
      case f: MultiValuedField[_] =>
        val results = queryService.getResults[f.T](FilterQuery(fq, SolrFilterMapper.MAX_INNER_RESULTS))
        results.map(_.values.flatMap(_.asInstanceOf[List[_]]))
      case f: SingleValuedField[_] =>
        queryService.getResults[f.T](FilterQuery(fq, SolrFilterMapper.MAX_INNER_RESULTS)).map(_.values)
      case _ => return Failure(new IllegalArgumentException(s"Field ${field.name} not allowed in query"))
    }
    results.map(_.map(_.toString)).map(toQuery)
  }

  private def anyInResult(res: Seq[String]): String = {
    if (res.isEmpty) {
      s"($NOT$ALL)"
    } else {
      s"(${res.map(escape(_, exact = true)).mkString(" ")})"
    }
  }

  /**
   * Escapes a value string.
   *
   * @param value to escape
   * @param exact whether the value is used for exact matching
   * @return escaped string, to be used in solr query
   */
  def escape(value: String, exact: Boolean): String = {
    if (exact) {
      ExactEscapeRegex.replaceAllIn(value, m => Matcher.quoteReplacement(s"\\$m"))
    } else {
      EscapeRegex.replaceAllIn(value, m => Matcher.quoteReplacement(s"\\$m"))
    }
  }

  private def escapeTerm(term: Term): String = {
    val escaped = escape(term.inner, exact = false)
    if (escaped.isBlank) {
      "\"" + escaped + "\""
    } else {
      s"($escaped)"
    }
  }

  private def escapeExact(exact: Exact): String = {
    "\"" + escape(exact.inner, exact = true) + "\"~" + SolrFilterMapper.DEFAULT_PHRASE_PROXIMITY
  }

  /**
   * Maps a filter to a solr query string.
   *
   * @param filter to map
   * @param queryService to execute recursive queries
   * @return mapped filter or error if recursive querying failed
   */
  def mapFilter(filter: Filter)(implicit index: String, queryService: SolrQueryService): Try[String] =
    filter match {
      case Not(filter) => mapFilter(filter).map(f => s"($NOT$f)")
      case Or(f1, f2, fn @ _*) =>
        (f1 +: f2 +: fn).map(mapFilter).toList.sequence.map(fs => s"(${fs.mkString(OR)})")
      case And(f1, f2, fn @ _*) =>
        (f1 +: f2 +: fn).map(mapFilter).toList.sequence.map(fs => s"(${fs.mkString(AND)})")
      case t: Term => Success(escapeTerm(t))
      case e: Exact => Success(escapeExact(e))
      case InRange(from, to) => Success(s"[$from TO $to]")
      case InResult(ofField, query) => innerQuery(ofField, query, anyInResult)
    }
}

/** Companion object. */
object SolrFilterMapper {

  /** Characters that need to be escaped. Special characters that may be used: '*' '?' */
  final val SPECIAL_CHARS = Set(
    "\\+",
    "-",
    "&&",
    "\\|\\|",
    "!",
    "\\(",
    "\\)",
    "\\{",
    "\\}",
    "\\[",
    "\\]",
    "\\^",
    "\"",
    "~",
    ":",
    "\\\\",
    "\\/",
    "\\s+")

  /** Characters that need to be escaped in exact (quoted) queries. */
  final val EXACT_ADDITIONAL_SPECIAL_CHARS = Set("\\*", "\\?")

  /** Maximum number of children for recursive queries. */
  final val MAX_INNER_RESULTS = 1000

  /** Default proximity for phrases. */
  final val DEFAULT_PHRASE_PROXIMITY = 10
}
