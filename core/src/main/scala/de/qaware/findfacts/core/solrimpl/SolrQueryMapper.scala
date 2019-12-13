package de.qaware.findfacts.core.solrimpl

import de.qaware.findfacts.common.solr.SolrSchema
import de.qaware.findfacts.core.solrimpl.SolrQuery.BiConnective
import de.qaware.findfacts.core.{AbstractFQ, AllInResult, AnyInResult, FacetQuery, Filter, FilterComplement, FilterIntersection, FilterQuery, FilterTerm, FilterUnion, Id, InRange, Number, Query, StringExpression}
// scalastyle:off
import de.qaware.findfacts.common.utils.TryUtils._
// scalastyle:on
import org.apache.solr.client.solrj

import scala.language.postfixOps
import scala.util.Try

/** Solr-specific query constants. */
object SolrQuery {

  /** Match all elements. */
  val All = "*"

  /** Match all documents. */
  val QueryAll = "*:*"

  /** Query connective. */
  sealed trait Connective {

    /** String representation of connective. */
    val str: String

    override def toString: String = str
  }

  /** Connective connecting two terms. */
  sealed trait BiConnective extends Connective

  /** Intersects two terms */
  case object And extends BiConnective {
    override val str: String = " AND "
  }

  /** Unions two terms. */
  case object Or extends BiConnective {
    override val str: String = " OR "
  }

  /** Inverts a term */
  case object Not extends Connective {
    override val str: String = "NOT "
  }
}

/** Maps abstract filter terms to solr query strings. */
class SolrFilterTermMapper {

  /** Builds nested query by executing inner one */
  private def buildInnerQuery(queryService: SolrQueryService, fq: AbstractFQ, connective: BiConnective) = {
    queryService.getShortResults(FilterQuery(fq)) map {
      case Vector() =>
        connective match {
          case SolrQuery.And => s"${SolrQuery.All}"
          case SolrQuery.Or => s"(${SolrQuery.Not}${SolrQuery.All})"
        }
      case elems => s"(${elems.map(_.id).mkString(connective.str)})"
    }
  }

  /** Builds a filter string.
    *
    * @param queryService for recursive calls
    * @param filter term
    * @return query string or exception if rescursive call failed
    */
  def buildFilterQuery(queryService: SolrQueryService, filter: FilterTerm): Try[String] = filter match {
    case Id(inner) => Try(inner)
    case Number(inner) => Try(inner.toString)
    case StringExpression(inner) => Try(s"($inner)")
    case InRange(from, to) => Try(s"[$from TO $to]")
    case AnyInResult(fq) => buildInnerQuery(queryService, fq, SolrQuery.Or)
    case AllInResult(fq) => buildInnerQuery(queryService, fq, SolrQuery.And)
  }
}

/** Maps filter queries to solr query strings.
  *
  * @param termMapper to map filter terms
  */
class SolrFilterMapper(termMapper: SolrFilterTermMapper) {

  /** Builds filter query string. Solr filter caches can be configured to cache the result for each string.
    *
    * @param queryService for recursive calls
    * @param filter query
    * @return query strings that can be cached individually
    */
  def buildFilter(queryService: SolrQueryService, filter: AbstractFQ): Try[Seq[String]] = filter match {
    // format: off
    case Filter(fieldTerms) =>
      for {
        (field, filter) <- fieldTerms.toSeq
      } yield for {
        solrFilters <- termMapper.buildFilterQuery(queryService, filter)
      } yield s"${SolrSchema.getFieldName(field)}:$solrFilters"
    // TODO utilize filter caches better by breaking fqs into proper chunks
    case FilterIntersection(f1, f2, fn @ _*) =>
      for {
        filter <- f1 +: f2 +: fn
      } yield for {
        solrFilters <- buildFilter(queryService, filter)
      } yield solrFilters
    case FilterUnion(f1, f2, fn @ _*) =>
      val disjunctFilters: Try[Seq[String]] = for {
        filter <- f1 +: f2 +: fn
      } yield for {
        filterQuery <- buildFilter(queryService, filter).map(_.mkString(SolrQuery.And.str))
      } yield filterQuery
      disjunctFilters.map(fs => Seq(fs.mkString(SolrQuery.Or.str)))
    case FilterComplement(filter) => buildFilter(queryService, filter).map(f => Seq(s"(${SolrQuery.Not}$f)"))
    // format: on
  }
}

/** Maps a query to solr query string.
  *
  * @param filterMapper to map filter queries
  */
class SolrQueryMapper(filterMapper: SolrFilterMapper) {

  /** Builds solr query for a query.
    *
    * @param queryService for recursive calls
    * @param query to map
    * @return solrquery representation that can be fed to a solrJ client
    */
  def buildQuery(queryService: SolrQueryService, query: Query): Try[solrj.SolrQuery] = query match {
    case FacetQuery(filter, field) =>
      filterMapper.buildFilter(queryService, filter) map { fqs =>
        new solrj.SolrQuery()
          .setQuery(SolrQuery.QueryAll)
          .setFilterQueries(fqs: _*)
          .addFacetField(SolrSchema.getFieldName(field))
          .setFacetMinCount(1)
          .setFacetLimit(Int.MaxValue)
          .setRows(0)
      }
    case FilterQuery(filter, max) =>
      filterMapper.buildFilter(queryService, filter) map { fqs =>
        new solrj.SolrQuery()
          .setQuery(SolrQuery.QueryAll)
          .setFilterQueries(fqs: _*)
          .setFacet(false)
          .setRows(max)
      }
  }
}
