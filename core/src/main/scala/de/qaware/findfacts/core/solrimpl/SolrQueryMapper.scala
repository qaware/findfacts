package de.qaware.findfacts.core.solrimpl

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.common.solr.SolrSchema
import de.qaware.findfacts.core.solrimpl.SolrQuery.BiConnective
import de.qaware.findfacts.core.{AbstractFQ, AllInResult, AnyInResult, FacetQuery, Filter, FilterComplement, FilterIntersection, FilterQuery, FilterTerm, FilterUnion, Id, InRange, Number, Query, StringExpression}
import de.qaware.findfacts.scalautils.EitherUtils
import org.apache.solr.client.solrj

import scala.language.postfixOps

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
  private def buildInnerQuery(
      queryService: SolrQueryService,
      fq: AbstractFQ,
      field: EtField,
      connective: BiConnective) = {
    queryService.getResults(FilterQuery(fq)) match {
      case Left(err) => Left(err)
      case Right(Vector()) =>
        connective match {
          case SolrQuery.And => Right(s"${SolrSchema.getFieldName(field)}:${SolrQuery.All}")
          case SolrQuery.Or => Right(s"${SolrQuery.Not}${SolrSchema.getFieldName(field)}:${SolrQuery.All}")
        }
      case Right(elems) => Right(s"${SolrSchema.getFieldName(field)}:(${elems.map(_.id).mkString(connective.str)})")
    }
  }

  /** Builds a filter string.
    *
    * @param queryService for recursive calls
    * @param filter term
    * @return query string or exception if rescursive call failed
    */
  def buildFilterQuery(queryService: SolrQueryService, filter: FilterTerm): Either[Throwable, String] = filter match {
    case Id(inner) => Right(inner)
    case Number(inner) => Right(inner.toString)
    case StringExpression(inner) => Right(inner)
    case InRange(from, to) => Right(s"[$from TO $to]")
    case AnyInResult(fq, field) => buildInnerQuery(queryService, fq, field, SolrQuery.Or)
    case AllInResult(fq, field) => buildInnerQuery(queryService, fq, field, SolrQuery.And)
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
  def buildFilter(queryService: SolrQueryService, filter: AbstractFQ): Either[Throwable, Seq[String]] = filter match {
    case Filter(fieldTerms) =>
      EitherUtils.sequence(fieldTerms.mapValues(termMapper.buildFilterQuery(queryService, _)) map {
        case (field, term) => term.right.map(t => s"${SolrSchema.getFieldName(field)}:$t")
      } toSeq)
    // TODO utilize filter caches better by breaking fqs into largest re-used chunks
    case FilterIntersection(f1, f2, fn @ _*) =>
      EitherUtils.sequence((f1 +: f2 +: fn).map(buildFilter(queryService, _))).map(_.flatten)
    case FilterUnion(f1, f2, fn @ _*) =>
      EitherUtils
        .sequence((f1 +: f2 +: fn).map(buildFilter(queryService, _)))
        .map(_.mkString(SolrQuery.Or.str))
        .map(s => Seq(s"($s)"))
    case FilterComplement(filter) => buildFilter(queryService, filter).map(f => Seq(s"(${SolrQuery.Not}$f)"))
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
  def buildQuery(queryService: SolrQueryService, query: Query): Either[Throwable, solrj.SolrQuery] = query match {
    case FacetQuery(filter, field) =>
      filterMapper.buildFilter(queryService, filter).right map { fqs =>
        new solrj.SolrQuery()
          .setQuery(SolrQuery.QueryAll)
          .setFilterQueries(fqs: _*)
          .addFacetField(SolrSchema.getFieldName(field))
          .setFacetMinCount(1)
          .setFacetLimit(Int.MaxValue)
          .setRows(0)
      }
    case FilterQuery(filter) =>
      filterMapper.buildFilter(queryService, filter).right map { fqs =>
        new solrj.SolrQuery()
          .setQuery(SolrQuery.QueryAll)
          .setFilterQueries(fqs: _*)
          .setFacet(false)
      }
  }
}
