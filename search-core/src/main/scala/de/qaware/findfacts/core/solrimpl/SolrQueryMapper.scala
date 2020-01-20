package de.qaware.findfacts.core.solrimpl

import scala.collection.mutable

import de.qaware.findfacts.common.dt.{EtField, IdEt}
import de.qaware.findfacts.core.solrimpl.SolrMapper.BiConnective
import de.qaware.findfacts.core.{
  AbstractFQ,
  AllInResult,
  AnyInResult,
  FacetQuery,
  Filter,
  FilterComplement,
  FilterIntersection,
  FilterQuery,
  FilterTerm,
  FilterUnion,
  Id,
  InRange,
  Number,
  Query,
  StringExpression
}
// scalastyle:off
import de.qaware.findfacts.common.utils.TryUtils._
// scalastyle:on
import scala.language.postfixOps
import scala.util.Try

import org.apache.solr.client.solrj

/** Solr-specific query constants. */
object SolrMapper {

  /** Match all elements. */
  final val All = "*"

  // TODO real query {!parent which='-_nest_path_:* *:*'}
  final val ParentFilter = "kind:Block"

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
  final val DefaultMaxResults = 10

  /** Builds nested query by executing inner one */
  private def buildInnerQuery(fq: AbstractFQ, connective: BiConnective)(implicit queryService: SolrQueryService) = {
    queryService.getListResults[IdEt](FilterQuery(fq, DefaultMaxResults)) map {
      case Vector() =>
        connective match {
          case SolrMapper.And => s"${SolrMapper.All}"
          case SolrMapper.Or => s"(${SolrMapper.Not}${SolrMapper.All})"
        }
      case elems: Any => s"(${elems.flatMap(_.children.map(_.id)).mkString(connective.str)})"
    }
  }

  /** Builds a filter string.
    *
    * @param filter term
    * @param queryService for recursive calls
    * @return query string or exception if rescursive call failed
    */
  def buildFilterQuery(filter: FilterTerm)(implicit queryService: SolrQueryService): Try[String] = filter match {
    case Id(inner) => Try(inner) // TODO escaping
    case Number(inner) => Try(inner.toString)
    case StringExpression(inner) => Try(s"($inner)") // TODO escaping
    case InRange(from, to) => Try(s"[$from TO $to]")
    case AnyInResult(fq) => buildInnerQuery(fq, SolrMapper.Or)
    case AllInResult(fq) => buildInnerQuery(fq, SolrMapper.And)
  }
}

/** Maps filter queries to solr query strings.
  *
  * @param termMapper to map filter terms
  */
class SolrFilterMapper(termMapper: SolrFilterTermMapper) {

  private def buildFieldTerms(fieldTerm: (EtField, FilterTerm))(
      implicit queryService: SolrQueryService): Try[String] = {
    termMapper.buildFilterQuery(fieldTerm._2).map(filter => s"${fieldTerm._1.name}:$filter")
  }

  private def makeQueryString(parentTerms: Seq[String], childTerms: Seq[String])(
      implicit qParams: mutable.Map[String, Seq[String]]): String = {
    val parentF = if (parentTerms.isEmpty) "" else parentTerms.map(e => s" +$e").mkString
    if (childTerms.isEmpty) {
      s"$parentF +{!parent which=${SolrMapper.ParentFilter}}"
    } else {
      // Make unique name
      val filter = s"childfq${qParams.size}"
      // Add child filter query (needs to be query parameter as solr query parser can't handle complex queries otherwise)
      qParams.put(filter, childTerms)
      s"$parentF +{!parent which=${SolrMapper.ParentFilter} filters=" + "$" + s"$filter}"
    }
  }

  /** Builds filter query string. Solr filter caches can be configured to cache the result for each string.
    *
    * @param filter query
    * @param qParams additional query parameters
    * @param queryService for recursive calls
    * @return query strings that can be cached individually
    */
  def buildFilter(filter: AbstractFQ)(
      implicit qParams: mutable.Map[String, Seq[String]],
      queryService: SolrQueryService): Try[String] = filter match {
    case Filter(fieldTerms) =>
      val (parentFieldQuery, others) = fieldTerms.toSeq.partition(!_._1.isChild)
      val (childFieldsQuery, bothFieldsQuery) = others.partition(!_._1.isParent)

      val parentFieldTerms: Try[Seq[String]] = parentFieldQuery.map(buildFieldTerms)
      val childFieldTerms: Try[Seq[String]] = childFieldsQuery.map(buildFieldTerms)
      val bothFieldTerms: Try[Seq[String]] = bothFieldsQuery.map(buildFieldTerms)

      for {
        parentQ <- parentFieldTerms
        childQ <- childFieldTerms
        bothQ <- bothFieldTerms
      } yield {
        if (bothQ.nonEmpty) {
          s"(${makeQueryString(parentQ ++ bothQ, childQ)} OR${makeQueryString(parentQ, childQ ++ bothQ)})"
        } else {
          makeQueryString(parentQ, childQ)
        }
      }
    case FilterIntersection(f1, f2, fn @ _*) =>
      val filters: Try[Seq[String]] = (f1 +: f2 +: fn).map(buildFilter)
      filters.map(_.map(e => s"($e)").mkString(SolrMapper.And.str))
    case FilterUnion(f1, f2, fn @ _*) =>
      val filters: Try[Seq[String]] = (f1 +: f2 +: fn).map(buildFilter)
      filters.map(_.map(e => s"($e)").mkString(SolrMapper.Or.str))
    case FilterComplement(filter) => buildFilter(filter).map(f => s"(${SolrMapper.Not}$f")
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
  def buildQuery(queryService: SolrQueryService, query: Query): Try[solrj.SolrQuery] = {
    // Make context impmlicit
    implicit val queryParams: mutable.Map[String, Seq[String]] = mutable.Map.empty
    implicit val qs: SolrQueryService = queryService

    query match {
      case FacetQuery(filter, field) =>
        buildFilterQuery(filter).map(
          _.addFacetField(field.name)
            .setFacetMinCount(1)
            .setFacetLimit(Int.MaxValue)
            .setRows(0))
      case FilterQuery(filter, max) =>
        buildFilterQuery(filter).map(
          _.setFacet(false)
            .addField(s"[child parentFilter=${SolrMapper.ParentFilter}]")
            .setRows(max)
        )
    }
  }

  private def buildFilterQuery(filter: AbstractFQ)(
      implicit qParams: mutable.Map[String, Seq[String]],
      queryService: SolrQueryService): Try[solrj.SolrQuery] = {
    for {
      fq <- filterMapper.buildFilter(filter)
    } yield {
      val solrQuery = new solrj.SolrQuery().setQuery(fq)
      qParams.foreach(param => solrQuery.setParam(param._1, param._2: _*))
      solrQuery
    }
  }
}
