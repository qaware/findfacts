package de.qaware.findfacts.core.solrimpl

import java.util.regex.Matcher

import scala.collection.mutable
import scala.util.Failure

import de.qaware.findfacts.common.dt.solr.SolrSchema
import de.qaware.findfacts.common.dt.{EtField, IdEt}
import de.qaware.findfacts.core.solrimpl.SolrMapper.BiConnective
import de.qaware.findfacts.core.{
  AbstractFQ,
  AllInResult,
  AnyInResult,
  Exact,
  FacetQuery,
  Filter,
  FilterComplement,
  FilterIntersection,
  FilterQuery,
  FilterTerm,
  FilterUnion,
  InRange,
  Term
}
import org.apache.solr.client.solrj.SolrQuery.ORDER
import org.apache.solr.client.solrj.request.json.{DomainMap, JsonQueryRequest, TermsFacetMap}
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

  final val QueryAll = "*:*"

  /** Match only parent docs. */
  final val ParentFilter = s"${SolrSchema.CommandKind}:*"

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

  /** Characters that need to be escaped. Special characters that may be used: * ? */
  final val SpecialCharacters =
    Set("\\+", "-", "&&", "\\|\\|", "!", "\\(", "\\)", "\\{", "\\}", "\\[", "\\]", "\\^", "\"", "~", ":", "\\\\", "\\/")

  /** Regex built from special characters */
  final private val EscapeRegex = SpecialCharacters.map(s => s"($s)").mkString("|").r

  /** Default maximum number of result for recursive query. */
  final val DefaultMaxResults = 100

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

  private def escape(s: String): String = {
    EscapeRegex.replaceAllIn(s, m => Matcher.quoteReplacement(s"\\$m"))
  }

  /** Builds a filter string.
    *
    * @param filter term
    * @param queryService for recursive calls
    * @return query string or exception if rescursive call failed
    */
  def buildFilterQuery(filter: FilterTerm)(implicit queryService: SolrQueryService): Try[String] = filter match {
    case Term(inner) => Try(s"(${escape(inner)})")
    case Exact(inner) => Try("\"" + escape(inner) + "\"")
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
      } yield s"(${bothQ.map(e => s"$e AND").mkString}${makeQueryString(parentQ, childQ)})"
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

  /** Selector for parents. */
  final val SelectParents = "child parentFilter"

  /** Root field name */
  final val RootField = "_root_"

  /** Stats subfacet to aggregate parent blocks. */
  final val BlockAggregation = s"uniqueBlock($RootField)"

  /** Name of field for block aggregation subfacet. Overrides the default 'count' field. */
  final val BlockCountField = "count"

  /** Builds solr query for a filter query.
    *
    * @param queryService for recursive calls
    * @param query to map
    * @return solrquery representation that can be fed to a solrJ client
    */
  def buildFilterQuery(queryService: SolrQueryService, query: FilterQuery): Try[solrj.SolrQuery] = {
    val qParams = mutable.Map.empty[String, Seq[String]]
    for {
      fq <- filterMapper.buildFilter(query.filter)(qParams, queryService)
    } yield {
      val solrQuery = new solrj.SolrQuery().setQuery(fq)
      qParams.foreach(param => solrQuery.setParam(param._1, param._2: _*))
      solrQuery
        .setFacet(false)
        .addField(s"[$SelectParents=${SolrMapper.ParentFilter} limit=-1]")
        .addSort(RootField, ORDER.desc)
        .setRows(query.maxResults)
    }
  }

  /** Builds solr query for a facet query.
    *
    * @param queryService for recursive calls
    * @param facetQuery to transform
    * @return solr query
    */
  def buildFacetQuery(queryService: SolrQueryService, facetQuery: FacetQuery): Try[JsonQueryRequest] = {
    // Faceting on ID field does not make any sense,
    // and won't work properly because it's a field of both parent and children
    if (facetQuery.fields.contains(EtField.Id)) {
      return Failure(new IllegalArgumentException("Cannot facet on Id field!"))
    }

    val qParams = mutable.Map.empty[String, Seq[String]]
    for {
      fq <- filterMapper.buildFilter(facetQuery.filter)(qParams, queryService)
    } yield {
      val domainMap = new DomainMap().setBlockChildQuery(SolrMapper.ParentFilter)
      val jsonRequest = new JsonQueryRequest()

      facetQuery.fields foreach { field =>
        val facet = new TermsFacetMap(field.name).setMinCount(1).setLimit(facetQuery.maxFacets + 1)
        if (field.isChild) {
          // For child fields, go to child documents domain and then count unique parent blocks.
          facet.withDomain(domainMap).withStatSubFacet(BlockCountField, BlockAggregation)
        }
        jsonRequest.withFacet(field.name, facet)
      }

      qParams foreach {
        case (name, vals) => jsonRequest.withParam(name, vals.mkString(SolrMapper.And.str))
      }

      jsonRequest.setLimit(0).withFilter(fq).setQuery(SolrMapper.QueryAll)
    }
  }
}
