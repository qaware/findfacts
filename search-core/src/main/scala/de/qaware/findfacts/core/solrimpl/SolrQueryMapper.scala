package de.qaware.findfacts.core.solrimpl

import java.util.regex.Matcher

import scala.collection.mutable
import scala.util.Failure

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.common.dt.EtField.{Children, Id}
import de.qaware.findfacts.common.dt.solr.SolrSchema
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.core.QueryService.ResultList
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
import org.apache.solr.common.params.CursorMarkParams
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

  /** Children for id-only blocks. */
  case object IdChildren extends Children[Id.T] {
    override implicit val implicits: FieldImplicits[Id.T] = FieldImplicits()
  }

  /** Characters that need to be escaped. Special characters that may be used: * ? */
  final val SpecialCharacters =
    Set("\\+", "-", "&&", "\\|\\|", "!", "\\(", "\\)", "\\{", "\\}", "\\[", "\\]", "\\^", "\"", "~", ":", "\\\\", "\\/")

  /** Regex built from special characters */
  final private val EscapeRegex = SpecialCharacters.map(s => s"($s)").mkString("|").r

  /** Builds nested query by executing inner one */
  private def buildInnerQuery(fq: AbstractFQ, connective: BiConnective)(implicit queryService: SolrQueryService) = {
    queryService.getResultList[IdChildren.T](FilterQuery(fq, -1))(FromSolrDoc[IdChildren.T]) map {
      case ResultList(Vector(), _, _) =>
        connective match {
          case SolrMapper.And => s"${SolrMapper.All}"
          case SolrMapper.Or => s"(${SolrMapper.Not}${SolrMapper.All})"
        }
      case ResultList(elems, _, _) => s"(${elems.mkString(connective.str)})"
    }
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

  /** Builds a filter string.
    *
    * @param filter term
    * @param queryService for recursive calls
    * @return query string or exception if rescursive call failed
    */
  def buildFilterQuery(filter: FilterTerm)(implicit queryService: SolrQueryService): Try[String] = filter match {
    case Term(inner) => Try(escape(inner, exact = false))
    case Exact(inner) => Try(escape(inner, exact = true))
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

  /** Prefix for child query names. */
  final val ChildFQPrefix = "childfq"

  /** Selector field to choose parent documents. */
  final val ParentSelector = "!parent which"

  /** Filter field for child filters. */
  final val ChildFilters = "filters"

  private def buildFieldTerms(fieldTerm: (EtField, FilterTerm))(
      implicit queryService: SolrQueryService): Try[String] = {
    termMapper.buildFilterQuery(fieldTerm._2).map(filter => s"${fieldTerm._1.name}:$filter")
  }

  private def makeQueryString(parentTerms: Seq[String], childTerms: Seq[String])(
      implicit qParams: mutable.Map[String, Seq[String]]): String = {
    val parentF = if (parentTerms.isEmpty) "" else parentTerms.map(e => s" +$e").mkString
    if (childTerms.isEmpty) {
      s"$parentF +{$ParentSelector=${SolrMapper.ParentFilter}}"
    } else {
      // Make unique name
      val filter = s"$ChildFQPrefix${qParams.size}"
      // Add child filter query (needs to be query parameter as solr query parser can't handle complex queries otherwise)
      qParams.put(filter, childTerms)
      s"$parentF +{$ParentSelector=${SolrMapper.ParentFilter} $ChildFilters=" + "$" + s"$filter}"
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
      } yield s"(${bothQ.map(e => s"$e${SolrMapper.And.str}").mkString}${makeQueryString(parentQ, childQ)})"
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
class SolrQueryMapper(filterMapper: SolrFilterMapper, filterTermMapper: SolrFilterTermMapper) {

  /** Query name of '_childDocuments_' field. */
  final val ChildField = s"[child parentFilter=${SolrMapper.ParentFilter} limit=-1]"

  /** Root field name */
  final val RootField = "_root_"

  /** Stats subfacet to aggregate parent blocks. */
  final val BlockAggregation = s"uniqueBlock($RootField)"

  /** Name of field for block aggregation subfacet. Overrides the default 'count' field. */
  final val BlockCountField = "count"

  /** Name of the field which scores results, on which the result set is sorted. */
  final val ScoreField = "score"

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

      // Set cursor to start or next cursor for paging
      solrQuery.set(CursorMarkParams.CURSOR_MARK_PARAM, query.cursor match {
        case None => CursorMarkParams.CURSOR_MARK_START
        case Some(cursor) => cursor
      })
      qParams.foreach(param => solrQuery.setParam(param._1, param._2: _*))
      solrQuery
        .setFacet(false)
        .addField(ChildField)
        .setRows(query.pageSize)
        .addSort(ScoreField, ORDER.desc)
        .addSort(EtField.Id.name, ORDER.asc)
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

  /** Builds a solr query to retrieve a single document by id.
    *
    * @param id to search for
    * @return built solr query
    */
  def buildSingleQuery(id: EtField.Id.T): solrj.SolrQuery = {
    new solrj.SolrQuery()
      .setQuery(s"${EtField.Id.name}:${filterTermMapper.escape(id, exact = false)}")
      .setFields(SolrMapper.All, ChildField)
  }
}
