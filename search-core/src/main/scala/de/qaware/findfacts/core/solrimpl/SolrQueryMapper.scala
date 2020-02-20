package de.qaware.findfacts.core.solrimpl

import scala.collection.mutable
import scala.util.Try

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.{FacetQuery, FilterQuery}
import org.apache.solr.client.solrj
import org.apache.solr.client.solrj.SolrQuery.ORDER
import org.apache.solr.client.solrj.request.json.{DomainMap, JsonQueryRequest, TermsFacetMap}
import org.apache.solr.common.params.CursorMarkParams

/** Maps a query to solr query string.
  *
  * @param filterMapper to map filter queries
  */
class SolrQueryMapper(filterMapper: SolrAbstractFqMapper, filterTermMapper: SolrFilterTermMapper) {

  /** Query name of '_childDocuments_' field. */
  final val ChildField = s"[child parentFilter=${SolrQueryLiterals.QueryParent} limit=-1]"

  /** Stats subfacet to aggregate parent blocks. */
  final val BlockAggregation = s"uniqueBlock(_root_)"

  /** Name of field for block aggregation subfacet. Overrides the default 'count' field. */
  final val CountField = "count"

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
    val qParams = mutable.Map.empty[String, Seq[String]]
    for {
      fq <- filterMapper.buildFilter(facetQuery.filter)(qParams, queryService)
    } yield {
      val domainMap = new DomainMap().setBlockChildQuery(SolrQueryLiterals.QueryParent)
      val jsonRequest = new JsonQueryRequest()

      facetQuery.fields foreach { field =>
        val facet = new TermsFacetMap(field.name).setMinCount(1).setLimit(facetQuery.maxFacets + 1)
        if (field.isChild) {
          // For child fields, go to child documents domain and then count unique parent blocks.
          facet.withDomain(domainMap).withStatSubFacet(CountField, BlockAggregation)
        }
        jsonRequest.withFacet(field.name, facet)
      }

      qParams foreach {
        case (name, vals) => jsonRequest.withParam(name, vals.mkString(SolrQueryLiterals.And))
      }

      jsonRequest.setLimit(0).withFilter(fq).setQuery(SolrQueryLiterals.QueryAll)
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
      .setFields(SolrQueryLiterals.All, ChildField)
  }
}
