package de.qaware.findfacts.core.solrimpl

import scala.collection.JavaConverters._
import scala.util.Try

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.solrimpl.SolrQueryLiterals.{ParentTag, QueryParent}
import de.qaware.findfacts.core.{FacetQuery, FilterQuery}
import org.apache.solr.client.solrj
import org.apache.solr.client.solrj.SolrQuery.ORDER
import org.apache.solr.client.solrj.request.json.{DomainMap, JsonQueryRequest, TermsFacetMap}
import org.apache.solr.common.params.CursorMarkParams

/** Mappers to map queries to solr query string.
  *
  * @param filterMapper to map filter queries
  */
class SolrQueryMapper(fieldFilterMapper: SolrFieldFilterMapper, filterMapper: SolrFilterMapper) {

  /** Query name of '_childDocuments_' field. */
  final val ChildField = s"[child parentFilter=$QueryParent limit=-1]"

  /** Filter query field. */
  final val Fq = "fq"

  /** Child filter query field. */
  final val ChildFq = "child.fq"

  /** Query to select parent docs. */
  final val ParentQuery = "{!parent tag=top filters=$" + s"$ChildFq which=" + s"$QueryParent}"

  /** Query to select all docs */
  final val AllQuery = "*:*"

  /** Parent query when there are no child filters. */
  final val ParentQueryAllChildren = "{!parent tag=top which=" + s"$QueryParent}"

  /** Query to select child docs. */
  final val ChildQuery = s"{!child of=$QueryParent filters=" + "$fq}"

  /** Child query when there are no parent filters. */
  final val ChildQueryAllParents = s"{!child of=$QueryParent}"

  /** Stats subfacet to aggregate parent blocks. */
  final val BlockAggregation = s"uniqueBlock(_root_)"

  /** (Open) environment for additional filters. */
  final val FilterEnv = "{!filters param=$child.fq excludeTags="

  /** Name of field for block aggregation subfacet. Overrides the default 'count' field. */
  final val CountField = "count"

  /** Name of the field which scores results, on which the result set is sorted. */
  final val ScoreField = "score"

  /** Builds solr query (that retrieves parent blocks) for a filter query.
    *
    * @param queryService for recursive calls
    * @param query to map
    * @return solrquery representation that can be fed to a solrJ client
    */
  def buildBlockFilterQuery(query: FilterQuery)(implicit queryService: SolrQueryService): Try[solrj.SolrQuery] = {
    fieldFilterMapper.mapFieldFilters(query.filters)(queryService) map { filters =>
      val solrQuery = new solrj.SolrQuery()
        .setFacet(false)
        .addField(ChildField)
        .setRows(query.pageSize)
        .addSort(ScoreField, ORDER.desc)
        .addSort(EtField.Id.name, ORDER.asc)

      if (filters.fqs.nonEmpty) {
        solrQuery.setFilterQueries(filters.fqs: _*)
      }
      if (filters.childFqs.nonEmpty) {
        solrQuery
          .setQuery(ParentQuery)
          .set(ChildFq, filters.childFqs: _*)
      } else {
        solrQuery
          .setQuery(ParentQueryAllChildren)
      }

      // Set cursor to start or next cursor for paging
      solrQuery.set(CursorMarkParams.CURSOR_MARK_PARAM, query.cursor match {
        case None => CursorMarkParams.CURSOR_MARK_START
        case Some(cursor) => cursor
      })
      solrQuery
    }
  }

  /** Builds solr query for a filter query. This will NOT resolve parent/child relations.
    *
    * @param queryService for recursive calls
    * @param query to map
    * @return solrquery representation that can be fed to a solrJ client
    */
  def buildFilterQuery(query: FilterQuery)(implicit queryService: SolrQueryService): Try[solrj.SolrQuery] = {
    fieldFilterMapper.mapFieldFilters(query.filters)(queryService) map { filters =>
      val solrQuery = new solrj.SolrQuery()
        .setQuery(AllQuery)
        .setFacet(false)
        .addField(ChildField)
        .setRows(query.pageSize)
        .addSort(ScoreField, ORDER.desc)
        .addSort(EtField.Id.name, ORDER.asc)

      solrQuery.setFilterQueries(filters.fqs ++ filters.childFqs: _*)

      // Set cursor to start or next cursor for paging
      solrQuery.set(CursorMarkParams.CURSOR_MARK_PARAM, query.cursor match {
        case None => CursorMarkParams.CURSOR_MARK_START
        case Some(cursor) => cursor
      })
      solrQuery
    }
  }

  /** Builds solr query for a facet query.
    *
    * @param queryService for recursive calls
    * @param facetQuery to transform
    * @return solr query
    */
  def buildBlockFacetQuery(facetQuery: FacetQuery)(implicit queryService: SolrQueryService): Try[JsonQueryRequest] = {
    fieldFilterMapper.mapFieldFilters(facetQuery.filters)(queryService) map { filters =>
      val jsonRequest = new JsonQueryRequest()
        .setLimit(0)

      // Add params
      if (filters.fqs.nonEmpty) {
        jsonRequest.withParam(Fq, filters.fqs.toList.asJava)
      }
      if (filters.childFqs.nonEmpty) {
        jsonRequest
          .setQuery(ParentQuery)
          .withParam(ChildFq, filters.childFqs.toList.asJava)
      } else {
        jsonRequest.setQuery(ParentQueryAllChildren)
      }

      facetQuery.fields foreach { field =>
        val facet = new TermsFacetMap(field.name).setMinCount(1).setLimit(facetQuery.maxFacets + 1)
        if (field.isChild) {
          val domain = new DomainMap()
            .withTagsToExclude(ParentTag)
            .withFilter(s"$FilterEnv${field.name}}")
            .withFilter(if (filters.fqs.nonEmpty) ChildQuery else ChildQueryAllParents)

          // For child fields, go to child documents domain and then count unique parent blocks.
          facet.withDomain(domain).withStatSubFacet(CountField, BlockAggregation)
        } else {
          facet.withDomain(new DomainMap().withTagsToExclude(field.name))
        }
        // Add facet
        jsonRequest.withFacet(field.name, facet)
      }

      jsonRequest
    }
  }
}
