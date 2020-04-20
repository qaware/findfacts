package de.qaware.findfacts.core.solrimpl

import scala.collection.JavaConverters._
import scala.util.Try

import org.apache.solr.client.solrj
import org.apache.solr.client.solrj.SolrQuery.ORDER
import org.apache.solr.client.solrj.request.json.{DomainMap, JsonQueryRequest, TermsFacetMap}
import org.apache.solr.common.params.CursorMarkParams

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.solrimpl.SolrQueryLiterals.QUERY_PARENT
import de.qaware.findfacts.core.solrimpl.SolrQueryMapper._
import de.qaware.findfacts.core.{FacetQuery, FilterQuery}

/**
 * Mappers to map queries to solr query string.
 *
 * @param filterMapper to map filter queries
 */
class SolrQueryMapper(fieldFilterMapper: SolrFieldFilterMapper, filterMapper: SolrFilterMapper) {

  /**
   * Builds solr query (that retrieves parent blocks) for a filter query.
   *
   * @param queryService for recursive calls
   * @param index for recursive calls
   * @param query to map
   * @return solrquery representation that can be fed to a solrJ client
   */
  def buildBlockFilterQuery(
      query: FilterQuery)(implicit index: String, queryService: SolrQueryService): Try[solrj.SolrQuery] = {
    fieldFilterMapper.mapFieldFilters(query.filters) map { filters =>
      val solrQuery = new solrj.SolrQuery()
        .setFacet(false)
        .addField(CHILD_FIELD)
        .setRows(query.pageSize)
        .addSort(ScoreField, ORDER.desc)
        .addSort(EtField.Id.name, ORDER.asc)

      if (filters.fqs.nonEmpty) {
        solrQuery.setFilterQueries(filters.fqs: _*)
      }
      if (filters.childFqs.nonEmpty) {
        solrQuery
          .setQuery(PARENT_QUERY)
          .set(CHILD_FQ, filters.childFqs: _*)
      } else {
        solrQuery
          .setQuery(ParentQueryAllChildren)
      }

      // Set cursor to start or next cursor for paging
      solrQuery.set(
        CursorMarkParams.CURSOR_MARK_PARAM,
        query.cursor match {
          case None => CursorMarkParams.CURSOR_MARK_START
          case Some(cursor) => cursor
        })
      solrQuery
    }
  }

  /**
   * Builds solr query for a filter query. This will NOT resolve parent/child relations.
   *
   * @param queryService for recursive calls
   * @param index for recursive calls
   * @param query to map
   * @return solrquery representation that can be fed to a solrJ client
   */
  def buildFilterQuery(
      query: FilterQuery)(implicit index: String, queryService: SolrQueryService): Try[solrj.SolrQuery] = {
    fieldFilterMapper.mapFieldFilters(query.filters) map { filters =>
      val solrQuery = new solrj.SolrQuery()
        .setQuery(SolrQueryLiterals.QUERY_ALL)
        .setFacet(false)
        .addField(CHILD_FIELD)
        .setRows(query.pageSize)
        .addSort(ScoreField, ORDER.desc)
        .addSort(EtField.Id.name, ORDER.asc)

      solrQuery.setFilterQueries(filters.fqs ++ filters.childFqs: _*)

      // Set cursor to start or next cursor for paging
      solrQuery.set(
        CursorMarkParams.CURSOR_MARK_PARAM,
        query.cursor match {
          case None => CursorMarkParams.CURSOR_MARK_START
          case Some(cursor) => cursor
        })
      solrQuery
    }
  }

  /**
   * Builds solr query for a facet query.
   *
   * @param queryService for recursive calls
   * @param index for recursive calls
   * @param facetQuery to transform
   * @return solr query
   */
  def buildBlockFacetQuery(
      facetQuery: FacetQuery)(implicit index: String, queryService: SolrQueryService): Try[JsonQueryRequest] = {
    fieldFilterMapper.mapFieldFilters(facetQuery.filters) map { filters =>
      val jsonRequest = new JsonQueryRequest()
        .setLimit(0)

      // Add params
      if (filters.fqs.nonEmpty) {
        jsonRequest.withParam("fq", filters.fqs.toList.asJava)
      }
      if (filters.childFqs.nonEmpty) {
        jsonRequest
          .setQuery(PARENT_QUERY)
          .withParam(CHILD_FQ, filters.childFqs.toList.asJava)
      } else {
        jsonRequest.setQuery(ParentQueryAllChildren)
      }

      facetQuery.fields foreach { field =>
        val facet = new TermsFacetMap(field.name).setMinCount(1).setLimit(facetQuery.maxFacets + 1)
        if (field.isChild) {
          val domain = new DomainMap()
            .withTagsToExclude("top")
            .withFilter("{!filters param=$child.fq excludeTags=" + s"${field.name}}")
            .withFilter(if (filters.fqs.nonEmpty) ChildQuery else s"{!child of=$QUERY_PARENT}")

          // For child fields, go to child documents domain and then count unique parent blocks.
          facet.withDomain(domain).withStatSubFacet(CountField, "uniqueBlock(_root_)")
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

/** Companion object. */
object SolrQueryMapper {

  /** Child filter query field. */
  final val CHILD_FQ = "child.fq"

  /** Query name of '_childDocuments_' field. */
  final val CHILD_FIELD = s"[child parentFilter=$QUERY_PARENT limit=-1]"

  /** Query to select parent docs. */
  final val PARENT_QUERY = "{!parent tag=top filters=$child.fq which=" + s"$QUERY_PARENT}"

  /** Parent query when there are no child filters. */
  final val ParentQueryAllChildren = "{!parent tag=top which=" + s"$QUERY_PARENT}"

  /** Query to select child docs. */
  final val ChildQuery = s"{!child of=$QUERY_PARENT filters=" + "$fq}"

  /** Name of field for block aggregation subfacet. Overrides the default 'count' field. */
  final val CountField = "count"

  /** Name of the field which scores results, on which the result set is sorted. */
  final val ScoreField = "score"
}
