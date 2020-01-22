package de.qaware.findfacts.core.solrimpl

import scala.collection.JavaConverters._
import scala.language.{postfixOps, reflectiveCalls}
import scala.util.Try

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.{BlockEt, EtField, ShortEt}
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.common.utils.TryUtils.flattenTryFailFirst
import de.qaware.findfacts.core.{FacetQuery, FilterQuery, QueryService}
import org.apache.solr.client.solrj
import org.apache.solr.client.solrj.SolrClient
import org.apache.solr.client.solrj.response.QueryResponse

/** Solr impl of the query service.
  *
  * @param connection solr instance
  * @param mapper solr query mapper
  */
class SolrQueryService(connection: SolrClient, mapper: SolrQueryMapper) extends QueryService {
  private val logger = Logger[SolrQueryService]

  /** Get result from solr */
  private def getSolrResult(query: solrj.SolrQuery): Try[QueryResponse] = {
    Try {
      logger.info(s"Executing query $query")
      val resp = connection.query(query)
      if (resp.getStatus != 0 && resp.getStatus != 200) {
        throw new IllegalStateException(s"Query status was not ok: $resp")
      }
      logger.info(s"Found ${resp.getResults.getNumFound} entries")
      resp
    }
  }

  override def getFacetResults(facetQuery: FacetQuery): Try[Map[EtField, Map[String, Long]]] = {
    for {
      solrQuery <- mapper.buildFacetQuery(this, facetQuery)
      solrResult <- getSolrResult(solrQuery)
      result <- Try {
        val res = facetQuery.fields.map { field =>
          field -> solrResult
            .getFacetField(field.name)
            .getValues
            .asScala
            .groupBy(_.getName)
            .mapValues(_.head.getCount)
        }
        res.filter(_._2.size < facetQuery.maxFacets).toMap
      }
    } yield result
  }

  /** Get result docs and map to entity.
    *
    * @param filterQuery fo execute
    * @param docMapper for result types
    * @tparam A type of result
    * @return vector containing results or error
    */
  private[solrimpl] def getListResults[A](filterQuery: FilterQuery)(
      implicit docMapper: FromSolrDoc[A]): Try[Vector[A]] = {
    val results = for {
      query <- mapper.buildFilterQuery(this, filterQuery)
      () = docMapper.getSolrFields.foreach(f => query.addField(f.name))
      solrRes <- getSolrResult(query)
    } yield solrRes.getResults.asScala.map(docMapper.fromSolrDoc)

    (results: Try[Seq[A]]).map(_.toVector)
  }

  override def getResults(filterQuery: FilterQuery): Try[Vector[BlockEt]] = {
    getListResults(filterQuery)(FromSolrDoc[BlockEt])
  }

  override def getShortResults(filterQuery: FilterQuery): Try[Vector[ShortEt]] = {
    getListResults(filterQuery)(FromSolrDoc[ShortEt])
  }
}
