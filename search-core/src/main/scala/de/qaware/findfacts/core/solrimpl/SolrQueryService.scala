package de.qaware.findfacts.core.solrimpl

import scala.collection.JavaConverters._
import scala.language.{postfixOps, reflectiveCalls}
import scala.util.{Failure, Try}

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.{CodeblockEt, EtField, ShortCmdEt}
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.common.utils.TryUtils.flattenTryFailFirst
import de.qaware.findfacts.core.{FacetQuery, FilterQuery, QueryService}
import org.apache.solr.client.solrj
import org.apache.solr.client.solrj.request.json.JsonQueryRequest
import org.apache.solr.client.solrj.response.QueryResponse
import org.apache.solr.client.solrj.{SolrClient, SolrServerException}

/** Solr impl of the query service.
  *
  * @param connection solr instance
  * @param mapper solr query mapper
  */
class SolrQueryService(connection: SolrClient, mapper: SolrQueryMapper) extends QueryService {
  private val logger = Logger[SolrQueryService]

  /** Get result from solr */
  private def getSolrResult(query: Either[solrj.SolrQuery, JsonQueryRequest]): Try[QueryResponse] = {
    Try {
      logger.info(s"Executing query $query")

      val resp = query match {
        case Left(query) => connection.query(query)
        case Right(query) => query.process(connection)
      }

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
      solrResult <- getSolrResult(Right(solrQuery))
    } yield {
      // Handle null values in response / for facets
      if (solrResult == null) {
        return Failure(new SolrServerException("No response"))
      }
      if (solrResult.getJsonFacetingResponse == null) {
        return Failure(new SolrServerException("Empty facet in response"))
      }

      val res = facetQuery.fields.map { field =>
        val facetField = solrResult.getJsonFacetingResponse.getBucketBasedFacets(field.name)

        if (facetField == null) {
          // Null indicates zero facet values for field here
          field -> Map.empty[String, Long]
        } else {
          if (facetField.getBuckets == null) {
            return Failure(new SolrServerException("Faceting does not contain buckets!"))
          }
          field -> facetField.getBuckets.asScala.map(bucket => bucket.getVal.toString -> bucket.getCount).toMap
        }
      }
      res.toMap
    }
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
      solrRes <- getSolrResult(Left(query))
    } yield solrRes.getResults.asScala.map(docMapper.fromSolrDoc)

    (results: Try[Seq[A]]).map(_.toVector)
  }

  override def getResults(filterQuery: FilterQuery): Try[Vector[CodeblockEt]] = {
    getListResults(filterQuery)(FromSolrDoc[CodeblockEt])
  }

  override def getShortResults(filterQuery: FilterQuery): Try[Vector[ShortCmdEt]] = {
    getListResults(filterQuery)(FromSolrDoc[ShortCmdEt])
  }
}
