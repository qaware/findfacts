package de.qaware.findfacts.core.solrimpl

import scala.collection.JavaConverters._
import scala.language.{postfixOps, reflectiveCalls}
import scala.util.{Failure, Success, Try}

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.{BaseEt, ConstantEt, EtField, FactEt, TypeEt}
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.common.utils.TryUtils._
import de.qaware.findfacts.core.QueryService.{FacetResult, ResultList}
import de.qaware.findfacts.core.dt.{ResolvedConstant, ResolvedFact, ResolvedThyEt, ResolvedType, ShortCmd, ShortThyEt}
import de.qaware.findfacts.core.{FacetQuery, FilterQuery, QueryService}
import org.apache.solr.client.solrj
import org.apache.solr.client.solrj.SolrRequest.METHOD
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
        case Left(query) => connection.query(query, METHOD.POST)
        case Right(query) => query.process(connection)
      }

      if (resp.getStatus != 0 && resp.getStatus != 200) {
        throw new IllegalStateException(s"Query status was not ok: $resp")
      }
      logger.info(s"Found ${resp.getResults.getNumFound} entries")
      resp
    }
  }

  override def getResultFacet(facetQuery: FacetQuery): Try[FacetResult] = {
    if (facetQuery.maxFacets < 0) {
      return Failure(new IllegalArgumentException("Maximum number of facets cannot be negative"))
    }

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
      res.filter(_._2.size < facetQuery.maxFacets).toMap
    }
  }

  /** Get result docs and map to entity.
    *
    * @param filterQuery fo execute
    * @param docMapper for result types
    * @tparam A type of result
    * @return vector containing results, result count, and next cursor or error
    */
  def getResultList[A](filterQuery: FilterQuery)(implicit docMapper: FromSolrDoc[A]): Try[ResultList[A]] = {
    if (filterQuery.pageSize < 0) {
      return Failure(new IllegalArgumentException("Page size cannot be negative"))
    }

    val results = for {
      query <- mapper.buildFilterQuery(this, filterQuery)
      () = docMapper.getSolrFields.foreach(f => query.addField(f.name))
      solrRes <- getSolrResult(Left(query))
    } yield {
      val resList: Try[Seq[A]] = solrRes.getResults.asScala.map(docMapper.fromSolrDoc)
      resList.map(r => ResultList(r.toVector, solrRes.getResults.getNumFound, solrRes.getNextCursorMark))
    }
    results.flatten
  }

  /** Get single doc by id and map to entity.
    *
    * @param query solr query to search for
    * @param docMapper for result type
    * @tparam A type of result
    * @return option containing result if found
    */
  private def getResult[A](query: solrj.SolrQuery)(implicit docMapper: FromSolrDoc[A]): Try[Vector[A]] = {
    docMapper.getSolrFields.foreach(f => query.addField(f.name))
    val res: Try[Try[Seq[A]]] = for {
      solrRes <- getSolrResult(Left(query))
    } yield solrRes.getResults.asScala.map(docMapper.fromSolrDoc)

    res.flatten.map(_.toVector)
  }

  override def getResultResolved(id: EtField.Id.T): Try[Option[ResolvedThyEt]] = {
    val res: Try[Option[Try[ResolvedThyEt]]] = for {
      resOpt <- getResult(id)
    } yield
      for {
        res <- resOpt
      } yield
        res match {
          case ConstantEt(id, _, _, propositionUses, typeUses, constantType) =>
            for {
              propUsesRes <- getResult[ShortThyEt](mapper.buildSingleQuery(propositionUses.mkString(" ")))
              typeUsesRes <- getResult[ShortThyEt](mapper.buildSingleQuery(typeUses.mkString(" ")))
            } yield ResolvedConstant(id, constantType, typeUsesRes.toList, propUsesRes.toList)
          case FactEt(id, _, _, propositionUses, proofUses) =>
            for {
              propUsesRes <- getResult[ShortThyEt](mapper.buildSingleQuery(propositionUses.mkString(" ")))
              proofUsesRes <- getResult[ShortThyEt](mapper.buildSingleQuery(proofUses.mkString(" ")))
            } yield ResolvedFact(id, propUsesRes.toList, proofUsesRes.toList)
          case TypeEt(id, _, _, propositionUses) =>
            for {
              propUsesRes <- getResult[ShortThyEt](mapper.buildSingleQuery(propositionUses.mkString(" ")))
            } yield ResolvedType(id, propUsesRes.toList)
          case _ => return Success(None)
        }
    res.map(_.map(_.toEither.left.map(t => return Failure(t)).merge))
  }

  override def getResult(id: EtField.Id.T): Try[Option[BaseEt]] =
    getResult[BaseEt](mapper.buildSingleQuery(id)) map {
      case Vector(elem) => Some(elem)
      case _ => None
    }

  override def getShortResult(id: EtField.Id.T): Try[Option[ShortCmd]] = {
    getResult[ShortCmd](mapper.buildSingleQuery(id)) map {
      case Vector(elem) => Some(elem)
      case _ => None
    }
  }

  override def getResultShortlist(filterQuery: FilterQuery): Try[ResultList[ShortCmd]] =
    getResultList[ShortCmd](filterQuery)
}
