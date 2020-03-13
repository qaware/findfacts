package de.qaware.findfacts.core.solrimpl

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.{BaseEt, CodeblockEt, ConstantEt, EtField, FactEt, TypeEt}
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.common.utils.TryUtils._
import de.qaware.findfacts.core.QueryService.{FacetResult, ResultList}
import de.qaware.findfacts.core.dt.{ResolvedConstant, ResolvedFact, ResolvedThyEt, ResolvedType, ShortBlock, ShortThyEt}
import de.qaware.findfacts.core.{Exact, FacetQuery, FieldFilter, FilterQuery, Or, QueryService}
import org.apache.solr.client.solrj
import org.apache.solr.client.solrj.SolrRequest.METHOD
import org.apache.solr.client.solrj.request.json.JsonQueryRequest
import org.apache.solr.client.solrj.response.QueryResponse
import org.apache.solr.client.solrj.{SolrClient, SolrServerException}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

/** Solr impl of the query service.
  *
  * @param connection solr instance
  * @param mapper solr query mapper
  */
class SolrQueryService(connection: SolrClient, mapper: SolrQueryMapper) extends QueryService {
  private val logger = Logger[SolrQueryService]

  /** Make this implicitly available. */
  private implicit val queryService: SolrQueryService = this

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

  private def mapSingle[A](resp: QueryResponse, typed: Seq[A]): Option[A] = typed match {
    case Seq(elem) => Some(elem)
    case elems => None
  }

  private def mapResults[A](resp: QueryResponse, typed: Seq[A]): Vector[A] = typed.toVector

  private def mapResultList[A](resp: QueryResponse, typed: Seq[A]): ResultList[A] = {
    ResultList(typed.toVector, resp.getResults.getNumFound, resp.getNextCursorMark)
  }

  /** Generic method to get results.
    *
    * @param query to find results for
    * @param qBuilder to build solr query
    * @param rMapper to map typed results to desired type
    * @param docMapper to map solr docs to results
    * @tparam A type of result entities
    * @tparam B result type
    * @return container with results
    */
  private def getResults[A, B](
      query: FilterQuery,
      qBuilder: FilterQuery => Try[solrj.SolrQuery],
      rMapper: (QueryResponse, Seq[A]) => B)(implicit docMapper: FromSolrDoc[A]): Try[B] = {
    if (query.pageSize < 0) {
      return Failure(new IllegalArgumentException("Page size cannot be negative"))
    }

    for {
      query <- qBuilder(query)
      () = docMapper.getSolrFields.foreach(f => query.addField(f.name))
      res <- getSolrResult(Left(query))
      typedRes <- tryFailFirst(res.getResults.asScala.map(docMapper.fromSolrDoc))
    } yield rMapper(res, typedRes)
  }

  private[solrimpl] def getResultVector[A](query: FilterQuery)(implicit docMapper: FromSolrDoc[A]): Try[Vector[A]] =
    getResults[A, Vector[A]](query, mapper.buildFilterQuery, mapResults)

  private def idsFilterQuery(ids: EtField.Id.T*): FilterQuery = {
    val idFilters = ids.toList match {
      case List(t) => Exact(t)
      case t1 :: t2 :: tn => Or(Exact(t1), Exact(t2), tn.map(Exact(_)): _*)
    }
    FilterQuery(List(FieldFilter(EtField.Id, idFilters)), ids.size)
  }

  override def getResultFacet(facetQuery: FacetQuery): Try[FacetResult] = {
    if (facetQuery.maxFacets < 0) {
      return Failure(new IllegalArgumentException("Maximum number of facets cannot be negative"))
    }
    if (facetQuery.fields.isEmpty) {
      return Success(Map())
    }
    // Faceting on ID field does not make any sense,
    // and won't work properly because it's a field of both parent and children
    if (facetQuery.fields.contains(EtField.Id)) {
      return Failure(new IllegalArgumentException("Cannot facet on id field"))
    }

    for {
      solrQuery <- mapper.buildBlockFacetQuery(facetQuery)
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

  override def getResultResolved(id: EtField.Id.T): Try[Option[ResolvedThyEt]] = {
    val res: Try[Option[Try[ResolvedThyEt]]] = for {
      resOpt <- getResults[BaseEt, Option[BaseEt]](idsFilterQuery(id), mapper.buildFilterQuery, mapSingle)
    } yield
      for {
        elem <- resOpt
      } yield
        elem match {
          case ConstantEt(id, _, uses, constantType, _) =>
            for {
              resolved <- getResults[ShortThyEt, Vector[ShortThyEt]](
                idsFilterQuery(uses.map(x => EtField.Id(x)): _*),
                mapper.buildFilterQuery,
                mapResults)
            } yield ResolvedConstant(id, constantType, resolved.toList)
          case FactEt(id, _, uses, _) =>
            for {
              resolved <- getResults[ShortThyEt, Vector[ShortThyEt]](
                idsFilterQuery(uses.map(x => EtField.Id(x)): _*),
                mapper.buildFilterQuery,
                mapResults)
            } yield ResolvedFact(id, resolved.toList)
          case TypeEt(id, _, uses, _) =>
            for {
              resolved <- getResults[ShortThyEt, Vector[ShortThyEt]](
                idsFilterQuery(uses.map(x => EtField.Id(x)): _*),
                mapper.buildFilterQuery,
                mapResults)
            } yield ResolvedType(id, resolved.toList)
          case _ => return Success(None)
        }
    res.map(_.map(_.toEither.left.map(t => return Failure(t)).merge))
  }

  override def getBlock(id: EtField.Id.T): Try[Option[CodeblockEt]] = {
    val res = getResults[CodeblockEt, Option[CodeblockEt]](idsFilterQuery(id), mapper.buildBlockFilterQuery, mapSingle)
    if (res.map(_.isEmpty).getOrElse(false)) {
      // Try to fetch child doc on empty result
      getResults[CodeblockEt, Option[CodeblockEt]](
        FilterQuery(List(FieldFilter(EtField.ChildId, Exact(id))), 2),
        mapper.buildBlockFilterQuery,
        mapSingle)
    } else {
      res
    }
  }

  override def getShortBlock(id: EtField.Id.T): Try[Option[ShortBlock]] = {
    val res = getResults[ShortBlock, Option[ShortBlock]](idsFilterQuery(id), mapper.buildBlockFilterQuery, mapSingle)
    if (res.map(_.isEmpty).getOrElse(false)) {
      // Try to fetch child doc on empty result
      getResults[ShortBlock, Option[ShortBlock]](
        FilterQuery(List(FieldFilter(EtField.ChildId, Exact(id))), 2),
        mapper.buildBlockFilterQuery,
        mapSingle)
    } else {
      res
    }
  }

  override def getResultShortlist(filterQuery: FilterQuery): Try[ResultList[ShortBlock]] = {
    getResults[ShortBlock, ResultList[ShortBlock]](filterQuery, mapper.buildBlockFilterQuery, mapResultList)
  }
}
