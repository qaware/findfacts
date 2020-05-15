package de.qaware.findfacts.core.solrimpl

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._
import com.typesafe.scalalogging.Logger
import org.apache.solr.client.solrj.SolrRequest.METHOD
import org.apache.solr.client.solrj.request.json.JsonQueryRequest
import org.apache.solr.client.solrj.response.QueryResponse
import org.apache.solr.client.solrj.{SolrQuery, SolrServerException}

import de.qaware.findfacts.common.dt.EtField.Uses
import de.qaware.findfacts.common.dt.{BaseEt, CodeblockEt, ConstantEt, EtField, FactEt, TypeEt}
import de.qaware.findfacts.common.solr.SolrRepository
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.core.QueryService.{FacetResult, ResultList}
import de.qaware.findfacts.core._
import de.qaware.findfacts.core.dt.{ResolvedThyEt, ShortBlock, ShortThyEt}

/**
 * Solr impl of the query service.
 *
 * @param solr solr instance
 * @param mapper solr query mapper
 */
class SolrQueryService(solr: SolrRepository, mapper: SolrQueryMapper) extends QueryService {

  private val logger = Logger[SolrQueryService]

  /** Make this implicitly available. */
  private implicit val queryService: SolrQueryService = this

  /** Get result from solr */
  private def getSolrResult(query: Either[SolrQuery, JsonQueryRequest])(implicit index: String): Try[QueryResponse] = {
    Try {
      logger.info(s"Executing query $query")

      val resp = query match {
        case Left(query) => solr.query(index, query, METHOD.POST)
        case Right(query) => query.process(solr, index)
      }

      if (resp.getStatus != 0 && resp.getStatus != 200) {
        throw new IllegalStateException(s"Query status was not ok: $resp")
      }
      logger.info(s"Found ${resp.getResults.getNumFound} entries")
      resp
    }
  }

  private def mapSingle[A](resp: QueryResponse, typed: Seq[A]): Option[A] =
    typed match {
      case Seq(elem) => Some(elem)
      case _ => None
    }

  private def mapResults[A](resp: QueryResponse, typed: Seq[A]): Vector[A] = typed.toVector

  private def mapResultList[A](resp: QueryResponse, typed: Seq[A]): ResultList[A] = {
    ResultList(typed.toVector, resp.getResults.getNumFound, resp.getNextCursorMark)
  }

  /**
   * Generic method to get results.
   *
   * @param query     to find results for
   * @param qBuilder  to build solr query
   * @param rMapper   to map typed results to desired type
   * @param index     to query in
   * @param docMapper to map solr docs to results
   * @tparam A type of result entities
   * @tparam B result type
   * @return container with results
   */
  private def getRes[A, B](
      query: FilterQuery,
      qBuilder: FilterQuery => Try[SolrQuery],
      rMapper: (QueryResponse, Seq[A]) => B)(implicit index: String, docMapper: FromSolrDoc[A]): Try[B] = {
    if (query.pageSize < 0) {
      return Failure(new IllegalArgumentException("Page size cannot be negative"))
    }

    for {
      query <- qBuilder(query)
      () = docMapper.getSolrFields.foreach(f => query.addField(f.name))
      res <- getSolrResult(Left(query))
      typedRes <- res.getResults.asScala.map(docMapper.fromSolrDoc).toList.sequence
    } yield rMapper(res, typedRes)
  }

  /**
   * Get result elements for a query. Does not resolve parent/child relation.
   *
   * @param query to execute
   * @param index to query in
   * @param docMapper to map solr docs to results
   * @tparam A type of result entities
   * @return result list, or error
   */
  def getResults[A](query: FilterQuery)(implicit index: String, docMapper: FromSolrDoc[A]): Try[ResultList[A]] =
    getRes[A, ResultList[A]](query, mapper.buildFilterQuery, mapResultList)

  /**
   * Get result blocks for a query.
   *
   * @param query to execute
   * @param index to query in
   * @param docMapper to map solr docs to results
   * @tparam A type of blocks
   * @return result list of blocks, or error
   */
  def getResultBlocks[A](query: FilterQuery)(implicit index: String, docMapper: FromSolrDoc[A]): Try[ResultList[A]] =
    getRes[A, ResultList[A]](query, mapper.buildBlockFilterQuery, mapResultList)

  private def idsFilterQuery(ids: EtField.Id.T*): FilterQuery = {
    val idFilters = ids.toList match {
      case List(t) => Exact(t)
      case t1 :: t2 :: tn => Or(Exact(t1), Exact(t2), tn.map(Exact(_)): _*)
    }
    FilterQuery(List(FieldFilter(EtField.Id, idFilters)), ids.size)
  }

  override def getResultFacet(facetQuery: FacetQuery)(implicit index: String): Try[FacetResult] = {
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

  override def getResultResolved(id: EtField.Id.T)(implicit index: String): Try[Option[ResolvedThyEt]] = {
    val res: Try[Option[Try[ResolvedThyEt]]] = for {
      resOpt <- getRes[BaseEt, Option[BaseEt]](idsFilterQuery(id), mapper.buildFilterQuery, mapSingle)
    } yield for {
      elem <- resOpt
    } yield elem match {
      case ConstantEt(id, _, uses, constantType, _) =>
        for { resolved <- resolve(uses) } yield dt.ResolvedConstant(id, constantType, resolved.toList)
      case FactEt(id, _, uses, _) =>
        for { resolved <- resolve(uses) } yield dt.ResolvedFact(id, resolved.toList)
      case TypeEt(id, _, uses, _) =>
        for { resolved <- resolve(uses) } yield dt.ResolvedType(id, resolved.toList)
      case _ => return Success(None)
    }
    res.map(_.map(_.toEither.left.map(t => return Failure(t)).merge))
  }

  private def resolve(uses: Uses.T)(implicit index: String) = {
    if (uses.isEmpty) {
      Success(List.empty)
    } else {
      getRes[ShortThyEt, Vector[ShortThyEt]](
        idsFilterQuery(uses.map(x => EtField.Id(x)): _*),
        mapper.buildFilterQuery,
        mapResults)
    }
  }

  override def getBlock(id: EtField.Id.T)(implicit index: String): Try[Option[CodeblockEt]] = {
    val res =
      getRes[CodeblockEt, Option[CodeblockEt]](idsFilterQuery(id), mapper.buildBlockFilterQuery, mapSingle)
    if (res.map(_.isEmpty).getOrElse(false)) {
      // Try to fetch child doc on empty result
      getRes[CodeblockEt, Option[CodeblockEt]](
        FilterQuery(List(FieldFilter(EtField.ChildId, Exact(id))), 2),
        mapper.buildBlockFilterQuery,
        mapSingle)
    } else {
      res
    }
  }

  override def getShortBlock(id: EtField.Id.T)(implicit index: String): Try[Option[ShortBlock]] = {
    val res = getRes[ShortBlock, Option[ShortBlock]](idsFilterQuery(id), mapper.buildBlockFilterQuery, mapSingle)
    if (res.map(_.isEmpty).getOrElse(false)) {
      // Try to fetch child doc on empty result
      getRes[ShortBlock, Option[ShortBlock]](
        FilterQuery(List(FieldFilter(EtField.ChildId, Exact(id))), 2),
        mapper.buildBlockFilterQuery,
        mapSingle)
    } else {
      res
    }
  }

  override def getResultShortlist(filterQuery: FilterQuery)(implicit index: String): Try[ResultList[ShortBlock]] = {
    getRes[ShortBlock, ResultList[ShortBlock]](filterQuery, mapper.buildBlockFilterQuery, mapResultList)
  }

  override def listIndexes: Try[List[String]] =
    Try {
      val elems = solr.listIndexes
      val defaultIdx = elems.indexWhere(_.contains(SolrQueryService.DEFAULT_INDEX_PREFIX))
      if (defaultIdx >= 0) {
        // If there is an index with name "default", put it first in the results.
        elems(defaultIdx) :: elems.take(defaultIdx) ++ elems.drop(defaultIdx + 1)
      } else {
        elems
      }
    }
}

/** Companion object. */
object SolrQueryService {

  /** Prefix for the name of the default index. */
  final val DEFAULT_INDEX_PREFIX = "default"
}
