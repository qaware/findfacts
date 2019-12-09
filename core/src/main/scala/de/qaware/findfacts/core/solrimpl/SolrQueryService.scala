package de.qaware.findfacts.core.solrimpl

import scala.collection.JavaConverters._
import scala.language.{postfixOps, reflectiveCalls}
import scala.util.Try

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.EtFields.Kind
import de.qaware.findfacts.common.dt.{BaseEt, ConstantEt, DocumentationEt, EtKind, FactEt, TypeEt}
import de.qaware.findfacts.common.solr.{SolrRepository, SolrSchema}
import de.qaware.findfacts.core.{FacetQuery, FilterQuery, Query, QueryService}
import de.qaware.findfacts.scala.Using
import org.apache.solr.client.solrj.response.QueryResponse

/** Solr impl of the query service.
  *
  * @param connection solr instance
  * @param mapper solr query mapper
  */
class SolrQueryService(connection: SolrRepository, mapper: SolrQueryMapper) extends QueryService {
  private val logger = Logger[SolrQueryService]

  private def extractResults(query: Query, response: QueryResponse): Try[query.Result] = query match {
    case FacetQuery(_, field) =>
      Try {
        response
          .getFacetField(SolrSchema.getFieldName(field))
          .getValues
          .asScala
          .groupBy(e => field.implicits.fromString(e.getName).get)
          .mapValues(_.head.getCount)
          .asInstanceOf[query.Result]
      }
    case FilterQuery(_, _) =>
      val entities = response.getResults.asScala map { solrDoc =>
        for {
          kind <- EtKind.fromString(solrDoc.getFieldValue(SolrSchema.getFieldName(Kind)).toString)
          etMapper = kind match {
            case EtKind.Constant => SolrDocMapper[ConstantEt]
            case EtKind.Documentation => SolrDocMapper[DocumentationEt]
            case EtKind.Fact => SolrDocMapper[FactEt]
            case EtKind.Type => SolrDocMapper[TypeEt]
          }
          et <- etMapper.mapSolrDocument(solrDoc)
        } yield et
      }
      response.getNextCursorMark
      Try { entities.map(_.get).toVector.asInstanceOf[query.Result] }
  }

  def getResults(query: Query): Try[query.Result] = {
    val res = for {
      solrQuery <- mapper.buildQuery(this, query)
      solrResult <- Using(connection.solrConnection()) { solr =>
        logger.info(s"Executing query $solrQuery")
        val resp = solr.query(solrQuery)
        if (resp.getStatus != 0 && resp.getStatus != 200) {
          throw new IllegalStateException(s"Query status was not ok: $resp")
        }
        resp
      }
      results <- extractResults(query, solrResult)
    } yield results

    res.failed.foreach(logger.error(s"Error executing query: ", _))

    res
  }

  override def getFacetResults[A](facetQuery: FacetQuery): Try[Map[A, Long]] =
    getResults(facetQuery).asInstanceOf[Try[Map[A, Long]]]
  override def getResults(filterQuery: FilterQuery): Try[Vector[BaseEt]] = getResults(filterQuery)
}
