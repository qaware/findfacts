package de.qaware.findfacts.core.solrimpl

import de.qaware.findfacts.common.dt.EtFields.Kind
import de.qaware.findfacts.common.dt.{ConstantEt, DocumentationEt, EtKind, FactEt, TypeEt}
import de.qaware.findfacts.common.solr.{SolrRepository, SolrSchema}
import de.qaware.findfacts.core.{FacetQuery, FilterQuery, Query, QueryService}
import de.qaware.findfacts.scalautils.Using
import org.apache.solr.client.solrj.SolrServerException
import org.apache.solr.client.solrj.response.QueryResponse

import scala.collection.JavaConverters._
import scala.language.{postfixOps, reflectiveCalls}

/** Solr impl of the query service.
  *
  * @param connection solr instance
  * @param mapper solr query mapper
  */
class SolrQueryService(connection: SolrRepository, mapper: SolrQueryMapper) extends QueryService {
  private def extractResults(query: Query, response: QueryResponse): query.Result = query match {
    case FacetQuery(_, field) =>
      response
        .getFacetField(field.toString)
        .getValues
        .asScala
        .groupBy(e => field.fromString(e.getName))
        .mapValues(_.head.getCount)
        .asInstanceOf[query.Result]
    case FilterQuery(_) =>
      (response.getResults.asScala map { doc =>
        val kind: EtKind.Value = EtKind.fromString(doc.getFieldValue(SolrSchema.getFieldName(Kind)).toString)
        // Decide on kind which mapper to use
        val mapper = kind match {
          case EtKind.Constant => SolrDocMapper[ConstantEt]
          case EtKind.Documentation => SolrDocMapper[DocumentationEt]
          case EtKind.Fact => SolrDocMapper[FactEt]
          case EtKind.Type => SolrDocMapper[TypeEt]
        }
        mapper.mapSolrDocument(doc)
      } toVector).asInstanceOf[query.Result]
  }

  override def getResults(query: Query): Either[Throwable, query.Result] = {
    mapper.buildQuery(this, query) match {
      case Left(error) => Left(error)
      case Right(solrQuery) =>
        Using(connection.solrConnection()) { solr =>
          solr.query(solrQuery)
        } toEither match {
          case Left(e) => Left(e)
          case Right(resp) =>
            if (resp.getStatus != 0 && resp.getStatus != 200) {
              Left(new SolrServerException(s"Status was not ok: $resp"))
            } else {
              Right(extractResults(query, resp))
            }
        }
    }
  }
}
