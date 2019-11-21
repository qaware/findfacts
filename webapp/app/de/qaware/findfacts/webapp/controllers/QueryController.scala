package de.qaware.findfacts.webapp.controllers

import scala.language.postfixOps

import com.google.inject.{Inject, Singleton}
import com.typesafe.scalalogging.Logger
import play.api.libs.json.{JsError, JsSuccess, Json, Reads}
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents, Request, Result}

sealed trait Query
final case class Kind(of: String) extends Query
final case class Not(query: Query) extends Query

/** Controller for the query api.
  *
  * @param cc injected components
  */
@Singleton class QueryController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {
  private val logger = Logger[QueryController]

  implicit val notReads: Reads[Not] = Json.reads[Not]
  implicit val kindReads: Reads[Kind] = Json.reads[Kind]
  implicit val queryReads: Reads[Query] = Json.reads[Query]

  protected def parseQuery(request: Request[AnyContent]): Either[Result, Query] = {
    request.body.asJson match {
      case None => Left(BadRequest("Expecting application/json request body"))
      case Some(json) =>
        logger.info(s"Received request: $json")

        Json.fromJson[Query](json) match {
          case JsSuccess(query, _) => Right(query)
          case JsError(errors) => Left(BadRequest(s"Could not parse query: $errors"))
        }
    }
  }

  protected def decode(encodedQuery: String): Either[Result, Query] = ???

  protected def executeQuery(query: Query): Result = {
    // TODO fire query against solr
    Ok(Json.toJson(List("id1", "id2")))
  }

  def search = Action { implicit request: Request[AnyContent] =>
    parseQuery(request).right.map(executeQuery).merge
  }

  def encode = Action { implicit request: Request[AnyContent] =>
    parseQuery(request).right map { query =>
      // TODO encode and compress
      Ok(query.toString)
    } merge
  }

  def searchEncoded(encodedQuery: String) = Action { implicit request: Request[AnyContent] =>
    decode(encodedQuery).right.map(executeQuery).merge
  }

  def entity(id: String) = TODO
}
