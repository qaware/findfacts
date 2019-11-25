package de.qaware.findfacts.webapp.controllers

import scala.language.postfixOps

import com.google.inject.{Inject, Singleton}
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.webapp.model.ResultEntity
import io.swagger.annotations.{
  Api,
  ApiImplicitParam,
  ApiImplicitParams,
  ApiOperation,
  ApiParam,
  ApiResponse,
  ApiResponses
}
import play.api.libs.json.{JsError, JsSuccess, Json, Reads}
import play.api.mvc.{AbstractController, AnyContent, ControllerComponents, Request, Result}

sealed trait Query
final case class Kind(of: String) extends Query
final case class Not(query: Query) extends Query

/** Controller for the query api.
  *
  * @param cc injected components
  */
@Api(value = "/")
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

  @ApiOperation(
    value = "Search query",
    notes = "Accepts a search query and returns list of all results.",
    response = classOf[ResultEntity],
    responseContainer = "List",
    httpMethod = "POST"
  )
  @ApiResponses(Array(new ApiResponse(code = 400, message = "Invalid Query")))
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "query",
        value = "Query object",
        required = true,
        dataTypeClass = classOf[Query],
        paramType = "body"
      )))
  def search = Action { implicit request: Request[AnyContent] =>
    parseQuery(request).right.map(executeQuery).merge
  }

  @ApiOperation(
    value = "Encode query as url",
    notes = "Encodes query as url for permalinks.",
    response = classOf[String],
    httpMethod = "POST"
  )
  @ApiResponses(Array(new ApiResponse(code = 400, message = "Invalid Query")))
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "query",
        value = "Query object",
        required = true,
        dataTypeClass = classOf[Query],
        paramType = "body"
      )))
  def encode = Action { implicit request: Request[AnyContent] =>
    parseQuery(request).right map { query =>
      // TODO encode and compress
      Ok(query.toString)
    } merge
  }

  @ApiOperation(
    value = "Executes url-encoded query",
    notes = "Decodes query-url and executes query",
    response = classOf[ResultEntity],
    responseContainer = "List",
    httpMethod = "GET"
  )
  def searchEncoded(@ApiParam(value = "Encoded query", required = true) encodedQuery: String) = Action {
    implicit request: Request[AnyContent] =>
      decode(encodedQuery).right.map(executeQuery).merge
  }

  @ApiOperation(
    value = "Retrieves a single entity",
    notes = "Retrieves information about a single entity",
    response = classOf[ResultEntity],
    httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 404, message = "Entity not found")))
  def entity(@ApiParam(value = "ID of result entity to fetch", required = true) id: String) = TODO
}
