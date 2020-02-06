package de.qaware.findfacts.webapp.controllers

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.{BaseEt, EtField, ShortCmdEt}
import de.qaware.findfacts.core.{FacetQuery, Filter, FilterQuery, QueryService, Term}
import de.qaware.findfacts.webapp.utils.JsonMappings

// scalastyle:off
import io.circe.syntax._
// scalastyle:on
import io.circe.{Json, Printer}
import io.swagger.annotations.{
  Api,
  ApiImplicitParam,
  ApiImplicitParams,
  ApiOperation,
  ApiParam,
  ApiResponse,
  ApiResponses,
  Example,
  ExampleProperty
}
import play.api.libs.circe.Circe
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents, Request, Result}

/** Controller for the query api.
  *
  * @param cc injected components
  * @param queryService query service component
  * @param jsonMappings component to map queries and results to and from json
  */
@Api(value = "/")
class QueryController(cc: ControllerComponents, queryService: QueryService, jsonMappings: JsonMappings)
    extends AbstractController(cc)
    with Circe {
  // Import all json mapping implicits
  import jsonMappings._

  // scalastyle:scaladoc off
  // scalastyle:magic.number off
  private final val ExampleFilterQuery =
    """
{
  "filter": {
    "Filter": {
      "fieldTerms": {
        "Name": {
          "Term": {
            "inner": "*gauss*"
          }
        }
      }
    }
  },
  "maxResults": 10
}
"""
  private final val ExampleFacetQuery = """
{
  "filter" : {
    "Filter" : {
      "fieldTerms" : {
        "Name" : {
          "Term" : {
            "inner" : "*gauss*"
          }
        }
      }
    }
  },
  "fields" : [ "TheoryEtKind", "Name" ],
  "maxFacets": 10
}
"""
  private final val InternalErrorMsg = "Internal server error"
  private final val NotFoundMsg = "Entity not found"

  private val logger = Logger[QueryController]

  // Json printer defines output format
  implicit val jsonPrinter: Printer = Printer.noSpacesSortKeys.copy(dropNullValues = true)

  protected def executeFilterQuery(query: FilterQuery): Result =
    logIfErr(query.toString, queryService.getShortResults(query).map(_.toList.asJson))

  protected def executeFacetQuery(query: FacetQuery): Result =
    logIfErr(query.toString, queryService.getFacetResults(query).map(_.asJson))

  protected def logIfErr(query: String, json: Try[Json]): Result = json match {
    case Failure(exception) =>
      logger.error(s"Error executing query $query", exception)
      InternalServerError(InternalErrorMsg)
    case Success(value) => Ok(value)
  }

  @ApiOperation(
    value = "Search query",
    notes = "Accepts a search query and returns list of all results.",
    response = classOf[ShortCmdEt],
    responseContainer = "List",
    httpMethod = "POST"
  )
  @ApiResponses(Array(new ApiResponse(code = 400, message = "Invalid Query")))
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "filterQuery",
        value = "filter query object",
        required = true,
        paramType = "body",
        dataType = "de.qaware.findfacts.core.FilterQuery",
        examples = new Example(value = Array(new ExampleProperty(mediaType = "default", value = ExampleFilterQuery)))
      )))
  def search: Action[FilterQuery] = Action(circe.json[FilterQuery]) { implicit request: Request[FilterQuery] =>
    executeFilterQuery(request.body)
  }

  @ApiOperation(
    value = "Retrieves a single entity",
    notes = "Retrieves information about a single entity",
    response = classOf[BaseEt],
    httpMethod = "GET")
  @ApiResponses(Array(new ApiResponse(code = 400, message = NotFoundMsg)))
  def entity(@ApiParam(value = "ID of result entity to fetch", required = true) id: String): Action[AnyContent] =
    Action { implicit request: Request[AnyContent] =>
      val singleQuery = FilterQuery(Filter(Map(EtField.Id -> Term(id))), 2)
      queryService.getResults(singleQuery) match {
        case Failure(exception) =>
          logger.error(s"Error executing id query: $exception")
          InternalServerError(InternalErrorMsg)
        case Success(Vector(single)) => Ok(single.asJson)
        case Success(values) =>
          logger.error(s"Did not receive single value for id $id: ${values.mkString(",")}")
          BadRequest(NotFoundMsg)
      }
    }

  @ApiOperation(
    value = "Facet query",
    notes = "Executes a facet query and returns faceted results.",
    response = classOf[Map[_, _]],
    responseContainer = "Map",
    httpMethod = "POST"
  )
  @ApiResponses(Array(new ApiResponse(code = 400, message = "Invalid Query")))
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "facetQuery",
        value = "facet query object",
        required = true,
        paramType = "body",
        dataType = "de.qaware.findfacts.core.FacetQuery",
        examples = new Example(value = Array(new ExampleProperty(mediaType = "default", value = ExampleFacetQuery)))
      )
    )
  )
  def facet: Action[FacetQuery] = Action(circe.json[FacetQuery]) { implicit request: Request[FacetQuery] =>
    executeFacetQuery(request.body)
  }
}
