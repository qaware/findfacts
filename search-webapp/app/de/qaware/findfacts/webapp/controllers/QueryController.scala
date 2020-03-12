package de.qaware.findfacts.webapp.controllers

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.BaseEt
import de.qaware.findfacts.core.QueryService.{FacetResult, ResultList}
import de.qaware.findfacts.core.dt.{ResolvedThyEt, ShortBlock}
import de.qaware.findfacts.core.{FacetQuery, FilterQuery, QueryService}
import de.qaware.findfacts.webapp.utils.JsonMappings

// scalastyle:off
import io.circe.syntax._
// scalastyle:on
import io.circe.Printer
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
  "filters" : [
    {
      "field" : "Name",
      "filter" : {
        "Exact" : {
          "inner" : "*gauss*"
        }
      }
    }
  ],
  "pageSize" : 10
}
"""
  private final val ExampleFacetQuery = """
{
  "filters" : [
    {
      "field" : "Name",
      "filter" : {
        "Exact" : {
          "inner" : "*gauss*"
        }
      }
    }
  ],
  "fields" : [ "Kind", "NameFacet", "SourceTheory" ],
  "maxFacets": 10
}
"""
  private final val InternalErrorMsg = "Internal server error"
  private final val NotFoundMsg = "Entity not found"

  private val logger = Logger[QueryController]

  // Json printer defines output format
  implicit val jsonPrinter: Printer = Printer.noSpacesSortKeys.copy(dropNullValues = true)

  protected def logQueryErr[A](query: String, value: Try[A], onSuccess: A => Result): Result = value match {
    case Failure(exception) =>
      logger.error(s"Error executing query $query", exception)
      InternalServerError(InternalErrorMsg)
    case Success(a) => onSuccess(a)
  }

  @ApiOperation(
    value = "SearchComponent query",
    notes = "Accepts a search query and returns list of all results.",
    response = classOf[ShortBlock],
    responseContainer = "List",
    httpMethod = "POST"
  )
  @ApiResponses(
    Array(
      new ApiResponse(code = 400, message = "Invalid Query"),
      new ApiResponse(code = 422, message = "Invalid Query Parameter")))
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
    if (request.body.pageSize < 0) {
      UnprocessableEntity("Page size must not be negative")
    } else if (request.body.cursor.exists(_.isBlank)) {
      UnprocessableEntity("Cursor must not be blank")
    } else {
      val search = queryService.getResultShortlist(request.body)
      logQueryErr(request.body.toString, search, (list: ResultList[ShortBlock]) => Ok(list.asJson))
    }
  }

  @ApiOperation(
    value = "Gets a single entity",
    notes = "Retrieves information about a single entity",
    response = classOf[Option[BaseEt]],
    httpMethod = "GET")
  @ApiResponses(
    Array(new ApiResponse(code = 400, message = NotFoundMsg), new ApiResponse(code = 422, message = "Not an id")))
  def entity(@ApiParam(value = "ID of result entity to fetch", required = true) id: String): Action[AnyContent] =
    Action { implicit request: Request[AnyContent] =>
      if (id.isBlank) {
        UnprocessableEntity("Id must not be blank")
      } else {
        val entity = queryService.getBlock(id)
        // Handle possible result values
        logQueryErr[Option[BaseEt]](s"id:$id", entity, {
          case Some(value) => Ok(value.asJson)
          case None =>
            logger.info(s"Elem not found for id: $id")
            BadRequest(NotFoundMsg)
        })
      }
    }

  @ApiOperation(
    value = "Resolves a theory entity.",
    notes = "Fetches values for relations of a theory entity",
    response = classOf[Option[ResolvedThyEt]],
    httpMethod = "GET")
  @ApiResponses(
    Array(new ApiResponse(code = 400, message = NotFoundMsg), new ApiResponse(code = 422, message = "Not an id")))
  def resolved(@ApiParam(value = "ID of theory entity to fetch", required = true) id: String): Action[AnyContent] =
    Action { implicit request: Request[AnyContent] =>
      if (id.isBlank) {
        UnprocessableEntity("Id must not be blank")
      } else {
        val resolved = queryService.getResultResolved(id)
        // Handle possible result values
        logQueryErr[Option[ResolvedThyEt]](s"id:$id", resolved, {
          case Some(value) => Ok(value.asJson)
          case None =>
            logger.info(s"Found no thy et for id: $id")
            BadRequest(NotFoundMsg)
        })
      }
    }

  @ApiOperation(
    value = "Gets a single command.",
    notes = "Retrieves the shortened information about a single command.",
    response = classOf[Option[ShortBlock]],
    httpMethod = "GET"
  )
  @ApiResponses(
    Array(new ApiResponse(code = 400, message = NotFoundMsg), new ApiResponse(code = 422, message = "Not an id")))
  def shortBlock(@ApiParam(value = "ID of cmd to fetch", required = true) id: String): Action[AnyContent] =
    Action { implicit request: Request[AnyContent] =>
      if (id.isBlank) {
        UnprocessableEntity("Id must not be blank")
      } else {
        val entity = queryService.getShortBlock(id)

        logQueryErr[Option[ShortBlock]](s"id:$id", entity, {
          case Some(value) => Ok(value.asJson)
          case None =>
            logger.info(s"Elem not found for id: $id")
            BadRequest(NotFoundMsg)
        })
      }
    }

  @ApiOperation(
    value = "Facet query",
    notes = "Executes a facet query and returns faceted results.",
    response = classOf[Map[_, _]],
    responseContainer = "Map",
    httpMethod = "POST"
  )
  @ApiResponses(
    Array(
      new ApiResponse(code = 400, message = "Invalid Query"),
      new ApiResponse(code = 422, message = "Invalid Query Parameters")))
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
    if (request.body.maxFacets < 1) {
      UnprocessableEntity("Maximum number of facets must be greater than zero")
    } else {
      val facet = queryService.getResultFacet(request.body)
      logQueryErr(request.body.toString, facet, (res: FacetResult) => Ok(res.asJson))
    }
  }
}
