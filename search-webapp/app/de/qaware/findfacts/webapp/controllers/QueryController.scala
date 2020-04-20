package de.qaware.findfacts.webapp.controllers

import scala.concurrent.Future
import scala.language.postfixOps

import io.circe.Printer
import io.circe.syntax._
import io.swagger.annotations._
import play.api.Logging
import play.api.libs.circe.Circe
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents, Request}

import de.qaware.findfacts.common.dt.CodeblockEt
import de.qaware.findfacts.core.dt.{ResolvedThyEt, ShortBlock}
import de.qaware.findfacts.core.{FacetQuery, FilterQuery, QueryService}
import de.qaware.findfacts.webapp.utils.{ActionBuilderUtils, JsonMappings}

/**
 * Controller for the query api.
 *
 * @param cc injected components
 * @param queryService query service component
 * @param jsonMappings component to map queries and results to and from json
 */
@Api(value = "/")
class QueryController(
    cc: ControllerComponents,
    queryService: QueryService,
    actionBuilder: ActionBuilderUtils,
    jsonMappings: JsonMappings)
  extends AbstractController(cc)
  with Circe
  with Logging {
  // Import all json mapping implicits

  import jsonMappings._

  private final val ExampleFilterQuery =
    """
{
  "filters" : [
    {
      "field" : "Name",
      "filter" : {
        "Term" : {
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
        "Term" : {
          "inner" : "*gauss*"
        }
      }
    }
  ],
  "fields" : [ "Kind", "NameFacet", "SourceTheory" ],
  "maxFacets": 10
}
"""

  // Json printer defines output format
  implicit val jsonPrinter: Printer = Printer.noSpacesSortKeys.copy(dropNullValues = true)

  // Api Operations.
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
      new ApiResponse(code = 400, message = "Index Not Found"),
      new ApiResponse(code = 422, message = "Invalid Query Parameter")
    ))
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
  def search(@ApiParam(value = "Index to search in", required = true) index: String): Action[FilterQuery] = {
    implicit val idx: String = index
    actionBuilder.indexValid(index) {
      actionBuilder.filterQueryValid {
        Action.async(circe.json[FilterQuery]) { request =>
          actionBuilder.inResult(queryService.getResultShortlist) { list =>
            Future.successful(Ok(list.asJson))
          }(request.body)
        }
      }
    }
  }

  @ApiOperation(
    value = "Gets a single entity",
    notes = "Retrieves information about a single entity",
    response = classOf[Option[CodeblockEt]],
    httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 400, message = "Entity not found"),
      new ApiResponse(code = 422, message = "Not an id")))
  def entity(
      @ApiParam(value = "Index to search in", required = true) index: String,
      @ApiParam(value = "ID of result entity to fetch", required = true) id: String): Action[AnyContent] =
    actionBuilder.indexValid(index) {
      actionBuilder.idValid(id) {
        Action.async { request: Request[AnyContent] =>
          actionBuilder.inResult((_: Request[AnyContent]) => queryService.getBlock(id)(index)) {
            actionBuilder.inOption(identity[Option[CodeblockEt]]) { value =>
              Future.successful(Ok(value.asJson))
            }
          }(request)
        }
      }
    }

  @ApiOperation(
    value = "Resolves a theory entity.",
    notes = "Fetches values for relations of a theory entity",
    response = classOf[Option[ResolvedThyEt]],
    httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 400, message = "Entity not found"),
      new ApiResponse(code = 422, message = "Not an id")))
  def resolved(
      @ApiParam(value = "Index to search in", required = true) index: String,
      @ApiParam(value = "ID of theory entity to fetch", required = true) id: String): Action[AnyContent] =
    actionBuilder.indexValid(index) {
      actionBuilder.idValid(id) {
        Action.async { request: Request[AnyContent] =>
          actionBuilder.inResult((_: Request[AnyContent]) => queryService.getResultResolved(id)(index)) {
            actionBuilder.inOption(identity[Option[ResolvedThyEt]]) { value =>
              Future.successful(Ok(value.asJson))
            }
          }(request)
        }
      }
    }
  @ApiOperation(
    value = "Gets a single command.",
    notes = "Retrieves the shortened information about a single command.",
    response = classOf[Option[ShortBlock]],
    httpMethod = "GET"
  )
  @ApiResponses(
    Array(
      new ApiResponse(code = 400, message = "Entity not found"),
      new ApiResponse(code = 422, message = "Not an id")))
  def shortBlock(
      @ApiParam(value = "Index to search in", required = true) index: String,
      @ApiParam(value = "ID of cmd to fetch", required = true) id: String): Action[AnyContent] =
    actionBuilder.indexValid(index) {
      actionBuilder.idValid(id) {
        Action.async { request: Request[AnyContent] =>
          actionBuilder.inResult((_: Request[AnyContent]) => queryService.getShortBlock(id)(index)) {
            actionBuilder.inOption(identity[Option[ShortBlock]]) { value =>
              Future.successful(Ok(value.asJson))
            }
          }(request)
        }
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
  def facet(@ApiParam(value = "Index to search in", required = true) index: String): Action[FacetQuery] = {
    implicit val idx: String = index
    actionBuilder.indexValid(index) {
      actionBuilder.facetQueryValid {
        Action.async(circe.json[FacetQuery]) { request: Request[FacetQuery] =>
          actionBuilder.inResult(queryService.getResultFacet) { facet =>
            Future.successful(Ok(facet.asJson))
          }(request.body)
        }
      }
    }
  }

  @ApiOperation(
    value = "Index list",
    notes = "Gets all available indexes.",
    response = classOf[String],
    responseContainer = "List",
    httpMethod = "GET"
  )
  def indexes(): Action[AnyContent] =
    Action.async { request: Request[AnyContent] =>
      actionBuilder.inResult((_: Request[AnyContent]) => queryService.listIndexes) { indexes =>
        Future.successful(Ok(indexes.asJson))
      }(request)
    }
}
