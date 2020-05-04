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

  // Examples
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

  // Parameters
  private final val INDEX = "index to search in"
  private final val BLOCK = "id of block (or contained entity) to fetch"

  // Errors
  private final val NOT_AN_ID = "not an id"
  private final val INVALID_QUERY = "invalid query parameters"
  private final val ENTITY_NOT_FOUND = "index not found, entity not found"
  private final val MALFORMED_QUERY = "index not found, malformed query"

  // Json printer defines output format
  implicit val jsonPrinter: Printer = Printer.noSpacesSortKeys.copy(dropNullValues = true)

  // Api Operations.
  @ApiOperation(
    value = "List Indexes",
    notes = "Retrieves a list of all available indexes.",
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

  @ApiOperation(
    value = "Retrieve Block",
    notes = "Retrieves information about a single block.",
    response = classOf[CodeblockEt],
    httpMethod = "GET")
  @ApiResponses(
    Array(new ApiResponse(code = 400, message = ENTITY_NOT_FOUND), new ApiResponse(code = 422, message = NOT_AN_ID)))
  def block(
      @ApiParam(value = INDEX, required = true) index: String,
      @ApiParam(value = BLOCK, required = true) id: String): Action[AnyContent] =
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
    value = "Retrieve Short Block",
    notes = "Retrieves the shortened information about a single block.",
    response = classOf[ShortBlock],
    httpMethod = "GET"
  )
  @ApiResponses(
    Array(new ApiResponse(code = 400, message = ENTITY_NOT_FOUND), new ApiResponse(code = 422, message = NOT_AN_ID)))
  def shortBlock(
      @ApiParam(value = INDEX, required = true) index: String,
      @ApiParam(value = BLOCK, required = true) id: String): Action[AnyContent] =
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
    value = "Resolve Theory Entity",
    notes = "Retrieves a theory entity and resolved information about its relations.",
    response = classOf[ResolvedThyEt],
    httpMethod = "GET")
  @ApiResponses(
    Array(new ApiResponse(code = 400, message = ENTITY_NOT_FOUND), new ApiResponse(code = 422, message = NOT_AN_ID)))
  def resolved(
      @ApiParam(value = INDEX, required = true) index: String,
      @ApiParam(value = "id of theory entity to fetch", required = true) id: String): Action[AnyContent] =
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
    value = "Execute Filter Query",
    notes = "Executes a filter query and returns a list of all results.",
    response = classOf[ShortBlock],
    responseContainer = "List",
    httpMethod = "POST")
  @ApiResponses(
    Array(new ApiResponse(code = 400, message = MALFORMED_QUERY), new ApiResponse(code = 422, message = INVALID_QUERY)))
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        value = "filter query",
        required = true,
        paramType = "body",
        dataType = "de.qaware.findfacts.core.FilterQuery",
        examples = new Example(value = Array(new ExampleProperty(mediaType = "default", value = ExampleFilterQuery))))))
  def search(@ApiParam(value = INDEX, required = true) index: String): Action[FilterQuery] = {
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
    value = "Execute Facet Query",
    notes = "Executes a facet query and returns faceted results.",
    response = classOf[Int],
    responseContainer = "Map",
    httpMethod = "POST")
  @ApiResponses(
    Array(new ApiResponse(code = 400, message = MALFORMED_QUERY), new ApiResponse(code = 422, message = INVALID_QUERY)))
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        value = "facet query",
        required = true,
        paramType = "body",
        dataType = "de.qaware.findfacts.core.FacetQuery",
        examples = new Example(value = Array(new ExampleProperty(mediaType = "default", value = ExampleFacetQuery)))
      )
    )
  )
  def facet(@ApiParam(value = INDEX, required = true) index: String): Action[FacetQuery] = {
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
}
