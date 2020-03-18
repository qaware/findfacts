package de.qaware.findfacts.webapp.utils

import de.qaware.findfacts.core.{FacetQuery, FilterQuery, QueryService}
import play.api.Logging
import play.api.mvc.{Action, ActionBuilder, AnyContent, Request, Result, Results}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** Utility to provide re-usable action fragments.
  *
  * @param queryService to execute queries
  * @param actions underlying action builder for atoms
  * @param ec execution context for futures
  */
class ActionBuilderUtils(queryService: QueryService, actions: ActionBuilder[Request, AnyContent])(
    implicit ec: ExecutionContext)
    extends Logging
    with Results {
  private final val InternalErrorMsg = "Internal server error"
  private final val IndexNotFoundMsg = "Index not found"
  private final val EntityNotFoundMsg = "Entity not found"

  /** Handles [[Try]]s (failures indicating server errors).
    *
    * @param fn that creates the try from input
    * @param onSuccess action function when try succeeds
    * @tparam A input parameter type
    * @tparam B type of result in try
    * @return action function to handle input
    */
  def inResult[A, B](fn: A => Try[B])(onSuccess: B => Future[Result]): A => Future[Result] = { a =>
    Future(fn(a)).flatMap {
      case Failure(exception) =>
        logger.error(s"Error executing request $a: ", exception)
        Future.successful(InternalServerError(InternalErrorMsg))
      case Success(value) => onSuccess(value)
    }
  }

  /** Handles [[Option]]s (missing values indicating not found).
    *
    * @param fn that creates the option from input
    * @param onSome action function when option is present
    * @tparam A input parameter type
    * @tparam B type of result in option
    * @return action function to handle input
    */
  def inOption[A, B](fn: A => Option[B])(onSome: B => Future[Result]): A => Future[Result] = { a =>
    Future(fn(a)).flatMap {
      case Some(value) => onSome(value)
      case None =>
        logger.info(s"Not found: $a")
        Future.successful(BadRequest(EntityNotFoundMsg))
    }
  }

  /** Checks if a given index is available.
    *
    * @param index to check
    * @param action to execute if present
    * @tparam A type of action
    * @return action that can handle missing index
    */
  def indexValid[A](index: String)(action: Action[A]): Action[A] = actions.async(action.parser) { request: Request[A] =>
    inResult((_: A) => queryService.listIndexes) { indexes: List[String] =>
      if (indexes.contains(index)) {
        action(request)
      } else {
        Future.successful(BadRequest(IndexNotFoundMsg))
      }
    }(request.body)
  }

  /** Checks if a given id is valid.
    *
    * @param id to check
    * @param action to execute if valid
    * @tparam A type of action
    * @return action that can handle invalid id
    */
  def idValid[A](id: String)(action: Action[A]): Action[A] = actions.async(action.parser) { request: Request[A] =>
    if (id.isBlank) {
      Future.successful(UnprocessableEntity("Id must not be blank"))
    } else {
      action(request)
    }
  }

  /** Checks if a given filter query is valid.
    *
    * @param action to execute if valid
    * @return action that can handle invalid queries
    */
  def filterQueryValid(action: Action[FilterQuery]): Action[FilterQuery] = actions.async(action.parser) {
    request: Request[FilterQuery] =>
      if (request.body.pageSize < 0) {
        Future.successful(UnprocessableEntity("Page size must not be negative"))
      } else if (request.body.cursor.exists(_.isBlank)) {
        Future.successful(UnprocessableEntity("Cursor must not be blank"))
      } else {
        action(request)
      }
  }

  /** Checks if a given facet query is valid.
    *
    * @param action to execute if valid
    * @return action that can handle invalid queries
    */
  def facetQueryValid(action: Action[FacetQuery]): Action[FacetQuery] = actions.async(action.parser) {
    request: Request[FacetQuery] =>
      if (request.body.maxFacets < 1) {
        Future.successful(UnprocessableEntity("Maximum number of facets must be greater than zero"))
      } else {
        action(request)
      }
  }
}
