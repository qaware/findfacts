package de.qaware.findfacts.core

import scala.util.Try

import de.qaware.findfacts.common.dt.{BaseEt, EtField}
import de.qaware.findfacts.core.QueryService.{FacetResult, ResultList}
import de.qaware.findfacts.core.dt.{ResolvedThyEt, ShortCmd}

/** Query service interface. */
trait QueryService {

  /** Returns an entity by id.
    *
    * @param id to get
    * @return query result
    */
  def getResult(id: EtField.Id.T): Try[Option[BaseEt]]

  /** Returns the short version of an cmd by id.
    *
    * @param id to get
    * @return query result
    */
  def getShortResult(id: EtField.Id.T): Try[Option[ShortCmd]]

  /** Finds an entity by id and resolves its references to short entities.
    *
    * @param id to get
    * @return resolved query result
    */
  def getResultResolved(id: EtField.Id.T): Try[Option[ResolvedThyEt]]

  /** Executes a facetquery and returns result.
    *
    * @param facetQuery to execute
    * @return query result
    */
  def getResultFacet(facetQuery: FacetQuery): Try[FacetResult]

  /** Executes a filterquery and returns a shortlist.
    *
    * @param filterQuery to execute
    * @return query result as shortlist
    */
  def getResultShortlist(filterQuery: FilterQuery): Try[ResultList[ShortCmd]]
}

/** Companion object for type aliases. */
object QueryService {

  /** Result tpye for facets - a faceting per field. */
  type FacetResult = Map[EtField, Map[String, Long]]

  /** List of results, with metadata.
    *
    * @param values result values
    * @param count number of total elements found
    * @param nextCursor for paging
    * @tparam A type of result entities
    */
  case class ResultList[A](values: Vector[A], count: Long, nextCursor: String)
}
