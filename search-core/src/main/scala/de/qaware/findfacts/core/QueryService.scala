package de.qaware.findfacts.core

import scala.util.Try

import de.qaware.findfacts.common.dt.{BaseEt, EtField}
import de.qaware.findfacts.core.QueryService.{FacetResult, SingleResult}

/** Query service interface. */
trait QueryService {

  /** Returns an entity by id.
    *
    * @param id to get
    * @return query result
    */
  def getResult(id: EtField.Id.FieldType): Try[SingleResult]

  /** Executes a facetquery and returns result.
    *
    * @param facetQuery to execute
    * @return query result
    */
  def getFacetResults(facetQuery: FacetQuery): Try[FacetResult]

  /** Executes a filterquery and returns a shortlist.
    *
    * @param filterQuery to execute
    * @return query result as shortlist
    */
  def getResultShortlist(filterQuery: FilterQuery): Try[ResultShortlist]
}

/** Companion object for type aliases. */
object QueryService {

  /** Result type for a single result, may be none. */
  type SingleResult = Option[BaseEt]

  /** Result tpye for facets - a faceting per field. */
  type FacetResult = Map[EtField, Map[String, Long]]
}
