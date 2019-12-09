package de.qaware.findfacts.core

import scala.util.Try

import de.qaware.findfacts.common.dt.BaseEt

/** Query service interface. */
trait QueryService {

  /** Executes a filterquery and returns result.
    *
    * @param filterQuery to execute
    * @return query result
    */
  def getResults(filterQuery: FilterQuery): Try[Vector[BaseEt]]

  /** Executes a facetquery and returns result.
    *
    * @param facetQuery to execute
    * @tparam A type of facet field
    * @return query result
    */
  def getFacetResults[A](facetQuery: FacetQuery): Try[Map[A, Long]]
}
