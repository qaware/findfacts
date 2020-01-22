package de.qaware.findfacts.core

import scala.util.Try

import de.qaware.findfacts.common.dt.{BlockEt, EtField, ShortEt}

/** Query service interface. */
trait QueryService {

  /** Executes a filterquery and returns result.
    *
    * @param filterQuery to execute
    * @return query result
    */
  def getResults(filterQuery: FilterQuery): Try[Vector[BlockEt]]

  /** Executes a facetquery and returns result.
    *
    * @param facetQuery to execute
    * @return query result
    */
  def getFacetResults(facetQuery: FacetQuery): Try[Map[EtField, Map[String, Long]]]

  /** Executes a filterquery and returns a shortlist.
    *
    * @param filterQuery to execute
    * @return query result as shortlist
    */
  def getShortResults(filterQuery: FilterQuery): Try[Vector[ShortEt]]
}
