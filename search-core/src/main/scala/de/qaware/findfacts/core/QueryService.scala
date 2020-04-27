package de.qaware.findfacts.core

import scala.util.Try

import de.qaware.findfacts.common.dt.{CodeblockEt, EtField}
import de.qaware.findfacts.core.QueryService.{FacetResult, ResultList}
import de.qaware.findfacts.core.dt.{ResolvedThyEt, ShortBlock}

/** Query service interface. */
trait QueryService {

  /**
   * Returns an entity by id.
   *
   * @param id to get
   * @param index to search in
   * @return query result
   */
  def getBlock(id: EtField.Id.T)(implicit index: String): Try[Option[CodeblockEt]]

  /**
   * Returns the short version of an entity by id.
   *
   * @param id to get
   * @param index to search in
   * @return query result
   */
  def getShortBlock(id: EtField.Id.T)(implicit index: String): Try[Option[ShortBlock]]

  /**
   * Finds a theory entity by id and resolves its references to short entities.
   *
   * @param id to get
   * @param index to search in
   * @return resolved query result
   */
  def getResultResolved(id: EtField.Id.T)(implicit index: String): Try[Option[ResolvedThyEt]]

  /**
   * Executes a facet query and returns result.
   *
   * @param facetQuery to execute
   * @param index to search in
   * @return query result
   */
  def getResultFacet(facetQuery: FacetQuery)(implicit index: String): Try[FacetResult]

  /**
   * Executes a filter query and returns a shortlist.
   *
   * @param filterQuery to execute
   * @param index to search in
   * @return query result as shortlist
   */
  def getResultShortlist(filterQuery: FilterQuery)(implicit index: String): Try[ResultList[ShortBlock]]

  /**
   * Lists available indexes.
   *
   * @return list of available indexes
   */
  def listIndexes: Try[List[String]]
}

/** Companion object for type aliases. */
object QueryService {

  /** Result type for facets - a faceting per field. */
  type FacetResult = Map[EtField, Map[String, Long]]

  /**
   * List of results, with metadata.
   *
   * @param values result values
   * @param count number of total elements found
   * @param nextCursor for paging
   * @tparam A type of result entities
   */
  final case class ResultList[A](values: Vector[A], count: Long, nextCursor: String)

}
