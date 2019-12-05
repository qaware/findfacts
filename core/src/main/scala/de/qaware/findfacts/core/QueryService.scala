package de.qaware.findfacts.core

import scala.util.Try

/** Query service interface. */
trait QueryService {

  /** Executes query and returns result.
    *
    * @param query to execute
    * @return query result
    */
  def getResults(query: Query): Try[query.Result]
}
