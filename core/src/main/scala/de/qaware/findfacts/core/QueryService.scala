package de.qaware.findfacts.core

/** Query service interface. */
trait QueryService {

  /** Executes query and returns result.
    *
    * @param query to execute
    * @return query result
    */
  def getResults(query: Query): Either[Throwable, query.Result]
}
