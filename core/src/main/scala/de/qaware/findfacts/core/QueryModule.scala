package de.qaware.findfacts.core

/** Search-DB agnostic query module. */
trait QueryModule {

  /** Abstract service provider. */
  val service: QueryService
}
