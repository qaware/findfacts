package de.qaware.findfacts.core

/** Search-DB agnostic query module. */
trait QueryModule {

  /**
   * Abstract service provider.
   *
   * @return instantiated query service
   */
  def service: QueryService
}
