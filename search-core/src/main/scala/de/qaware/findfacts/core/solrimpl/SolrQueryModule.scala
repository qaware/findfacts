package de.qaware.findfacts.core.solrimpl

import com.softwaremill.macwire.wire

import de.qaware.findfacts.common.solr.SolrRepository
import de.qaware.findfacts.core.QueryModule

/** Solr impl of the query module. */
trait SolrQueryModule extends QueryModule {

  /**
   * Solr repository has to be provided.
   *
   * @return instantiated solr repository
   */
  def solr: SolrRepository

  // Internal modules
  lazy val solrTermMapper: SolrFilterMapper = wire[SolrFilterMapper]
  lazy val solrFilterMapper: SolrFieldFilterMapper = wire[SolrFieldFilterMapper]
  lazy val solrQueryMapper: SolrQueryMapper = wire[SolrQueryMapper]

  /** Finally provide the service */
  override lazy val service: SolrQueryService = new SolrQueryService(solr, solrQueryMapper)
}
