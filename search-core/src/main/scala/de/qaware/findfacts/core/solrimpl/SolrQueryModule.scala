package de.qaware.findfacts.core.solrimpl

import com.softwaremill.macwire.wire
import de.qaware.findfacts.common.solr.SolrRepository
import de.qaware.findfacts.core.QueryModule

/** Solr impl of the query module. */
trait SolrQueryModule extends QueryModule {

  /** Solr repository has to be provided. */
  def repository: SolrRepository

  // Internal modules
  private lazy val solrTermMapper: SolrFilterTermMapper = wire[SolrFilterTermMapper]
  private lazy val solrFilterMapper: SolrFilterMapper = wire[SolrFilterMapper]
  private lazy val solrQueryMapper: SolrQueryMapper = wire[SolrQueryMapper]

  /** Finally provide the service */
  override lazy val service: SolrQueryService = wire[SolrQueryService]
}
