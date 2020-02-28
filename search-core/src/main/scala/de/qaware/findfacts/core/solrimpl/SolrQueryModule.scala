package de.qaware.findfacts.core.solrimpl

import com.softwaremill.macwire.wire
import de.qaware.findfacts.core.QueryModule
import org.apache.solr.client.solrj.SolrClient

/** Solr impl of the query module. */
trait SolrQueryModule extends QueryModule {

  /** Solr repository has to be provided.
    *
    * @return instantiated solr client
    */
  def solrClient: SolrClient

  // Internal modules
  private lazy val solrTermMapper: SolrFilterMapper = wire[SolrFilterMapper]
  private lazy val solrFilterMapper: SolrFieldFilterMapper = wire[SolrFieldFilterMapper]
  private lazy val solrQueryMapper: SolrQueryMapper = wire[SolrQueryMapper]

  /** Finally provide the service */
  override lazy val service: SolrQueryService = new SolrQueryService(solrClient, solrQueryMapper)
}
