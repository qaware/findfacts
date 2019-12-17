package de.qaware.findfacts.core

import better.files.{File, Resource}
import de.qaware.findfacts.common.solr.{LocalSolr, SolrRepository}
import de.qaware.findfacts.core.solrimpl.SolrQueryModule

trait ITSolrQueryModule extends SolrQueryModule {
  override lazy val repository: SolrRepository = {
    // Use resource direcotry as solr home
    val solrHome = Resource.getUrl("it-solr/").getFile
    LocalSolr(File(solrHome))
  }
}
