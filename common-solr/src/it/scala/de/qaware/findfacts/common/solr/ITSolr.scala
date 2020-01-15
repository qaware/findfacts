package de.qaware.findfacts.common.solr

import java.io.File

import better.files.Resource

/** Solr for integration testing. Resource directory "it-solr/ has to exist. */
object ITSolr {
  def apply(): SolrRepository = {
    val solrHome = Resource.getUrl("it-solr/").getFile
    LocalSolr(new File(solrHome))
  }
}
