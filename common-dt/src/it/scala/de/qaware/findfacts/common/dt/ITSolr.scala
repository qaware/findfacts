package de.qaware.findfacts.common.dt

import java.io.File

import better.files.Resource
import de.qaware.findfacts.common.solr.{LocalSolr, SolrRepository}

/** Solr for integration testing. Resource directory "it-solr/ has to exist. */
object ITSolr {
  def apply(): SolrRepository = {
    val solrHome = Resource.getUrl("solr/").getFile
    LocalSolr(new File(solrHome), "theorydata")
  }
}
