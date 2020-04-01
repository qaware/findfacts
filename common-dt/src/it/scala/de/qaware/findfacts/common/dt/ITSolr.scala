package de.qaware.findfacts.common.dt

import better.files.File
import de.qaware.findfacts.common.solr.{LocalSolr, SolrRepository}

/** Solr for integration testing. */
object ITSolr {
  def apply(): SolrRepository = {
    val tmpSolr = File.newTemporaryDirectory("itsolr")
    LocalSolr(tmpSolr.toJava)
  }
}
