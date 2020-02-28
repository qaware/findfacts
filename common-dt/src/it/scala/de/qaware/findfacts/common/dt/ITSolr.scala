package de.qaware.findfacts.common.dt

import java.nio.file.Path
import java.util.UUID

import better.files.{File, Resource}
import de.qaware.findfacts.common.solr.{LocalSolr, SolrRepository}

/** Solr for integration testing. Resource directory "solr/ has to exist. */
object ITSolr {
  def apply(): SolrRepository = {
    val tmpSolr = File(Resource.getUrl("solr/")) / UUID.randomUUID().toString
    tmpSolr.createDirectory()
    LocalSolr(tmpSolr.toJava, "theorydata")
  }
}
