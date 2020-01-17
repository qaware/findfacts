package de.qaware.findfacts.common.solr

import java.io.File

import better.files.Resource
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.scalatest.{FunSuite, Matchers}

class LocalSolrTest extends FunSuite with Matchers {

  test("Test uniqueness") {
    val solrRepo = LocalSolr(new File(Resource.getUrl("solr/").getFile), "testdata")
    val conn1 = solrRepo.solrConnection().asInstanceOf[EmbeddedSolrServer]
    val conn2 = solrRepo.solrConnection().asInstanceOf[EmbeddedSolrServer]

    val dir1 = conn1.getCoreContainer.getCoreRootDirectory
    val dir2 = conn2.getCoreContainer.getCoreRootDirectory

    dir1 should not equal dir2

    conn1.close()
    conn2.close()
  }

  test("Test without config") {
    assertThrows[IllegalArgumentException] { LocalSolr(new File(""), "missing") }
  }
}
