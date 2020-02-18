package de.qaware.findfacts.common.solr

import java.io.File

import better.files.Resource
import org.apache.solr.client.solrj.embedded.EmbeddedSolrServer
import org.scalatest.{FunSuite, Matchers}

class LocalSolrTest extends FunSuite with Matchers {

  test("Test without config") {
    assertThrows[IllegalArgumentException] { LocalSolr(new File(""), "missing") }
  }
}
