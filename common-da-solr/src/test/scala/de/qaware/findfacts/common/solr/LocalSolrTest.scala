package de.qaware.findfacts.common.solr

import java.io.{File => JFile}

import better.files.File
import de.qaware.findfacts.scala.Using
import org.apache.solr.client.solrj.SolrQuery
import org.apache.solr.common.SolrInputDocument
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LocalSolrTest extends AnyFunSuite with Matchers {

  test("Test without config") {
    assertThrows[IllegalArgumentException] {
      LocalSolr(new JFile(""), "missing")
    }
  }

  test("Test core creation") {
    File.usingTemporaryDirectory() { dir =>
      Using.resource(LocalSolr(dir.toJava)) { solr =>
        solr.createIndex("test")
        solr.listIndexes should contain theSameElementsAs List("test")
      }
    }
  }

  test("Check core reloading") {
    File.usingTemporaryDirectory() { dir =>
      Using.resource(LocalSolr(dir.toJava)) { solr =>
        // Create index
        solr.createIndex("test")
        // Add document to index
        val doc = new SolrInputDocument("id", "kind")
        doc.setField("id", "id1")
        doc.setField("kind", "Parent")
        solr.add("test", doc)
        val res = solr.commit("test")
        res.getStatus should be(0)
      }
      // Re-open index
      Using.resource(LocalSolr(dir.toJava)) { solr =>
        // Check that index exists
        solr.listIndexes should contain theSameElementsAs List("test")
        // Get document
        val resp = solr.query("test", new SolrQuery("*:*"))
        resp.getStatus should be(0)
        resp.getResults.getNumFound should be(1)
      }
    }
  }

}
