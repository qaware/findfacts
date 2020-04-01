package de.qaware.findfacts

import better.files.{File, Resource}
import de.qaware.findfacts.common.solr.LocalSolr
import de.qaware.findfacts.scala.Using
import org.scalatest.{FunSuite, Matchers}

class ITTest extends FunSuite with Matchers {
  test("check if core was created by dump") {
    Using.resource(LocalSolr(File(Resource.getUrl("solrdir/")).toJava)) { solr =>
      val indexes = solr.listIndexes
      indexes should have size 1
    }
  }
}
