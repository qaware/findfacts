package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.EtField.StartPosition
import de.qaware.findfacts.common.dt.{EtField, EtKind, FactEt}
import de.qaware.findfacts.common.solr.{ConstRecord, FactRecord}
import de.qaware.findfacts.scala.Using
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers, TryValues}

class QueryIT extends FunSuite with BeforeAndAfterEach with Matchers with TryValues {
  val queryModule: ITSolrQueryModule = new ITSolrQueryModule {}

  override def beforeEach(): Unit = {
    // Reset solr data
    Using.resource(queryModule.repository.solrConnection()) { solr =>
      // First delete all
      solr.deleteByQuery("*:*")
      var status = solr.commit()
      status.getStatus should (be(200) or be(0))

      // Then add integration test data set
      val const1 = ConstRecord(
        "Example",
        1,
        11,
        "Const1",
        "'a => 'b'",
        Array(),
        "prop",
        Array(),
        Array(),
        "fun Const1 = ..."
      )
      val fact1 =
        FactRecord("Example", 20, 22, "ConstIsFact", "IsFact Const1", Array(const1.id), Array(), Array(), "lemma ...")
      solr.addBean(const1)
      solr.addBean(fact1)
      status = solr.commit()
      status.getStatus should (be(200) or be(0))
    }
  }

  test("Filter query") {
    val query = FilterQuery(Filter(Map(StartPosition -> InRange(10, 30))))
    val result = queryModule.service.getResults(query)

    val resList = result.success.value
    resList should have size (1)
    resList should matchPattern {
      case Vector(_: FactEt) =>
    }
    resList match {
      case Vector(f: FactEt) =>
        f.name should equal("ConstIsFact")
        f.startPosition should equal(20)
        f.kind should equal(EtKind.Fact)
        f.propositionUses should have size (1)
        f.related should have size (0)
    }
  }

  test("Facet query") {
    val query = FacetQuery(Filter(Map.empty), EtField.StartPosition)
    val result = queryModule.service.getFacetResults(query)

    val resultFacet = result.success.value
    resultFacet should equal(Map(1 -> 1, 20 -> 1))
  }
}
