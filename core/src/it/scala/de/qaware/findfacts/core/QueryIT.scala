package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.EtFields.StartPosition
import de.qaware.findfacts.common.solr.{ConstRecord, FactRecord}
import de.qaware.findfacts.scalautils.Using
import org.scalatest.{BeforeAndAfterEach, EitherValues, FunSuite, Matchers}

class QueryIT extends FunSuite with BeforeAndAfterEach with Matchers with EitherValues {
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
    val resList = result.right.value
    resList should have size (1)
    resList.head.sourceFile should equal("Example")
  }
}
