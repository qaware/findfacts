package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.EtField.{StartLine, Uses}
import de.qaware.findfacts.common.dt._
import de.qaware.findfacts.common.solr.mapper.ToSolrDoc
import de.qaware.findfacts.common.solr.{LocalSolr, SolrRepository}
import de.qaware.findfacts.core.solrimpl.SolrQueryModule
import org.apache.solr.client.solrj.SolrQuery
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, Inside}

/** Test base functionality of query module with a setup that's as simple as possible.  */
class SimpleQueryIT extends AnyFunSuite with BeforeAndAfterAll with Matchers with Inside {
  final val itSolr = ITSolr()
  final val queryModule: QueryModule = new SolrQueryModule {
    override lazy val solr: SolrRepository = itSolr
  }
  implicit val index: String = LocalSolr.DEFAULT_CORE_NAME

  override def beforeAll(): Unit = {
    itSolr.createIndex(LocalSolr.DEFAULT_CORE_NAME)

    // Add integration test data set
    val const1 = ConstantEt("Const.ExampleThy.Const1", "Const1", List("someId"), "'a => 'b")
    val block1 =
      CodeblockEt("ExampleThy.1.11", "ExampleThy", 1, "fun", "\n", "fun Example = ...", "\n...", List(const1))
    val fact1 = FactEt("Fact.ExampleThy.ConstIsFact", "ConstIsFact", List(const1.id))
    val block2 =
      CodeblockEt(
        "ExampleThy.12.14",
        "ExampleThy",
        3,
        "lemma",
        "\n...",
        "lemma example_lem...",
        "(* stuff *)",
        List(fact1))

    val mapper = ToSolrDoc[BaseEt]
    itSolr.add(mapper.toSolrDoc(block1))
    itSolr.add(mapper.toSolrDoc(block2))
    itSolr.commit().getStatus should (be(200) or be(0))
  }

  override def afterAll(): Unit = itSolr.close()

  test("Check all present") {
    itSolr.query(new SolrQuery("*:*")).getResults.size() should be(4)
  }

  test("Filter query") {
    val query = FilterQuery(List(FieldFilter(StartLine, InRange(2, 5))))
    val result = queryModule.service.getResultShortlist(query)

    val resList = result.get
    resList.count should be(1)
    resList.values should have size 1
    inside(resList.values) {
      case Vector(block) =>
        block.id should equal("ExampleThy.12.14")
        block.entities should have size 1
        block.entities.head.name should equal("ConstIsFact")
    }
  }

  test("Filter query shortlist") {
    val query = FilterQuery(List(FieldFilter(EtField.Kind, Exact(Kind.Constant.entryName))))
    val result = queryModule.service.getResultShortlist(query)

    val resList = result.get
    resList.values should have size 1

    val thyRes = resList.values.head.entities
    thyRes should have size 1
    thyRes.head.kind should equal(Kind.Constant)
  }

  test("Recursive query") {
    val innerQuery = List(FieldFilter(EtField.Kind, Exact(Kind.Constant.entryName)))
    val query = FilterQuery(List(FieldFilter(Uses, InResult(EtField.Id, innerQuery))))
    val result = queryModule.service.getResultShortlist(query)

    result.get.values should have size 1
    result.get.values.head.entities should have size 1
    result.get.values.head.entities.head.name should equal("ConstIsFact")
  }

  test("Facet query") {
    val query = FacetQuery(List(), Set(EtField.StartLine))
    val result = queryModule.service.getResultFacet(query)

    val resultFacet = result.get
    resultFacet should equal(Map(EtField.StartLine -> Map("1" -> 1, "3" -> 1)))
  }

  test("Query with connectives") {
    val query =
      FilterQuery(List(FieldFilter(EtField.Kind, Not(Or(Exact(Kind.Fact.entryName), Exact(Kind.Type.entryName))))))
    val result = queryModule.service.getResultShortlist(query)

    val resList = result.get
    resList.values should have size 1

    val thyRes = resList.values.head.entities
    thyRes should have size 1
    thyRes.head.kind should equal(Kind.Constant)
  }

  test("Result scoring") {
    val query = FilterQuery(List(FieldFilter(EtField.SourceCode, Or(Exact("example"), Exact("lemma")))))
    val result = queryModule.service.getResultShortlist(query)

    val resList = result.get
    resList.values should have size 2

    val first = resList.values.head
    first.command should equal("lemma")
  }
}
