package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.EtField.{Kind, Name, PropositionUses, StartPosition}
import de.qaware.findfacts.common.dt.{BaseEt, CodeblockEt, ConstantEt, EtField, FactEt, ITSolr, ThyEtKind}
import de.qaware.findfacts.common.solr.mapper.ToSolrDoc
import de.qaware.findfacts.core.dt.ShortBlock
import de.qaware.findfacts.core.solrimpl.SolrQueryModule
import org.apache.solr.client.solrj.{SolrClient, SolrQuery}
import org.scalatest.{BeforeAndAfterAll, FunSuite, Inside, Matchers}

/** Test base functionality of query module with a setupt that's as simple as possible.  */
class SimpleQueryIT extends FunSuite with BeforeAndAfterAll with Matchers with Inside {
  final val solr = ITSolr.apply().solrConnection()
  final val queryModule: QueryModule = new SolrQueryModule { override lazy val solrClient: SolrClient = solr }

  override def beforeAll(): Unit = {
    // Add integration test data set
    val const1 = new ConstantEt("Const1", "...", List("someid"), Nil, "'a => 'b")
    val block1 = CodeblockEt("ExampleThy.1", "ExampleThy", 1, 11, "fun Example = ...", List(const1))
    val fact1 = new FactEt("ConstIsFact", "IsFact Const1", List(const1.id), Nil)
    val block2 = CodeblockEt("ExampleThy.12", "ExampleThy", 12, 14, "lemma ...", List(fact1))

    val mapper = ToSolrDoc[BaseEt]
    solr.add(mapper.toSolrDoc(block1))
    solr.add(mapper.toSolrDoc(block2))
    solr.commit().getStatus should (be(200) or be(0))
  }

  override def afterAll(): Unit = solr.close()

  test("Check all present") {
    solr.query(new SolrQuery("*:*")).getResults.size() should be(4)
  }

  test("Filter query") {
    val query = FilterQuery(Filter(Map(StartPosition -> InRange(10, 30))))
    val result = queryModule.service.getResultShortlist(query)

    val resList = result.get
    resList.count should be(1)
    resList.values should have size 1
    inside(resList.values) {
      case Vector(ShortBlock(id, theory, src, entities)) =>
        id should equal("ExampleThy.12")
        entities should have size 1
        entities.head.name should equal("ConstIsFact")
    }
  }

  test("Filter query shortlist") {
    val query = FilterQuery(Filter(Map(Kind -> Exact(ThyEtKind.Constant.entryName))))
    val result = queryModule.service.getResultShortlist(query)

    val resList = result.get
    resList.values should have size 1

    val thyRes = resList.values.head.entities
    thyRes should have size 1
    thyRes.head.kind should equal(ThyEtKind.Constant)
  }

  test("Recursive query") {
    val innerQuery = Filter(Map(Kind -> Exact(ThyEtKind.Constant.toString)))
    val query = FilterQuery(Filter(Map(PropositionUses -> AnyInResult(innerQuery))))
    val result = queryModule.service.getResultShortlist(query)

    result.get.values should have size 1
    result.get.values.head.entities should have size 1
    result.get.values.head.entities.head.name should equal("ConstIsFact")
  }

  test("Query set operations") {
    // matches nothing
    val noMatchQuery = Filter(Map(Name -> Exact("does not exist")))
    // matches all
    val query1 = Filter(Map(PropositionUses -> AllInResult(noMatchQuery)))
    // matches kind:Constant
    val query2 = Filter(Map(Kind -> Exact(ThyEtKind.Constant.toString)))
    // matches all intersect kind:Constant
    val query = FilterQuery(FilterIntersection(query1, query2))
    val result = queryModule.service.getResultShortlist(query)

    result.get.values should have size 1
    result.get.values.head.entities should have size 1
    result.get.values.head.entities.head.name should equal("Const1")
  }

  test("Facet query") {
    val query = FacetQuery(Filter(Map.empty), Set(EtField.StartPosition))
    val result = queryModule.service.getResultFacet(query)

    val resultFacet = result.get
    resultFacet should equal(Map(EtField.StartPosition -> Map("1" -> 1, "12" -> 1)))
  }
}
