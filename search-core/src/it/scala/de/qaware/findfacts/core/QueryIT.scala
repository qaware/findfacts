package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.EtField.{Kind, Name, PropositionUses, StartPosition}
import de.qaware.findfacts.common.dt.{BaseEt, BlockEt, ConstantEt, EtField, EtKind, FactEt, ITSolr}
import de.qaware.findfacts.common.solr.mapper.ToSolrDoc
import de.qaware.findfacts.core.solrimpl.SolrQueryModule
import org.apache.solr.client.solrj.{SolrClient, SolrQuery}
import org.scalatest.{BeforeAndAfterAll, FunSuite, Inside, Matchers, TryValues}

class QueryIT extends FunSuite with BeforeAndAfterAll with Matchers with TryValues with Inside {
  final val solr = ITSolr.apply().solrConnection()
  final val queryModule: QueryModule = new SolrQueryModule { override lazy val solrClient: SolrClient = solr }

  override def beforeAll(): Unit = {
    // Add integration test data set
    val const1 = new ConstantEt("Const1", "...", List("someid"), Nil, "'a => 'b")
    val block1 = BlockEt("ExampleThy.1", "ExampleThy", 1, 11, "fun Example = ...", List(const1))
    val fact1 = new FactEt("ConstIsFact", "IsFact Const1", List(const1.id), Nil)
    val block2 = BlockEt("ExampleThy.12", "ExampleThy", 12, 14, "lemma ...", List(fact1))

    val mapper = ToSolrDoc[BaseEt]
    solr.add(mapper.toSolrDoc(block1))
    solr.add(mapper.toSolrDoc(block2))
    val status = solr.commit()
    status.getStatus should (be(200) or be(0))
  }

  override def afterAll(): Unit = solr.close()

  test("Check all present") {
    solr.query(new SolrQuery("*:*")).getResults.size() should be(4)
  }

  test("Filter query") {
    val query = FilterQuery(Filter(Map(StartPosition -> InRange(10, 30))), 10)
    val result = queryModule.service.getResults(query)

    val resList = result.success.value
    resList should have size 1
    inside(resList) {
      case Vector(b: BlockEt) =>
        b.startPosition should be(12)
        b.endPosition should be(14)
        b.entities should have size 1
        b.entities.head.name should equal("ConstIsFact")
    }
  }

  test("Filter query shortlist") {
    val query = FilterQuery(Filter(Map(Kind -> Id(EtKind.Constant.entryName))), 10)
    val result = queryModule.service.getShortResults(query)

    val resList = result.success.value
    resList should have size 1

    val thyRes = resList.head.entities
    thyRes should have size 1
    thyRes.head.kind should equal(EtKind.Constant)
    thyRes.head.shortDescription should equal("Const1 :: 'a => 'b")
  }

  test("Recursive query") {
    val innerQuery = Filter(Map(Kind -> StringExpression(EtKind.Constant.toString)))
    val query = FilterQuery(Filter(Map(PropositionUses -> AnyInResult(innerQuery))), 10)
    val result = queryModule.service.getShortResults(query)

    result.success.value should have size 1
    result.success.value.head.entities should have size 1
    result.success.value.head.entities.head.name should equal("ConstIsFact")
  }

  test("Query set operations") {
    // matches nothing
    val noMatchQuery = Filter(Map(Name -> StringExpression("does not exist")))
    // matches all
    val query1 = Filter(Map(PropositionUses -> AllInResult(noMatchQuery)))
    // matches kind:Constant
    val query2 = Filter(Map(Kind -> StringExpression(EtKind.Constant.toString)))
    // matches all intersect kind:Constant
    val query = FilterQuery(FilterIntersection(query1, query2), 10)
    val result = queryModule.service.getShortResults(query)

    result.success.value should have size 1
    result.success.value.head.entities should have size 1
    result.success.value.head.entities.head.name should equal("Const1")
  }

  test("Facet query") {
    val query = FacetQuery(Filter(Map.empty), Set(EtField.StartPosition))
    val result = queryModule.service.getFacetResults(query)

    val resultFacet = result.success.value
    resultFacet should equal(Map(EtField.StartPosition -> Map("1" -> 1, "12" -> 1)))
  }
}
