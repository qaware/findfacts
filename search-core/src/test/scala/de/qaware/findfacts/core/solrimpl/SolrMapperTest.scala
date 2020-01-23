package de.qaware.findfacts.core.solrimpl

import scala.collection.mutable
import scala.util.Try

import de.qaware.findfacts.common.dt.{EtField, IdChild, IdEt}
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.core.{AnyInResult, Filter, FilterQuery, FilterTerm, Id, InRange, Number, StringExpression}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers, TryValues}

class SolrMapperTest extends FunSuite with Matchers with TryValues with MockFactory {
  implicit val queryService: SolrQueryService = mock[SolrQueryService]

  test("Test string expression filter term mapping") {
    val sut = new SolrFilterTermMapper()

    val fq = StringExpression("*gauss*")

    sut.buildFilterQuery(fq).success.value should equal("(*gauss*)")
  }

  test("Test range filter term mapping") {
    val sut = new SolrFilterTermMapper()

    val fq = InRange(10, 42)

    sut.buildFilterQuery(fq).success.value should equal("[10 TO 42]")
  }

  // mocking does not work for some reason
  ignore("Test recursive filter term mapping") {
    val sut = new SolrFilterTermMapper()

    val fq = Filter(Map())
    val query = AnyInResult(fq)
    val results = Try(Vector(IdEt(List(IdChild("id1"), IdChild("id2")))))

    val fromSolrDoc: FromSolrDoc[IdEt] = FromSolrDoc[IdEt]

    (queryService
      .getListResults(_: FilterQuery)(_: FromSolrDoc[IdEt]))
      .expects(FilterQuery(fq, 10), *)
      .returning(results)

    sut.buildFilterQuery(query).success.value should equal("(id1 OR id2)")
  }

  test("Test filter query mapping with child fields") {
    implicit val qParams: mutable.Map[String, Seq[String]] = mutable.Map.empty
    val filter = Filter(Map(EtField.Kind -> StringExpression("Fact"), EtField.Name -> Id("somename")))

    val termMapper = mock[SolrFilterTermMapper]
    (termMapper
      .buildFilterQuery(_: FilterTerm)(_: SolrQueryService))
      .expects(StringExpression("Fact"), *)
      .returning(Try("(Fact)"))
    (termMapper
      .buildFilterQuery(_: FilterTerm)(_: SolrQueryService))
      .expects(Id("somename"), *)
      .returning(Try("somename"))

    val sut = new SolrFilterMapper(termMapper)
    val result = sut.buildFilter(filter).success.value
    result should equal(" +{!parent which='kind:Block OR kind:Documentation' filters=$childfq0}")
    qParams.keySet should contain theSameElementsAs List("childfq0")
    qParams("childfq0") should contain theSameElementsAs List("kind:(Fact)", "name:somename")
  }

  test("Test filter query mapping with parent and child fields") {
    implicit val qParams: mutable.Map[String, Seq[String]] = mutable.Map.empty
    val filter = Filter(Map(EtField.Id -> Id("id1"), EtField.StartPosition -> Number(42)))

    val termMapper = mock[SolrFilterTermMapper]
    (termMapper.buildFilterQuery(_: FilterTerm)(_: SolrQueryService)).expects(Id("id1"), *).returning(Try("id1"))
    (termMapper.buildFilterQuery(_: FilterTerm)(_: SolrQueryService)).expects(Number(42), *).returning(Try("42"))

    val sut = new SolrFilterMapper(termMapper)
    val result = sut.buildFilter(filter).success.value
    result should equal(
      "( +start_pos:42 +id:id1 +{!parent which='kind:Block OR kind:Documentation'} OR +start_pos:42 +{!parent which='kind:Block OR kind:Documentation' filters=$childfq0})")
    qParams.keySet should contain theSameElementsAs List("childfq0")
    qParams("childfq0") should contain theSameElementsAs List("id:id1")
  }
}
