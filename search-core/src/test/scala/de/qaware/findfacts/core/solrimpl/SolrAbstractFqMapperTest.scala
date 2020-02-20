package de.qaware.findfacts.core.solrimpl

import scala.collection.mutable
import scala.util.Try

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.core.{AnyInResult, Exact, Filter, FilterQuery, FilterTerm, InRange, Term}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}

class SolrAbstractFqMapperTest extends FunSuite with Matchers with MockFactory with BeforeAndAfterEach {
  implicit val queryService: SolrQueryService = mock[SolrQueryService]
  implicit val qParams: mutable.Map[String, Seq[String]] = mutable.Map.empty

  override def beforeEach(): Unit = {
    qParams.clear()
  }

  test("Test filter query mapping with child fields") {
    implicit val qParams: mutable.Map[String, Seq[String]] = mutable.Map.empty
    val filter = Filter(Map(EtField.Kind -> Exact("Fact"), EtField.Name -> Term("somename")))

    val termMapper = mock[SolrFilterTermMapper]
    (termMapper
      .mapFilterTerm(_: EtField, _: FilterTerm)(_: SolrQueryService, _: mutable.Map[String, Seq[String]]))
      .expects(EtField.Kind, Exact("Fact"), *, *)
      .returning(Try("(thy_kind:\"Fact\")"))
    (termMapper
      .mapFilterTerm(_: EtField, _: FilterTerm)(_: SolrQueryService, _: mutable.Map[String, Seq[String]]))
      .expects(EtField.Name, Term("somename"), *, *)
      .returning(Try("(name:(somename))"))

    val sut = new SolrAbstractFqMapper(termMapper)
    val result = sut.buildFilter(filter).get
    result should equal("({!parent which=cmd_kind:* filters=$cfq0})")
    qParams should have size 1
    qParams should contain key "cfq0"
    qParams should contain value List("(thy_kind:\"Fact\")", "(name:(somename))")
  }

  test("Test filter query mapping with parent and child fields") {
    implicit val qParams: mutable.Map[String, Seq[String]] = mutable.Map.empty
    val filter =
      Filter(Map(EtField.Id -> Exact("id1"), EtField.StartPosition -> Exact("42"), EtField.Proposition -> Exact("*")))

    val termMapper = mock[SolrFilterTermMapper]
    (termMapper
      .mapFilterTerm(_: EtField, _: FilterTerm)(_: SolrQueryService, _: mutable.Map[String, Seq[String]]))
      .expects(EtField.Id, Exact("id1"), *, *)
      .returning(Try("1"))
    (termMapper
      .mapFilterTerm(_: EtField, _: FilterTerm)(_: SolrQueryService, _: mutable.Map[String, Seq[String]]))
      .expects(EtField.StartPosition, Exact("42"), *, *)
      .returning(Try("2"))
    (termMapper
      .mapFilterTerm(_: EtField, _: FilterTerm)(_: SolrQueryService, _: mutable.Map[String, Seq[String]]))
      .expects(EtField.Proposition, Exact("*"), *, *)
      .returning(Try("3"))

    val sut = new SolrAbstractFqMapper(termMapper)
    val result = sut.buildFilter(filter).get
    result should equal("1&&2&&({!parent which=cmd_kind:* filters=$cfq0})")
    qParams.keySet should contain theSameElementsAs List("cfq0")
    qParams("cfq0") should contain theSameElementsAs List("3")
  }
}
