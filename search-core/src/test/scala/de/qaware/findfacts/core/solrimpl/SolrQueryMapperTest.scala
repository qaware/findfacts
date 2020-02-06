package de.qaware.findfacts.core.solrimpl

import scala.collection.mutable
import scala.util.Try

import de.qaware.findfacts.common.dt.{EtField, IdChild, IdEt}
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.core.{AnyInResult, Filter, FilterQuery, FilterTerm, InRange, Term}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}

class SolrQueryMapperTest extends FunSuite with Matchers with MockFactory {
  implicit val queryService: SolrQueryService = mock[SolrQueryService]

  test("Test string expression filter term mapping") {
    val sut = new SolrFilterTermMapper()

    val fq = Term("*gauss*")

    sut.buildFilterQuery(fq).get should equal("(*gauss*)")
  }

  test("Test range filter term mapping") {
    val sut = new SolrFilterTermMapper()

    val fq = InRange(10, 42)

    sut.buildFilterQuery(fq).get should equal("[10 TO 42]")
  }

  test("Test term escaping") {
    val sut = new SolrFilterTermMapper()

    val allAscii =
      " !\"#$%&\\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~&&||"
    val fq = Term(allAscii)
    sut.buildFilterQuery(fq).get should equal(
      "( \\!\\\"#$%&\\\\'\\(\\)*\\+,\\-.\\/0123456789\\:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\[\\]\\^_`abcdefghijklmnopqrstuvwxyz\\{|\\}\\~\\&&\\||)")
  }

  // mocking does not work for some reason
  ignore("Test recursive filter term mapping") {
    val sut = new SolrFilterTermMapper()

    val fq = Filter(Map())
    val query = AnyInResult(fq)
    val results = Try(Vector(IdEt(List(IdChild("id1"), IdChild("id2")))))

    (queryService
      .getListResults(_: FilterQuery)(_: FromSolrDoc[IdEt]))
      .expects(FilterQuery(fq, 10), *)
      .returning(results)

    sut.buildFilterQuery(query).get should equal("((id1) OR (id2))")
  }

  test("Test filter query mapping with child fields") {
    implicit val qParams: mutable.Map[String, Seq[String]] = mutable.Map.empty
    val filter = Filter(Map(EtField.Kind -> Term("Fact"), EtField.Name -> Term("somename")))

    val termMapper = mock[SolrFilterTermMapper]
    (termMapper
      .buildFilterQuery(_: FilterTerm)(_: SolrQueryService))
      .expects(Term("Fact"), *)
      .returning(Try("(Fact)"))
    (termMapper
      .buildFilterQuery(_: FilterTerm)(_: SolrQueryService))
      .expects(Term("somename"), *)
      .returning(Try("(somename)"))

    val sut = new SolrFilterMapper(termMapper)
    val result = sut.buildFilter(filter).get
    result should equal("( +{!parent which=cmd_kind:* filters=$childfq0})")
    qParams.keySet should contain theSameElementsAs List("childfq0")
    qParams("childfq0") should contain theSameElementsAs List("thy_kind:(Fact)", "name:(somename)")
  }

  test("Test filter query mapping with parent and child fields") {
    implicit val qParams: mutable.Map[String, Seq[String]] = mutable.Map.empty
    val filter =
      Filter(Map(EtField.Id -> Term("id1"), EtField.StartPosition -> Term("42"), EtField.Proposition -> Term("*")))

    val termMapper = mock[SolrFilterTermMapper]
    (termMapper.buildFilterQuery(_: FilterTerm)(_: SolrQueryService)).expects(Term("id1"), *).returning(Try("(id1)"))
    (termMapper.buildFilterQuery(_: FilterTerm)(_: SolrQueryService)).expects(Term("42"), *).returning(Try("(42)"))
    (termMapper.buildFilterQuery(_: FilterTerm)(_: SolrQueryService)).expects(Term("*"), *).returning(Try("(*)"))

    val sut = new SolrFilterMapper(termMapper)
    val result = sut.buildFilter(filter).get
    result should equal(
      "(id:(id1) AND +start_pos:(42) +{!parent which=cmd_kind:* filters=$childfq0})")
    qParams.keySet should contain theSameElementsAs List("childfq0")
    qParams("childfq0") should contain theSameElementsAs List("prop:(*)")
  }
}
