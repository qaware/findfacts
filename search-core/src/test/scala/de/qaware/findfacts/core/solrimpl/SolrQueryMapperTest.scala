package de.qaware.findfacts.core.solrimpl

import scala.collection.mutable
import scala.util.Try

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.core.{AnyInResult, Exact, Filter, FilterQuery, FilterTerm, InRange, Term}
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
    val fq = Exact(allAscii)
    sut.buildFilterQuery(fq).get should equal(
      "\" \\!\\\"#$%&\\\\'\\(\\)*\\+,\\-.\\/0123456789\\:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\[\\]\\^_`abcdefghijklmnopqrstuvwxyz\\{|\\}\\~\\&&\\||\"")
  }

  test("Test filter query mapping with child fields") {
    implicit val qParams: mutable.Map[String, Seq[String]] = mutable.Map.empty
    val filter = Filter(Map(EtField.Kind -> Exact("Fact"), EtField.Name -> Exact("somename")))

    val termMapper = mock[SolrFilterTermMapper]
    (termMapper
      .buildFilterQuery(_: FilterTerm)(_: SolrQueryService))
      .expects(Exact("Fact"), *)
      .returning(Try("(Fact)"))
    (termMapper
      .buildFilterQuery(_: FilterTerm)(_: SolrQueryService))
      .expects(Exact("somename"), *)
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
      Filter(Map(EtField.Id -> Exact("id1"), EtField.StartPosition -> Exact("42"), EtField.Proposition -> Exact("*")))

    val termMapper = mock[SolrFilterTermMapper]
    (termMapper.buildFilterQuery(_: FilterTerm)(_: SolrQueryService)).expects(Exact("id1"), *).returning(Try("(id1)"))
    (termMapper.buildFilterQuery(_: FilterTerm)(_: SolrQueryService)).expects(Exact("42"), *).returning(Try("(42)"))
    (termMapper.buildFilterQuery(_: FilterTerm)(_: SolrQueryService)).expects(Exact("*"), *).returning(Try("(*)"))

    val sut = new SolrFilterMapper(termMapper)
    val result = sut.buildFilter(filter).get
    result should equal(
      "(id:(id1) AND  +start_pos:(42) +{!parent which=cmd_kind:* filters=$childfq0})")
    qParams.keySet should contain theSameElementsAs List("childfq0")
    qParams("childfq0") should contain theSameElementsAs List("prop:(*)")
  }
}
