package de.qaware.findfacts.core.solrimpl

import scala.util.Success

import org.mockito.{ArgumentMatchersSugar, MockitoSugar}
import org.scalatest.{FunSuite, Matchers}

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.common.dt.EtField.Uses
import de.qaware.findfacts.common.solr.mapper.FromSolrDoc
import de.qaware.findfacts.core
import de.qaware.findfacts.core.QueryService.ResultList
import de.qaware.findfacts.core.{Exact, FieldFilter, InRange, InResult, Term}

class SolrFilterMapperTest extends FunSuite with Matchers with MockitoSugar with ArgumentMatchersSugar {

  implicit val queryService: SolrQueryService = mock[SolrQueryService]
  implicit val index: String = "default"

  val sut = new SolrFilterMapper()

  test("Test filter mappings for terminal filters") {
    sut.mapFilter(Term("*gauss jordan*")).get should equal("(*gauss\\ jordan*)")
    sut.mapFilter(Term("")).get should equal("\"\"")
    sut.mapFilter(Exact("Stuff*")).get should equal("\"Stuff\\*\"~10")
    sut.mapFilter(InRange(10, 42)).get should equal("[10 TO 42]")
  }

  test("Test filter mapping for bool connectives") {
    val term = core.And(Term("1"), core.Or(core.Not(Term("2")), Term("3")), Term("4"))

    sut.mapFilter(term).get should equal("((1)&&((* !(2))||(3))&&(4))")
  }

  test("Test filter mapping for recursive query") {
    val subQ = List(FieldFilter(EtField.Kind, Term("sub")))
    val res = Success(ResultList(Vector(Uses(List("id1", "id2")), Uses(List("id3"))), 2, ""))

    when(queryService.getResults(any)(eqTo(index), any[FromSolrDoc[EtField.Uses.T]])) thenReturn res

    sut.mapFilter(InResult(Uses, subQ)).get should equal("(id1 id2 id3)")
  }

  test("Test string escaping") {
    val allAscii =
      " !\"#$%&\\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~&&||"

    sut.escape(allAscii, exact = true) should equal(
      "\\ \\!\\\"#$%&\\\\'\\(\\)\\*\\+,\\-.\\/0123456789\\:;<=>\\?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\[\\]\\^_`abcdefghijklmnopqrstuvwxyz\\{|\\}\\~\\&&\\||")
    sut.escape(allAscii, exact = false) should equal(
      "\\ \\!\\\"#$%&\\\\'\\(\\)*\\+,\\-.\\/0123456789\\:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\[\\]\\^_`abcdefghijklmnopqrstuvwxyz\\{|\\}\\~\\&&\\||")
  }
}
