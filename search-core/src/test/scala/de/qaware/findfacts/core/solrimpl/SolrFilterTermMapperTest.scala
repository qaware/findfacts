package de.qaware.findfacts.core.solrimpl

import scala.collection.mutable

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.{Exact, InRange, Term}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfterEach, FunSuite, Matchers}

class SolrFilterTermMapperTest extends FunSuite with Matchers with MockFactory with BeforeAndAfterEach {
  implicit val queryService: SolrQueryService = mock[SolrQueryService]
  implicit val qParams: mutable.Map[String, Seq[String]] = mutable.Map.empty

  override def beforeEach(): Unit = {
    qParams.clear()
  }

  test("Test string expression filter term mapping") {
    val sut = new SolrFilterTermMapper()

    val fq = Term("*gauss*")

    sut.mapFilterTerm(EtField.Name, fq).get should equal("({!v=$fq0})")
    qParams should have size 1
    qParams.keySet should contain("fq0")
    qParams should contain value List("filter(name:(*gauss*))")
  }

  test("Test range filter term mapping") {
    val sut = new SolrFilterTermMapper()

    val fq = InRange(10, 42)

    sut.mapFilterTerm(EtField.StartPosition, fq).get

    qParams should contain value List("filter(start_pos:[10 TO 42])")
  }

  test("Test term escaping") {
    val sut = new SolrFilterTermMapper()

    val allAscii =
      " !\"#$%&\\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~&&||"
    val fq = Exact(allAscii)
    sut.mapFilterTerm(EtField.Id, fq).get

    qParams should contain value List(
      "filter(id:\" \\!\\\"#$%&\\\\'\\(\\)*\\+,\\-.\\/0123456789\\:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\\[\\]\\^_`abcdefghijklmnopqrstuvwxyz\\{|\\}\\~\\&&\\||\")")
  }
}
