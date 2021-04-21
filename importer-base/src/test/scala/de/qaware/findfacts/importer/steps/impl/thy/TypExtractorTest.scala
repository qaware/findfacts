package de.qaware.findfacts.importer.steps.impl.thy

import org.mockito.MockitoSugar
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import de.qaware.findfacts.importer.TheoryView
import de.qaware.findfacts.importer.steps.impl.pure.PureSyntax

class TypExtractorTest extends AnyFunSuite with Matchers with MockitoSugar {
  val nameExtractor: NameExtractor = mock[NameExtractor]
  val typExtractor = new TypExtractor(nameExtractor)

  case class TCtor(override val name: String, override val args: List[TheoryView.Typ]) extends TheoryView.TypeTyp

  def tFun(_args: List[TheoryView.Typ]): TCtor = TCtor(PureSyntax.Fun.name, _args)

  case class Free(override val name: String, override val sort: List[String] = List.empty) extends TheoryView.TFree

  test("Check pretty print multi-arg functions") {
    val simple = tFun(List(Free("'a"), Free("'b"), Free("'c")))
    typExtractor.prettyPrint(simple) should equal("'a ⇒ 'b ⇒ 'c")

    val bracket = tFun(List(tFun(List(Free("'a"), Free("'b"))), Free("'c")))
    typExtractor.prettyPrint(bracket) should equal("('a ⇒ 'b) ⇒ 'c")
  }

  test("Check pretty print nested types") {
    val nestedSingle = TCtor("Set", List(TCtor("Map", List(TCtor("Bool", List.empty)))))
    typExtractor.prettyPrint(nestedSingle) should equal("(Bool Map) Set")

    val nestedMultiple = TCtor("Tree", List(Free("'a"), Free("'b", List("Trait"))))
    typExtractor.prettyPrint(nestedMultiple) should equal("('a, 'b :: Trait) Tree")
  }
}
