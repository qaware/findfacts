package de.qaware.findfacts.importer.steps.impl.thy

import org.mockito.MockitoSugar
import org.scalatest.{FunSuite, Matchers}

import de.qaware.findfacts.importer.TheoryView
import de.qaware.findfacts.importer.steps.impl.pure.PureSyntax

class TypExtractorTest extends FunSuite with Matchers with MockitoSugar {
  val nameExtractor = mock[NameExtractor]
  val typExtractor = new TypExtractor(nameExtractor)

  case class TCtor(override val name: String, override val args: List[TheoryView.Typ]) extends TheoryView.TypeTyp

  object Fun {
    def apply(_args: List[TheoryView.Typ]) = TCtor(PureSyntax.Fun.name, _args)
  }

  case class Free(override val name: String, override val sort: List[String] = Nil) extends TheoryView.TFree

  test("Check pretty print multi-arg functions") {
    val simple = Fun(List(Free("'a"), Free("'b"), Free("'c")))
    typExtractor.prettyPrint(simple) should equal("'a ⇒ 'b ⇒ 'c")

    val bracket = Fun(List(Fun(List(Free("'a"), Free("'b"))), Free("'c")))
    typExtractor.prettyPrint(bracket) should equal("('a ⇒ 'b) ⇒ 'c")
  }

  test("Check pretty print nested types") {
    val nestedSingle = TCtor("Set", List(TCtor("Map", List(TCtor("Bool", Nil)))))
    typExtractor.prettyPrint(nestedSingle) should equal("(Bool Map) Set")

    val nestedMultiple = TCtor("Tree", List(Free("'a"), Free("'b", List("Trait"))))
    typExtractor.prettyPrint(nestedMultiple) should equal("('a, 'b :: Trait) Tree")
  }
}
