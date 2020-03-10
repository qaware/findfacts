package de.qaware.findfacts.theoryimporter.steps.impl.thy

import de.qaware.findfacts.theoryimporter.TheoryView
import de.qaware.findfacts.theoryimporter.TheoryView.{TFree, TypeTyp}
import de.qaware.findfacts.theoryimporter.steps.impl.pure.PureSyntax
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}

class TypExtractorTest extends FunSuite with Matchers with MockFactory {
  val nameExtractor = mock[NameExtractor]
  val typExtractor = new TypExtractor(nameExtractor)

  object TCtor {
    def apply(_name: String, _args: List[TheoryView.Typ]) = new TypeTyp {
      override def name: String = _name
      override def args: List[TheoryView.Typ] = _args
    }
  }
  object Fun {
    def apply(_args: List[TheoryView.Typ]) = TCtor(PureSyntax.Fun.name, _args)
  }
  object Free {
    def apply(_name: String, _sorts: List[String] = Nil) = new TFree {
      override def name: String = _name
      override def sort: List[String] = _sorts
    }
  }

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
