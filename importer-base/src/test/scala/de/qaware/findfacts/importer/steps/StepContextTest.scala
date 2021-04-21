package de.qaware.findfacts.importer.steps

import de.qaware.findfacts.common.dt.{CodeblockEt, ConstantEt, FactEt, TypeEt}
import de.qaware.findfacts.importer.TheoryView.Position
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterEach, OptionValues}

class StepContextTest
  extends org.scalatest.funsuite.AnyFunSuite
  with Matchers
  with BeforeAndAfterEach
  with OptionValues {

  val block: CodeblockEt = CodeblockEt("thy1.0.10", "thy1", 1, "lemma", "...", "src", "other stuff", List.empty)
  val const: ConstantEt = ConstantEt("Const.thy1.some_const", "some_const", List.empty, "'a -> 'a")
  val fact: FactEt = FactEt("Fact.thy1.some_fact", "some_fact", List.empty)
  val typ: TypeEt = TypeEt("Typ.thy1.some_type", "some_type", List.empty)
  val typ1: TypeEt = TypeEt("Typ.thy1.some_other_type", "some_other_type", List.empty)
  val pos: Position = new Position {
    override def offset: Int = 2
    override def endOffset: Int = 7
  }
  val pos1: Position = new Position {
    override def offset: Int = 2
    override def endOffset: Int = 7
  }

  test("Put entities at new positions") {
    val sut = StepContext()

    sut.allEts should be(empty)

    sut.putBlock(block)
    sut.allEts should contain theSameElementsAs List(block)

    sut.putConst(pos, const)
    sut.allEts should contain theSameElementsAs List(block, const)

    sut.putFact(pos, fact)
    sut.allEts should contain theSameElementsAs List(block, const, fact)

    sut.putType(pos, typ)
    sut.allEts should contain theSameElementsAs List(block, const, fact, typ)

    sut.putType(pos1, typ1)
    sut.allEts should contain theSameElementsAs List(block, const, fact, typ, typ1)
  }

  test("Put entities at same position") {
    val sut = StepContext()

    sut.putType(pos, typ)
    sut.putType(pos, typ1)

    sut.allEts should contain theSameElementsAs List(typ, typ1)
  }

  test("Test accessors") {
    val ctx = StepContext()

    ctx.putBlock(block)
    ctx.putConst(pos, const)
    ctx.putFact(pos, fact)
    ctx.putType(pos1, typ)

    ctx.theoryEts should contain theSameElementsAs List(const, fact, typ)
    ctx.theoryEtsByPosition.keys should contain theSameElementsAs List(pos)
    ctx.theoryEtsByPosition.values.flatten should contain theSameElementsAs List(const, fact, typ)
    ctx.allEts should contain theSameElementsAs List(const, fact, typ, block)
  }
}
