package de.qaware.findfacts.importer.steps

import scala.collection.mutable

import org.scalatest.{BeforeAndAfterEach, OptionValues}
import de.qaware.findfacts.common.dt.{CodeblockEt, ConstantEt, FactEt, TypeEt}
import de.qaware.findfacts.importer.TheoryView.Position
import org.scalatest.matchers.should.Matchers

class StepContextTest extends org.scalatest.funsuite.AnyFunSuite with Matchers with BeforeAndAfterEach with OptionValues {

  val block: CodeblockEt = CodeblockEt("thy1.0.10", "thy1", 1, "lemma", "...", "src", "other stuff", List.empty)
  val const: ConstantEt = ConstantEt("Const.thy1.some_const", "some_const", List.empty, "'a -> 'a")
  val fact: FactEt = FactEt("Fact.thy1.some_fact", "some_fact", List.empty)
  val typ: TypeEt = TypeEt("Typ.thy1.some_type", "some_type", List.empty)
  val pos: Position = new Position {
    override def offset: Int = 2
    override def endOffset: Int = 7
  }
  val pos1: Position = new Position {
    override def offset: Int = 2
    override def endOffset: Int = 7
  }

  def context: StepContext = {
    val ctx = StepContext()

    ctx.blocks.add(block)
    ctx.consts.put(pos, mutable.Set(const))
    ctx.facts.put(pos, mutable.Set(fact))
    ctx.types.put(pos1, mutable.Set(typ))

    ctx
  }

  test("Test accessors") {
    val ctx = context

    ctx.theoryEts should contain theSameElementsAs List(const, fact, typ)
    ctx.theoryEtsByPosition.keys should contain theSameElementsAs List(pos)
    ctx.theoryEtsByPosition.values.flatten should contain theSameElementsAs List(const, fact, typ)
    ctx.allEts should contain theSameElementsAs List(const, fact, typ, block)
  }
}
