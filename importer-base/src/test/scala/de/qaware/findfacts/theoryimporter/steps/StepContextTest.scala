package de.qaware.findfacts.theoryimporter.steps

import de.qaware.findfacts.common.dt.{CodeblockEt, ConstantEt, FactEt, TypeEt}
import de.qaware.findfacts.theoryimporter.TheoryView.Position
import org.scalatest.{BeforeAndAfterEach, Matchers, OptionValues}

class StepContextTest extends org.scalatest.FunSuite with Matchers with BeforeAndAfterEach with OptionValues {
  val block = new CodeblockEt(0, 10, "thy1", 1, "lemma", "...", "src", "other stuff")
  val const = new ConstantEt("Const", List.empty, "'a -> 'a")
  val fact = new FactEt("Fact", List.empty)
  val typ = new TypeEt("Typ", List.empty)
  val pos = new Position {
    override def offset: Int = 2
    override def endOffset: Int = 7
  }
  val pos1 = new Position {
    override def offset: Int = 2
    override def endOffset: Int = 7
  }

  def context: StepContext = {
    val ctx = StepContext()

    ctx.blocks.add(block)
    ctx.consts.addBinding(pos, const)
    ctx.facts.addBinding(pos, fact)
    ctx.types.addBinding(pos1, typ)

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
