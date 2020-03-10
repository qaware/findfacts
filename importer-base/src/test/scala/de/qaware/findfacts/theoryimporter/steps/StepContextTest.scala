package de.qaware.findfacts.theoryimporter.steps

import de.qaware.findfacts.common.dt.{CodeblockEt, ConstantEt, DocKind, DocumentationEt, FactEt, TypeEt}
import org.scalatest.{BeforeAndAfterEach, Matchers, OptionValues}

class StepContextTest extends org.scalatest.FunSuite with Matchers with BeforeAndAfterEach with OptionValues {
  val block1 = new CodeblockEt("src", 1, 2, "")
  val block2 = new CodeblockEt("src", 2, 3, "")
  val const = new ConstantEt("Const", "prop", List.empty, List.empty, "()")
  val fact = new FactEt("Fact", "prop", List.empty, List.empty)
  def typ = new TypeEt("Typ", "prop", List.empty)
  def doc = new DocumentationEt("src", 3, 4, "", DocKind.Latex)

  def context: StepContext = {
    val ctx = StepContext()
    ctx.addEntity(const, block1)
    ctx.addEntity(fact, block1)
    ctx.addEntity(typ, block2)
    ctx.addEntity(doc)
    ctx
  }

  test("Test parent-chlild relation") {
    val ctx = context

    ctx.blocks should have size 2
    val block1 = ctx.blocks.find(_.id == "src.1")
    block1.value.entities should contain theSameElementsAs List(const, fact)

    val block2 = ctx.blocks.find(_.id == "src.2")
    block2.value.entities should contain theSameElementsAs List(typ)
  }

  test("Test accessors") {
    val ctx = context

    ctx.consts should contain theSameElementsAs List(const)
    ctx.facts should contain theSameElementsAs List(fact)
    ctx.types should contain theSameElementsAs List(typ)
    ctx.theoryEntities should contain theSameElementsAs List(const, fact, typ)
    ctx.docs should contain theSameElementsAs List(doc)
    ctx.blocks should have size 2
    ctx.allEntities should have size 6
  }

  test("Test multiple parents") {
    val ctx = StepContext()
    ctx.addEntity(const, block1)
    ctx.addEntity(const, block2)

    ctx.blocks should have size 2
    ctx.blocks.head.entities should contain theSameElementsAs List(const)
    ctx.blocks.tail.head.entities should contain theSameElementsAs List(const)
  }

  test("Test update") {
    val ctx = context
    val newTyp = typ.copy(uses = List("something"))

    ctx.updateEntity(typ, newTyp)
    ctx.types should contain theSameElementsAs List(newTyp)
    ctx.blocks.find(_.id == "src.2").value.entities.filter(_.id == typ.id) should contain theSameElementsAs List(newTyp)
  }

  test("Test update illegal id change") {
    val newIdConst = const.copy(id = "Constant.new", name = "new")
    assertThrows[IllegalArgumentException](context.updateEntity(const, newIdConst))
  }

  test("Test update non-existing") {
    val nonExisting = new TypeEt("nonexisting", "", List.empty)
    assertThrows[IllegalArgumentException](context.updateEntity(nonExisting, nonExisting.copy(proposition = "prop")))
  }
}
