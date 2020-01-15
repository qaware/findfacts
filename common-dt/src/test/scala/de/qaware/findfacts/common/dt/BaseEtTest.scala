package de.qaware.findfacts.common.dt

import org.scalatest.{FunSuite, Matchers}

class BaseEtTest extends FunSuite with Matchers {
  val block = new BlockEt("B", 0, 0, "")
  val const = new ConstantEt("C", "", Nil, Nil, "")
  val doc = new DocumentationEt("D", 0, 0, "", DocKind.Latex)
  val fact = new FactEt("F", "", Nil, Nil)
  val typ = new TypeEt("T", "", Nil)

  test("Test ID generation") {
    block.id should equal("B.0")
    const.id should equal("Constant.C")
    doc.id should equal("D.0.Latex")
    fact.id should equal("Fact.F")
    typ.id should equal("Type.T")
  }

  test("Test ID consistency") {
    assertThrows[IllegalArgumentException](block.copy(startPosition = 2))
    assertThrows[IllegalArgumentException](const.copy(name = "new"))
    assertThrows[IllegalArgumentException](doc.copy(documentationKind = DocKind.Inline))
    assertThrows[IllegalArgumentException](fact.copy(name = "new"))
    assertThrows[IllegalArgumentException](typ.copy(name = "new"))

    block.copy(endPosition = 2)
    const.copy(proposition = "prop")
    doc.copy(sourceText = "src")
    fact.copy(proofUses = List("fact"))
    typ.copy(proposition = "prop")
  }
}
