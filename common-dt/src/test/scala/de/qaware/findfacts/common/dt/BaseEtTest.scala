package de.qaware.findfacts.common.dt

import org.scalatest.{FunSuite, Matchers}

class BaseEtTest extends FunSuite with Matchers {
  val block = new CodeblockEt(0, 1, "B", 2, "cmd", "bef", "Src", "after")
  val const = new ConstantEt("C", Nil, "")
  val fact = new FactEt("F", Nil)
  val typ = new TypeEt("T", Nil)

  test("Test ID generation") {
    block.id should equal("B.0.1")
    const.id should equal("Constant.C")
    fact.id should equal("Fact.F")
    typ.id should equal("Type.T")
  }
}
