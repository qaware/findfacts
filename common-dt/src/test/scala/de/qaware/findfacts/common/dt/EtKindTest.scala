package de.qaware.findfacts.common.dt

import org.scalatest.{FunSuite, Matchers}

class EtKindTest extends FunSuite with Matchers {
  test("from string") {
    val result = EtKind.fromString("Constant")
    result should equal(EtKind.Constant)
  }

  test("from invalid should fail") {
    assertThrows[IllegalArgumentException] {
      EtKind.fromString("invalid")
    }
  }
}
