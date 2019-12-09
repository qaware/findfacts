package de.qaware.findfacts.common.dt

import org.scalatest.{FunSuite, Matchers, TryValues}

class EtKindTest extends FunSuite with Matchers with TryValues {
  test("from string") {
    val result = EtKind.fromString("Constant").success.value
    result should equal(EtKind.Constant)
  }

  test("from invalid should fail") {
      EtKind.fromString("invalid").isFailure should be(true)
  }
}
