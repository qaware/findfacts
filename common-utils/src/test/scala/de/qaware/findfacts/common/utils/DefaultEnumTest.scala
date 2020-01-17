package de.qaware.findfacts.common.utils

import enumeratum.EnumEntry
import org.scalatest.{FunSuite, Matchers, TryValues}

class DefaultEnumTest extends FunSuite with Matchers with TryValues {
  sealed trait TestEnum extends EnumEntry
  object TestEnum extends DefaultEnum[TestEnum] {
    override final val values = findValues

    case object VariantA extends Value
    case object VariantB extends Value
  }

  test("values") {
    TestEnum.values should contain theSameElementsAs List(TestEnum.VariantA, TestEnum.VariantB)
  }

  test("from string") {
    val result = TestEnum.fromString("VariantA").success.value
    result should equal(TestEnum.VariantA)
  }

  test("from invalid should fail") {
    TestEnum.fromString("invalid").isFailure should be(true)
  }
}
