package de.qaware.findfacts.common.utils

import enumeratum.EnumEntry
import org.scalatest.TryValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class DefaultEnumTest extends AnyFunSuite with Matchers with TryValues {
  sealed trait TestEnum extends EnumEntry
  object TestEnum extends DefaultEnum[TestEnum] {
    override final val values = findValues
    override final val names = findNames

    case object VariantA extends TestEnum
    case object VariantB extends TestEnum
  }

  test("values") {
    TestEnum.values should contain theSameElementsAs List(TestEnum.VariantA, TestEnum.VariantB)
  }

  test("names") {
    TestEnum.names should equal("VariantA, VariantB")
  }

  test("from string") {
    val result = TestEnum.fromString("VariantA").success.value
    result should equal(TestEnum.VariantA)
  }

  test("from invalid should fail") {
    TestEnum.fromString("invalid").isFailure should be(true)
  }
}
