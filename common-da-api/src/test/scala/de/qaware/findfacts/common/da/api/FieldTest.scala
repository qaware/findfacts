package de.qaware.findfacts.common.da.api

import io.circe.generic.auto._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FieldTest extends AnyFunSuite with Matchers {

  test("Test field implicits") {
    assertCompiles("case object Test extends MultiValuedField[Float] { override val implicits = FieldImplicits() }")
    assertDoesNotCompile(
      "case object Test extends MultiValuedField[Object] { override val implicits = FieldImplicits() }")
  }

  test("Test single valued field json") {
    final case object TestSingleField extends SingleValuedField[Float] {
      override val name: String = "test_single_field"
      override val implicits: FieldImplicits[Float] = FieldImplicits()
    }

    TestSingleField.fromJsonString("1.0") should be(1.0)
    TestSingleField.valueEncoder(1.5f).noSpaces should equal("1.5")
  }

  test("Test multi-valued field json") {
    case object TestMultiField extends MultiValuedField[Int] {
      override val name: String = "test_multi_field"
      override val implicits: FieldImplicits[Int] = FieldImplicits()
    }

    TestMultiField.fromJsonString("42") should be(42)
    TestMultiField.valueEncoder(List(7, 8)).noSpaces should equal("[7,8]")
  }

  test("Test optional field json") {
    case object TestOptionField extends OptionalField[String] {
      override val name: String = "test_option_field"
      override val implicits: FieldImplicits[String] = FieldImplicits()
    }

    TestOptionField.fromJsonString("str") should be("str")
    TestOptionField.valueEncoder(None).asString shouldBe empty
    TestOptionField.valueEncoder(Some("s")).noSpaces should equal("\"s\"")
  }

  test("Test children field json") {
    case class TestChild(name: String, count: Int)
    case object TestChildrenField extends ChildrenField[TestChild] {
      override val name: String = "test_children_field"
      override val implicits: FieldImplicits[TestChild] = FieldImplicits()
    }

    TestChildrenField.valueEncoder(List(TestChild("Name", 3))).noSpaces should equal(
      "[{\"name\":\"Name\",\"count\":3}]"
    )
  }
}
