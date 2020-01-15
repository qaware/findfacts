package de.qaware.findfacts.common.dt

import io.circe.Encoder
import io.circe.generic.auto._
import org.scalatest.{FunSuite, Matchers}

class FieldTest extends FunSuite with Matchers {
  test("Test field implicits") {
    assertCompiles("case object Test extends MultiValuedField[Float] { override val implicits = FieldImplicits() }")
    assertDoesNotCompile(
      "case object Test extends MultiValuedField[Object] { override val implicits = FieldImplicits() }")
  }

  test("Test single valued field json") {
    final case object TestSingleField extends SingleValuedField[Float] { override val implicits = FieldImplicits() }

    TestSingleField.fromJsonString("1.0") should be(1.0)
    TestSingleField.valueEncoder(1.5F).noSpaces should equal("1.5")
  }

  test("Test multi-valued field json") {
    case object TestMultiField extends MultiValuedField[Int] { override val implicits = FieldImplicits() }

    TestMultiField.fromJsonString("42") should be(42)
    TestMultiField.valueEncoder(List(7, 8)).noSpaces should equal("[7,8]")
  }

  test("Test optional field json") {
    case object TestOptionField extends OptionalField[String] { override val implicits = FieldImplicits() }

    TestOptionField.fromJsonString("str") should be("str")
    TestOptionField.valueEncoder(None).asString shouldBe empty
    TestOptionField.valueEncoder(Some("s")).noSpaces should equal("\"s\"")
  }

  test("Test children field json") {
    case class TestChild(name: String, count: Int)

    case object TestChildrenField extends ChildrenField[TestChild] { override val implicits = FieldImplicits() }

    TestChildrenField.valueEncoder(List(TestChild("Name", 3))).noSpaces should equal(
      "[{\"name\":\"Name\",\"count\":3}]")
  }
}
