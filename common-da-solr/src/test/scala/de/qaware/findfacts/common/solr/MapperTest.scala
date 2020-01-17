package de.qaware.findfacts.common.solr

import de.qaware.findfacts.common.da.api.Variant.Discriminator
import de.qaware.findfacts.common.da.api.{ChildrenField, Field, MultiValuedField, SingleValuedField, Variant}
import de.qaware.findfacts.common.solr.mapper._
import de.qaware.findfacts.common.utils.DefaultEnum
import enumeratum.EnumEntry
import org.scalatest.FunSuite
import io.circe.generic.auto._

class MapperTest extends FunSuite {
  case object SingleField extends SingleValuedField[String] {
    override val name: String = "Single"
    override val implicits = FieldImplicits()
  }
  case object MultiField extends MultiValuedField[Int] {
    override val name: String = "Multi"
    override val implicits = FieldImplicits()
  }
  case object ChildField extends ChildrenField[SimpleTestEt] {
    override val name: String = "Child"
    override val implicits = FieldImplicits()
  }
  sealed trait TestKind extends EnumEntry
  object Kinds extends DefaultEnum[TestKind] {
    override final val values = findValues
    case object A extends Value
    case object B extends Value
  }
  case object VariantField extends SingleValuedField[TestKind] {
    override val name: String = "Variant"
    override val implicits = FieldImplicits()
  }

  case class SimpleTestEt(name: SingleField.FieldType, nums: MultiField.FieldType)

  test("simple mapper generation") {
    assertCompiles("FromSolrDoc[SimpleTestEt]")
    assertCompiles("ToSolrDoc[SimpleTestEt]")
  }

  test("nested mapper generation") {
    case class NestedTestEt(name: SingleField.FieldType, children: ChildField.FieldType)

    assertCompiles("FromSolrDoc[NestedTestEt]")
    assertCompiles("ToSolrDoc[NestedTestEt]")
  }

  test("variant mapper generation") {
    sealed trait Base
    case class VariantA(name: SingleField.FieldType)
        extends Base
        with Discriminator[TestKind, VariantField.type, Kinds.A.type]

    case class VariantB(nums: MultiField.FieldType)
        extends Base
        with Discriminator[TestKind, VariantField.type, Kinds.B.type]

    assertCompiles("FromSolrDoc[Base]")
    assertCompiles("ToSolrDoc[Base]")
  }
}
