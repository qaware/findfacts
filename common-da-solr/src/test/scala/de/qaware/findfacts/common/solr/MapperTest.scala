package de.qaware.findfacts.common.solr

import enumeratum.EnumEntry
import io.circe.generic.auto._
import org.scalatest.FunSuite

import de.qaware.findfacts.common.da.api.Variant.Discriminator
import de.qaware.findfacts.common.da.api.{ChildrenField, MultiValuedField, SingleValuedField}
import de.qaware.findfacts.common.solr.mapper.{FromSolrDoc, ToSolrDoc}
import de.qaware.findfacts.common.utils.DefaultEnum

class MapperTest extends FunSuite {

  case object SingleField extends SingleValuedField[String] {
    override val name: String = "Single"
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  case object MultiField extends MultiValuedField[Int] {
    override val name: String = "Multi"
    override val implicits: FieldImplicits[Int] = FieldImplicits()
  }
  case object ChildField extends ChildrenField[SimpleTestEt] {
    override val name: String = "Child"
    override val implicits: FieldImplicits[SimpleTestEt] = FieldImplicits()
  }
  sealed trait TestKind extends EnumEntry
  object Kinds extends DefaultEnum[TestKind] {
    override final val values = findValues
    override final val names = findNames
    case object A extends TestKind
    case object B extends TestKind
  }
  case object VariantField extends SingleValuedField[TestKind] {
    override val name: String = "Variant"
    override val implicits: FieldImplicits[TestKind] = FieldImplicits()
  }

  case class SimpleTestEt(name: SingleField.T, nums: MultiField.T)

  test("simple mapper generation") {
    assertCompiles("FromSolrDoc[SimpleTestEt]")
    assertCompiles("ToSolrDoc[SimpleTestEt]")
  }

  test("nested mapper generation") {
    case class NestedTestEt(name: SingleField.T, children: ChildField.T)

    assertCompiles("FromSolrDoc[NestedTestEt]")
    assertCompiles("ToSolrDoc[NestedTestEt]")
  }

  test("variant mapper generation") {
    sealed trait Base
    case class VariantA(name: SingleField.T) extends Base with Discriminator[TestKind, VariantField.type, Kinds.A.type]

    case class VariantB(nums: MultiField.T) extends Base with Discriminator[TestKind, VariantField.type, Kinds.B.type]

    assertCompiles("FromSolrDoc[Base]")
    assertCompiles("ToSolrDoc[Base]")
  }
}
