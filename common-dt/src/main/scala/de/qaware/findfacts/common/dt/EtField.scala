package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.da.api.{ChildrenField, Field, MultiValuedField, SingleValuedField}
import de.qaware.findfacts.common.dt.solr.SolrSchema
import de.qaware.findfacts.common.utils.DefaultEnum
import enumeratum.EnumEntry

/** Seal field types so only existing fields can be used. */
sealed trait EtField extends EnumEntry with Field {
  val isParent: Boolean = true
  val isChild: Boolean = true
}

/** Parent-only field. */
sealed trait ParentField extends EtField {
  override val isChild: Boolean = false
}

/** Child-only field. */
sealed trait ChildField extends EtField {
  override val isParent: Boolean = false
}

/** Typelevel representation of parent fields. */
object EtField extends DefaultEnum[EtField] {
  override final val values = findValues

  /** Unique id. */
  case object Id extends SingleValuedField[String] with EtField {
    override final val name = SolrSchema.Id
    override val implicits = FieldImplicits()
  }

  /** Kind of command span. */
  case object Command extends SingleValuedField[String] with ParentField {
    override val name = SolrSchema.Command
    override val implicits = FieldImplicits()
  }

  /** Source text in theory before this code block. */
  case object SourceTextBefore extends SingleValuedField[String] with ParentField {
    override val name: String = SolrSchema.SourceTextBefore
    override val implicits = FieldImplicits()
  }

  /** Source text in isabelle thy. */
  case object SourceText extends SingleValuedField[String] with ParentField {
    override final val name = SolrSchema.SourceText
    override val implicits = FieldImplicits()
  }

  /** Sourc text in theory after this code block. */
  case object SourceTextAfter extends SingleValuedField[String] with ParentField {
    override val name: String = SolrSchema.SourceTextAfter
    override val implicits = FieldImplicits()
  }

  /** Source theory file containing the entity. */
  case object SourceTheory extends SingleValuedField[String] with ParentField {
    override final val name = SolrSchema.SourceTheory
    override val implicits = FieldImplicits()
  }

  /** Start position of entity definition, in absolute isabelle characters. */
  case object StartLine extends SingleValuedField[Int] with ParentField {
    override final val name = SolrSchema.StartLine
    override val implicits = FieldImplicits()
  }

  /** Field for child entities.
    *
    * @tparam A Children type
    */
  abstract class Children[A] extends ChildrenField[A] with ParentField {
    override val name: String = SolrSchema.Children
  }

  /** Kind of theory entity. Possible values in [[Kind]]. */
  case object Kind extends SingleValuedField[Kind] with ChildField {
    override final val name = SolrSchema.TheoryKind
    override val implicits = FieldImplicits()
  }

  /** Long (in most cases fully-qualified) name. */
  case object Name extends SingleValuedField[String] with ChildField {
    override final val name = SolrSchema.Name
    override val implicits = FieldImplicits()
  }

  case object NameFacet extends SingleValuedField[String] with ChildField {
    override final val name = SolrSchema.NameFacet
    override val implicits = FieldImplicits()
  }

  /** Type of a constant entity. */
  case object ConstantType extends SingleValuedField[String] with ChildField {
    override final val name = SolrSchema.ConstantType
    override val implicits = FieldImplicits()
  }

  /** Facetable field for a constant entity. */
  case object ConstantTypeFacet extends SingleValuedField[String] with ChildField {
    override final val name = SolrSchema.ConstantTypeFacet
    override val implicits = FieldImplicits()
  }

  /** Other entities that this entity uses. */
  case object Uses extends MultiValuedField[String] with ChildField {
    override final val name = SolrSchema.Uses
    override val implicits = FieldImplicits()
  }
}
