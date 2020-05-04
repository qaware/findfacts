package de.qaware.findfacts.common.dt

import enumeratum.EnumEntry
import io.swagger.annotations.ApiModel

import de.qaware.findfacts.common.da.api.{ChildrenField, DocumentKind, Field, MultiValuedField, SingleValuedField}
import de.qaware.findfacts.common.dt.solr.SolrSchema
import de.qaware.findfacts.common.utils.DefaultEnum

/** Seal field types so only existing fields can be used. */
@ApiModel(description = EtField.names)
sealed trait EtField extends EnumEntry with Field {

  /** Flag for parent fields. */
  val isParent: Boolean = true

  /** Flag for child fields. */
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

/**
 * Field for child entities.
 *
 * @tparam A Children type
 */
abstract class Children[A] extends ChildrenField[A] with ParentField {
  override val name: String = SolrSchema.Children
}

/** Type-level representation of parent fields. */
object EtField extends DefaultEnum[EtField] {
  override final val names = findNames
  override final val values = findValues

  /** Unique id. */
  case object Id extends SingleValuedField[String] with EtField {
    override val name: String = SolrSchema.Id
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Document kind. */
  case object DocKind extends SingleValuedField[DocumentKind] with EtField {
    override val name: String = SolrSchema.DocKind
    override val implicits: FieldImplicits[DocumentKind] = FieldImplicits()
  }

  /** Id of child document. */
  case object ChildId extends SingleValuedField[String] with ChildField {
    override val name: String = SolrSchema.Id
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Kind of command span. */
  case object Command extends SingleValuedField[String] with ParentField {
    override val name: String = SolrSchema.Command
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Source text in theory before this code block. */
  case object SourceCodeBefore extends SingleValuedField[String] with ParentField {
    override val name: String = SolrSchema.SourceCodeBefore
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Source text in isabelle thy. */
  case object SourceCode extends SingleValuedField[String] with ParentField {
    override final val name = SolrSchema.SourceCode
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Source text in theory after this code block. */
  case object SourceCodeAfter extends SingleValuedField[String] with ParentField {
    override val name: String = SolrSchema.SourceCodeAfter
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Source theory file containing the entity. */
  case object SourceTheory extends SingleValuedField[String] with ParentField {
    override final val name = SolrSchema.SourceTheory
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Facetable field for source theory. */
  case object SourceTheoryFacet extends SingleValuedField[String] with ParentField {
    override final val name = SolrSchema.SourceTheoryFacet
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Start position of entity definition, in absolute isabelle characters. */
  case object StartLine extends SingleValuedField[Int] with ParentField {
    override final val name = SolrSchema.StartLine
    override val implicits: FieldImplicits[Int] = FieldImplicits()
  }

  /** Kind of theory entity. Possible values in [[Kind]]. */
  case object Kind extends SingleValuedField[Kind] with ChildField {
    override final val name = SolrSchema.TheoryKind
    override val implicits: FieldImplicits[Kind] = FieldImplicits()
  }

  /** Long (in most cases fully-qualified) name. */
  case object Name extends SingleValuedField[String] with ChildField {
    override final val name = SolrSchema.Name
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Facetable field for name. */
  case object NameFacet extends SingleValuedField[String] with ChildField {
    override final val name = SolrSchema.NameFacet
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Type of a constant entity. */
  case object ConstantType extends SingleValuedField[String] with ChildField {
    override final val name = SolrSchema.ConstantType
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Facetable field for a constant entity. */
  case object ConstantTypeFacet extends SingleValuedField[String] with ChildField {
    override final val name = SolrSchema.ConstantTypeFacet
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }

  /** Other entities that this entity uses. */
  case object Uses extends MultiValuedField[String] with ChildField {
    override final val name = SolrSchema.Uses
    override val implicits: FieldImplicits[String] = FieldImplicits()
  }
}
