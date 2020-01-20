package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.da.api.{ChildrenField, Field, MultiValuedField, SingleValuedField}
import de.qaware.findfacts.common.dt.solr.SolrSchema
import de.qaware.findfacts.common.utils.DefaultEnum
import enumeratum.EnumEntry
// scalastyle:off
import io.circe.generic.auto._
// scalastyle:on

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

  /** Kind of the entity. Possible values in [[Kind]]. */
  case object Kind extends SingleValuedField[EtKind] with ChildField {
    override val name: String = SolrSchema.Kind
    override val implicits = FieldImplicits()
  }

  /** Child theory entities. */
  case object ChildEntities extends ChildrenField[TheoryEt] with ParentField {
    override final val name = SolrSchema.Children
    override val implicits = FieldImplicits()
  }

  /** Child short entries. */
  case object ChildShorts extends ChildrenField[ShortThyEt] with ParentField {
    override final val name = SolrSchema.Children
    override val implicits = FieldImplicits()
  }

  case object ChildIds extends ChildrenField[IdChild] with ParentField {
    override val name: String = SolrSchema.Children
    override val implicits = FieldImplicits()
  }

  /** Source theory file containing the entity. */
  case object SourceFile extends SingleValuedField[String] with ParentField {
    override final val name = SolrSchema.SourceFile
    override val implicits = FieldImplicits()
  }

  /** Start position of entity definition, in absolute isabelle characters. */
  case object StartPosition extends SingleValuedField[Int] with ParentField {
    override final val name = SolrSchema.StartPosition
    override val implicits = FieldImplicits()
  }

  /** End position of entity definition, in absolute isabelle characters. */
  case object EndPosition extends SingleValuedField[Int] with ParentField {
    override final val name = SolrSchema.EndPosition
    override val implicits = FieldImplicits()
  }

  /** Long (in most cases fully-qualified) name. */
  case object Name extends SingleValuedField[String] with ChildField {
    override final val name = SolrSchema.Name
    override val implicits = FieldImplicits()
  }

  /** Proposition, of a constants, fact, or type entity. */
  case object Proposition extends SingleValuedField[String] with ChildField {
    override final val name = SolrSchema.Proposition
    override val implicits = FieldImplicits()
  }

  /** Source text in isabelle thy. */
  case object SourceText extends SingleValuedField[String] with ParentField {
    override final val name = SolrSchema.SourceText
    override val implicits = FieldImplicits()
  }

  /** Type of a constant entity. */
  case object ConstantType extends SingleValuedField[String] with ChildField {
    override final val name = SolrSchema.ConstantType
    override val implicits = FieldImplicits()
  }

  /** Other entities that this entity uses in its propositions. */
  case object PropositionUses extends MultiValuedField[String] with ChildField {
    override final val name = SolrSchema.PropositionUses
    override val implicits = FieldImplicits()
  }

  /** Other entities that this entity uses in its types. */
  case object TypeUses extends MultiValuedField[String] with ChildField {
    override final val name = SolrSchema.TypeUses
    override val implicits = FieldImplicits()
  }

  /** Other entities that this entity uses in its proofs. */
  case object ProofUses extends MultiValuedField[String] with ChildField {
    override final val name = SolrSchema.ProofUses
    override val implicits = FieldImplicits()
  }

  /** Kind of a documentation entity. Possible values in [[DocumentationKind]]. */
  case object DocumentationKind extends SingleValuedField[DocKind] with ParentField {
    override final val name = SolrSchema.DocumentationKind
    override val implicits = FieldImplicits()
  }
}
