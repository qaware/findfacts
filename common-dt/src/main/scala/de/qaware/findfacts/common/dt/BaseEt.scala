package de.qaware.findfacts.common.dt

// scalastyle:off
import de.qaware.findfacts.common.dt.EtFields._
// scalastyle:on

/** Fields for all entities. */
trait BaseEt {

  /** Unique id. */
  val id: Id.FieldType

  /** Kind of the entity. */
  val kind: Kind.FieldType

  /** Source in which entity was defined. */
  val sourceFile: SourceFile.FieldType

  /** Start pos of entity definition, in isabelle tokens. */
  val startPosition: StartPosition.FieldType

  /** End pos of entity definition, in isabelle tokens. */
  val endPosition: EndPosition.FieldType

  /** Source code of the entity. */
  val sourceText: SourceText.FieldType
}

/** Fields for Entities that from the semantic theory. */
trait TheoryEt extends BaseEt {

  /** Name of the entity. */
  val name: Name.FieldType

  /** Proposition that the entity poses. */
  val proposition: Proposition.FieldType

  /** Other entities that the proposition of the entity uses. */
  val propositionUses: PropositionUses.FieldType

  /** Other entities defined at the same source positions as this entity. */
  val related: Related.FieldType
}

// scalastyle:off
/** Constants.
  *
  * @param constantType type of the constant
  * @param typeUses other entities that the type of the constant references
  */
case class ConstantEt(
    override val id: Id.FieldType,
    override val sourceFile: SourceFile.FieldType,
    override val startPosition: StartPosition.FieldType,
    override val endPosition: EndPosition.FieldType,
    override val name: Name.FieldType,
    override val proposition: Proposition.FieldType,
    override val sourceText: SourceText.FieldType,
    override val propositionUses: PropositionUses.FieldType,
    override val related: Related.FieldType,
    typeUses: TypeUses.FieldType,
    constantType: ConstantType.FieldType
) extends TheoryEt {
  override val kind: Kind.FieldType = EtKind.Constant.asInstanceOf[Kind.FieldType]
}

/** Documentation.
  *
  * @param documentationKind kind of documentation
  */
case class DocumentationEt(
    override val id: Id.FieldType,
    override val sourceFile: SourceFile.FieldType,
    override val startPosition: StartPosition.FieldType,
    override val endPosition: EndPosition.FieldType,
    override val sourceText: SourceText.FieldType,
    documentationKind: DocumentationKind.FieldType)
    extends BaseEt {
  override val kind: Kind.FieldType = EtKind.Documentation.asInstanceOf[Kind.FieldType]
}

/** Any fact.
  *
  * @param proofUses entities that the proof references
  */
case class FactEt(
    override val id: Id.FieldType,
    override val sourceFile: SourceFile.FieldType,
    override val startPosition: StartPosition.FieldType,
    override val endPosition: EndPosition.FieldType,
    override val sourceText: SourceText.FieldType,
    override val name: Name.FieldType,
    override val proposition: Proposition.FieldType,
    override val propositionUses: PropositionUses.FieldType,
    override val related: Related.FieldType,
    proofUses: ProofUses.FieldType
) extends TheoryEt {
  override val kind: Kind.FieldType = EtKind.Fact.asInstanceOf[Kind.FieldType]
}

/** Type entity. */
case class TypeEt(
    override val id: Id.FieldType,
    override val sourceFile: SourceFile.FieldType,
    override val startPosition: StartPosition.FieldType,
    override val endPosition: EndPosition.FieldType,
    override val sourceText: SourceText.FieldType,
    override val name: Name.FieldType,
    override val proposition: Proposition.FieldType,
    override val propositionUses: PropositionUses.FieldType,
    override val related: Related.FieldType
) extends TheoryEt {
  override val kind: Kind.FieldType = EtKind.Type.asInstanceOf[Kind.FieldType]
}
// scalastyle:on
