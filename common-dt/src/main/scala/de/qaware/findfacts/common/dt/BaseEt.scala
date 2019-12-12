package de.qaware.findfacts.common.dt

// scalastyle:off
import de.qaware.findfacts.common.dt.EtField._
import shapeless.tag.Tagged
// scalastyle:on

/** Fields for all entities. */
sealed trait BaseEt {

  /** Unique id. */
  val id: Id.FieldType

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
sealed trait TheoryEt extends BaseEt {

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
final case class ConstantEt(
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
) extends TheoryEt
    with Tagged[EtKind.Constant.type]

/** Documentation.
  *
  * @param documentationKind kind of documentation
  */
final case class DocumentationEt(
    override val id: Id.FieldType,
    override val sourceFile: SourceFile.FieldType,
    override val startPosition: StartPosition.FieldType,
    override val endPosition: EndPosition.FieldType,
    override val sourceText: SourceText.FieldType,
    documentationKind: DocumentationKind.FieldType)
    extends BaseEt
    with Tagged[EtKind.Documentation.type]

/** Any fact.
  *
  * @param proofUses entities that the proof references
  */
final case class FactEt(
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
) extends TheoryEt
    with Tagged[EtKind.Fact.type]

/** Type entity. */
final case class TypeEt(
    override val id: Id.FieldType,
    override val sourceFile: SourceFile.FieldType,
    override val startPosition: StartPosition.FieldType,
    override val endPosition: EndPosition.FieldType,
    override val sourceText: SourceText.FieldType,
    override val name: Name.FieldType,
    override val proposition: Proposition.FieldType,
    override val propositionUses: PropositionUses.FieldType,
    override val related: Related.FieldType
) extends TheoryEt
    with Tagged[EtKind.Type.type]
// scalastyle:on
