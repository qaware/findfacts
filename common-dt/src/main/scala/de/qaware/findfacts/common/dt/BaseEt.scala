package de.qaware.findfacts.common.dt

// scalastyle:off
import de.qaware.findfacts.common.da.api.Variant.Discriminator
import de.qaware.findfacts.common.dt.EtField._
// scalastyle:on
import shapeless.tag.Tagged

/** Fields for all entities. */
sealed trait BaseEt {

  /** Unique id. */
  val id: Id.FieldType
}

/** Fields for Entities that from the semantic theory. */
sealed trait TheoryEt extends BaseEt {

  /** Name of the entity. */
  val name: Name.FieldType

  /** Proposition that the entity poses. */
  val proposition: Proposition.FieldType

  /** Other entities that the proposition of the entity uses. */
  val propositionUses: PropositionUses.FieldType
}

final case class BlockEt private (
    override val id: Id.FieldType,
    /** Source in which entity was defined. */
    sourceFile: SourceFile.FieldType,
    /** Start pos of entity definition, in isabelle tokens. */
    startPosition: StartPosition.FieldType,
    /** End pos of entity definition, in isabelle tokens. */
    endPosition: EndPosition.FieldType,
    /** Source code of the entity. */
    sourceText: SourceText.FieldType,
    /** Entities from this bloock. */
    entities: Children.FieldType,
) extends BaseEt
    with Discriminator[EtKind, Kind.type, EtKind.Block.type] {
  require(id == s"$sourceFile.$startPosition")

  def this(
      sourceFile: SourceFile.FieldType,
      startPosition: StartPosition.FieldType,
      endPosition: EndPosition.FieldType,
      sourceText: SourceText.FieldType) =
    this(s"$sourceFile.$startPosition", sourceFile, startPosition, endPosition, sourceText, List.empty)
}

// scalastyle:off scaladoc
/** Constants.
  *
  * @param constantType type of the constant
  * @param typeUses other entities that the type of the constant references
  */
final case class ConstantEt private (
    override val id: Id.FieldType,
    override val name: Name.FieldType,
    override val proposition: Proposition.FieldType,
    override val propositionUses: PropositionUses.FieldType,
    typeUses: TypeUses.FieldType,
    constantType: ConstantType.FieldType,
) extends TheoryEt
    with Discriminator[EtKind, Kind.type, EtKind.Constant.type] {
  require(id == s"${EtKind.Constant}.$name")

  def this(
      name: Name.FieldType,
      proposition: Proposition.FieldType,
      propositionUses: PropositionUses.FieldType,
      typeUses: TypeUses.FieldType,
      constantType: ConstantType.FieldType
  ) = this(s"${EtKind.Constant}.$name", name, proposition, propositionUses, typeUses, constantType)
}

/** Documentation.
  *
  * @param documentationKind kind of documentation
  */
final case class DocumentationEt private (
    override val id: Id.FieldType,
    sourceFile: SourceFile.FieldType,
    startPosition: StartPosition.FieldType,
    endPosition: EndPosition.FieldType,
    sourceText: SourceText.FieldType,
    documentationKind: DocumentationKind.FieldType)
    extends BaseEt
    with Discriminator[EtKind, Kind.type, EtKind.Documentation.type] {
  require(id == s"$sourceFile.$startPosition.$documentationKind")

  def this(
      sourceFile: SourceFile.FieldType,
      startPosition: StartPosition.FieldType,
      endPosition: EndPosition.FieldType,
      sourceText: SourceText.FieldType,
      documentationKind: DocumentationKind.FieldType
  ) =
    this(
      s"$sourceFile.$startPosition.$documentationKind",
      sourceFile,
      startPosition,
      endPosition,
      sourceText,
      documentationKind)
}

/** Any fact.
  *
  * @param proofUses entities that the proof references
  */
final case class FactEt private (
    override val id: Id.FieldType,
    override val name: Name.FieldType,
    override val proposition: Proposition.FieldType,
    override val propositionUses: PropositionUses.FieldType,
    proofUses: ProofUses.FieldType,
) extends TheoryEt
    with Discriminator[EtKind, Kind.type, EtKind.Fact.type] {
  require(id == s"${EtKind.Fact}.$name")

  def this(
      name: Name.FieldType,
      proposition: Proposition.FieldType,
      propositionUses: PropositionUses.FieldType,
      proofUses: ProofUses.FieldType
  ) = this(s"${EtKind.Fact}.$name", name, proposition, propositionUses, proofUses)
}

/** Type entity. */
final case class TypeEt private (
    override val id: Id.FieldType,
    override val name: Name.FieldType,
    override val proposition: Proposition.FieldType,
    override val propositionUses: PropositionUses.FieldType,
) extends TheoryEt
    with Discriminator[EtKind, Kind.type, EtKind.Type.type] {
  require(id == s"${EtKind.Type}.$name")

  def this(
      name: Name.FieldType,
      proposition: Proposition.FieldType,
      propositionUses: PropositionUses.FieldType
  ) = this(s"${EtKind.Type}.$name", name, proposition, propositionUses)
}
