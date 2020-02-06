package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.da.api.Variant.Discriminator
import de.qaware.findfacts.common.dt
// scalastyle:off
import de.qaware.findfacts.common.dt.EtField._
// scalastyle:on

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

final case class CodeblockEt private (
    override val id: Id.FieldType,
    /** Source in which entity was defined. */
    sourceFile: SourceTheory.FieldType,
    /** Start pos of entity definition, in isabelle tokens. */
    startPosition: StartPosition.FieldType,
    /** End pos of entity definition, in isabelle tokens. */
    endPosition: EndPosition.FieldType,
    /** Source code of the entity. */
    sourceText: SourceText.FieldType,
    /** Entities from this bloock. */
    entities: ChildEntities.FieldType
) extends BaseEt
    with Discriminator[CmdKind, CommandKind.type, CmdKind.Codeblock.type] {
  require(id == s"$sourceFile.$startPosition")

  def this(
      sourceFile: SourceTheory.FieldType,
      startPosition: StartPosition.FieldType,
      endPosition: EndPosition.FieldType,
      sourceText: SourceText.FieldType) =
    this(s"$sourceFile.$startPosition", sourceFile, startPosition, endPosition, sourceText, List.empty)
}

/** Documentation.
  *
  * @param documentationKind kind of documentation
  */
final case class DocumentationEt private (
    override val id: Id.FieldType,
    sourceFile: SourceTheory.FieldType,
    startPosition: StartPosition.FieldType,
    endPosition: EndPosition.FieldType,
    sourceText: SourceText.FieldType,
    documentationKind: DocumentationKind.FieldType
) extends BaseEt
    with Discriminator[CmdKind, CommandKind.type, dt.CmdKind.Documentation.type] {
  require(id == s"$sourceFile.$startPosition.$documentationKind")

  def this(
      sourceFile: SourceTheory.FieldType,
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
    constantType: ConstantType.FieldType
) extends TheoryEt
    with Discriminator[ThyEtKind, Kind.type, ThyEtKind.Constant.type] {
  require(id == s"${ThyEtKind.Constant}.$name")

  def this(
      name: Name.FieldType,
      proposition: Proposition.FieldType,
      propositionUses: PropositionUses.FieldType,
      typeUses: TypeUses.FieldType,
      constantType: ConstantType.FieldType
  ) = this(s"${ThyEtKind.Constant}.$name", name, proposition, propositionUses, typeUses, constantType)
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
    proofUses: ProofUses.FieldType
) extends TheoryEt
    with Discriminator[ThyEtKind, Kind.type, ThyEtKind.Fact.type] {
  require(id == s"${ThyEtKind.Fact}.$name")

  def this(
      name: Name.FieldType,
      proposition: Proposition.FieldType,
      propositionUses: PropositionUses.FieldType,
      proofUses: ProofUses.FieldType
  ) = this(s"${ThyEtKind.Fact}.$name", name, proposition, propositionUses, proofUses)
}

/** Type entity. */
final case class TypeEt private (
    override val id: Id.FieldType,
    override val name: Name.FieldType,
    override val proposition: Proposition.FieldType,
    override val propositionUses: PropositionUses.FieldType
) extends TheoryEt
    with Discriminator[ThyEtKind, Kind.type, ThyEtKind.Type.type] {
  require(id == s"${ThyEtKind.Type}.$name")

  def this(
      name: Name.FieldType,
      proposition: Proposition.FieldType,
      propositionUses: PropositionUses.FieldType
  ) = this(s"${ThyEtKind.Type}.$name", name, proposition, propositionUses)
}
