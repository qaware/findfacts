package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.da.api.Variant.Discriminator
import de.qaware.findfacts.common.dt
// scalastyle:off
import de.qaware.findfacts.common.dt.EtField._
import io.circe.generic.auto._
// scalastyle:on

/** Children for base theory blocks. */
case object TheoryChildren extends Children[TheoryEt] {
  override implicit def implicits: TheoryChildren.FieldImplicits[TheoryEt] = FieldImplicits()
}

/** Fields for all entities. */
sealed trait BaseEt {

  /** Unique id. */
  val id: Id.T
}

/** Fields for Entities that from the semantic theory. */
sealed trait TheoryEt extends BaseEt {

  /** Name of the entity. */
  val name: Name.T

  /** Proposition that the entity poses. */
  val proposition: Proposition.T

  /** Other entities that the proposition of the entity uses. */
  val propositionUses: PropositionUses.T
}

final case class CodeblockEt private (
    override val id: Id.T,
    /** Source in which entity was defined. */
    sourceFile: SourceTheory.T,
    /** Start pos of entity definition, in isabelle tokens. */
    startPosition: StartPosition.T,
    /** End pos of entity definition, in isabelle tokens. */
    endPosition: EndPosition.T,
    /** Source code of the entity. */
    sourceText: SourceText.T,
    /** Entities from this bloock. */
    entities: TheoryChildren.T
) extends BaseEt
    with Discriminator[CmdKind, CommandKind.type, CmdKind.Codeblock.type] {
  require(id == s"$sourceFile.$startPosition")

  def this(
      sourceFile: SourceTheory.T,
      startPosition: StartPosition.T,
      endPosition: EndPosition.T,
      sourceText: SourceText.T) =
    this(s"$sourceFile.$startPosition", sourceFile, startPosition, endPosition, sourceText, List.empty)
}

/** Documentation.
  *
  * @param documentationKind kind of documentation
  */
final case class DocumentationEt private (
    override val id: Id.T,
    sourceFile: SourceTheory.T,
    startPosition: StartPosition.T,
    endPosition: EndPosition.T,
    sourceText: SourceText.T,
    documentationKind: DocumentationKind.T
) extends BaseEt
    with Discriminator[CmdKind, CommandKind.type, dt.CmdKind.Documentation.type] {
  require(id == s"$sourceFile.$startPosition.$documentationKind")

  def this(
      sourceFile: SourceTheory.T,
      startPosition: StartPosition.T,
      endPosition: EndPosition.T,
      sourceText: SourceText.T,
      documentationKind: DocumentationKind.T
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
    override val id: Id.T,
    override val name: Name.T,
    override val proposition: Proposition.T,
    override val propositionUses: PropositionUses.T,
    typeUses: TypeUses.T,
    constantType: ConstantType.T
) extends TheoryEt
    with Discriminator[ThyEtKind, Kind.type, ThyEtKind.Constant.type] {
  require(id == s"${ThyEtKind.Constant}.$name")

  def this(
      name: Name.T,
      proposition: Proposition.T,
      propositionUses: PropositionUses.T,
      typeUses: TypeUses.T,
      constantType: ConstantType.T
  ) = this(s"${ThyEtKind.Constant}.$name", name, proposition, propositionUses, typeUses, constantType)
}

/** Any fact.
  *
  * @param proofUses entities that the proof references
  */
final case class FactEt private (
    override val id: Id.T,
    override val name: Name.T,
    override val proposition: Proposition.T,
    override val propositionUses: PropositionUses.T,
    proofUses: ProofUses.T
) extends TheoryEt
    with Discriminator[ThyEtKind, Kind.type, ThyEtKind.Fact.type] {
  require(id == s"${ThyEtKind.Fact}.$name")

  def this(
      name: Name.T,
      proposition: Proposition.T,
      propositionUses: PropositionUses.T,
      proofUses: ProofUses.T
  ) = this(s"${ThyEtKind.Fact}.$name", name, proposition, propositionUses, proofUses)
}

/** Type entity. */
final case class TypeEt private (
    override val id: Id.T,
    override val name: Name.T,
    override val proposition: Proposition.T,
    override val propositionUses: PropositionUses.T
) extends TheoryEt
    with Discriminator[ThyEtKind, Kind.type, ThyEtKind.Type.type] {
  require(id == s"${ThyEtKind.Type}.$name")

  def this(
      name: Name.T,
      proposition: Proposition.T,
      propositionUses: PropositionUses.T
  ) = this(s"${ThyEtKind.Type}.$name", name, proposition, propositionUses)
}
