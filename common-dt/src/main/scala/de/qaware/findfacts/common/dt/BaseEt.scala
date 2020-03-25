package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.da.api.Variant.Discriminator
// scalastyle:off
import de.qaware.findfacts.common.dt.EtField._
import io.circe.generic.auto._
// scalastyle:on

/** Children for base theory blocks. */
case object TheoryChildren extends Children[TheoryEt] {
  override implicit val implicits: TheoryChildren.FieldImplicits[TheoryEt] = FieldImplicits()
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

  /** Other entities that the entity uses. */
  val uses: Uses.T
}

/** Companion object. */
object TheoryEt {

  /** Creates an unique id for a given theory entity.
    *
    * @param kind of the entity
    * @param name of the entity. Must be unique within the kind of entities.
    * @return unique id
    */
  def makeId(kind: Kind, name: Name.T): Id.T = s"$kind.$name"
}

// scalastyle:off scaladoc
/** Any code block.
  *
  * @param theory source in which entity was defined
  * @param startLine line at which code block starts
  * @param srcBefore source code before this entity
  * @param src source code of the entity
  * @param srcAfter source code after this entity
  * @param entities entities from this block
  */
final case class CodeblockEt private (
    override val id: Id.T,
    theory: SourceTheory.T,
    startLine: StartLine.T,
    command: Command.T,
    srcBefore: SourceTextBefore.T,
    src: SourceText.T,
    srcAfter: SourceTextAfter.T,
    entities: TheoryChildren.T
) extends BaseEt
    with Discriminator[Kind, EtField.DocKind.type, Kind.Block.type] {
  def this(
      startPos: Int,
      endPos: Int,
      theory: SourceTheory.T,
      startLine: StartLine.T,
      command: Command.T,
      srcBefore: SourceTextBefore.T,
      src: SourceText.T,
      srcAfter: SourceTextAfter.T,
      entities: TheoryChildren.T = List.empty) = {
    this(CodeblockEt.makeId(theory, startPos, endPos), theory, startLine, command, srcBefore, src, srcAfter, entities)
  }
}

/** Companion object. */
object CodeblockEt {

  /** Creates an unique id for a codeblock.
    *
    * @param theory the block is in
    * @param startPos the position of the first character in the block
    * @param endPos the position after the last character in the block
    * @return unique id
    */
  def makeId(theory: SourceTheory.T, startPos: Int, endPos: Int): Id.T = s"$theory.$startPos.$endPos"
}

/** Constant entity.
  *
  * @param constantType type of the constant
  */
final case class ConstantEt private (
    override val id: Id.T,
    override val name: Name.T,
    override val uses: Uses.T,
    constantType: ConstantType.T,
    docKind: DocKind.T = Kind.Constant)
    extends TheoryEt
    with Discriminator[Kind, EtField.Kind.type, Kind.Constant.type] {
  def this(name: Name.T, uses: Uses.T, constantType: ConstantType.T) =
    this(TheoryEt.makeId(Kind.Constant, name), name, uses, constantType)
}

/** Fact entity. */
final case class FactEt private (
    override val id: Id.T,
    override val name: Name.T,
    override val uses: Uses.T,
    docKind: DocKind.T = Kind.Fact)
    extends TheoryEt
    with Discriminator[Kind, EtField.Kind.type, Kind.Fact.type] {
  def this(name: Name.T, uses: Uses.T) = this(TheoryEt.makeId(Kind.Fact, name), name, uses)
}

/** Type entity. */
final case class TypeEt private (
    override val id: Id.T,
    override val name: Name.T,
    override val uses: Uses.T,
    docKind: DocKind.T = Kind.Type)
    extends TheoryEt
    with Discriminator[Kind, EtField.Kind.type, Kind.Type.type] {
  def this(name: Name.T, uses: Uses.T) = this(TheoryEt.makeId(Kind.Type, name), name, uses)
}
