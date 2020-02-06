package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.da.api.Variant.Discriminator
import de.qaware.findfacts.common.dt
// scalastyle:off
import de.qaware.findfacts.common.dt.EtField._
// scalastyle:on

// scalastyle:off scaladoc
sealed trait ShortCmdEt {
  val id: EtField.Id.FieldType
  val file: SourceTheory.FieldType
  val src: SourceText.FieldType
  val kind: CmdKind.Value
  val entities: ChildShorts.FieldType
}

/** Short source code block. */
final case class ShortBlock(
    override val id: EtField.Id.FieldType,
    override val file: SourceTheory.FieldType,
    override val src: SourceText.FieldType,
    override val entities: ChildShorts.FieldType)
    extends ShortCmdEt
    with Discriminator[CmdKind, CommandKind.type, CmdKind.Codeblock.type] {
  override val kind: CmdKind.Value = CmdKind.Codeblock
}

/** Short documentation entity */
final case class DocumentationShortEt(
    override val id: EtField.Id.FieldType,
    override val file: SourceTheory.FieldType,
    override val src: SourceText.FieldType,
) extends ShortCmdEt
    with Discriminator[CmdKind, CommandKind.type, dt.CmdKind.Documentation.type] {
  override val kind: dt.CmdKind.Value = CmdKind.Documentation
  override val entities: ChildShorts.FieldType = List.empty
}

/** Short entry. */
sealed trait ShortThyEt {
  val id: Id.FieldType
  val kind: ThyEtKind.Value
  val name: Name.FieldType

  /** Short description for the entity, e.g. to display in results. */
  val shortDescription: String
}

/** Short constant entity. */
final case class ConstantShortEt(
    override val id: EtField.Id.FieldType,
    override val name: Name.FieldType,
    constType: ConstantType.FieldType,
) extends ShortThyEt
    with Discriminator[ThyEtKind, Kind.type, ThyEtKind.Constant.type] {
  override val shortDescription: String = constType
  override val kind: ThyEtKind.Value = ThyEtKind.Constant
}

/** Short fact entity. */
final case class FactShortEt(
    override val id: EtField.Id.FieldType,
    override val name: Name.FieldType,
) extends ShortThyEt
    with Discriminator[ThyEtKind, Kind.type, ThyEtKind.Fact.type] {
  override val shortDescription: String = ""
  override val kind: ThyEtKind.Value = ThyEtKind.Fact
}

/** Short type entity. */
final case class TypeShortEt(
    override val id: EtField.Id.FieldType,
    override val name: Name.FieldType,
) extends ShortThyEt
    with Discriminator[ThyEtKind, Kind.type, ThyEtKind.Type.type] {
  override val shortDescription: String = ""
  override val kind: ThyEtKind.Value = ThyEtKind.Type
}
