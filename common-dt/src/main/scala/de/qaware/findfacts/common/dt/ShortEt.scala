package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.da.api.Variant.Discriminator
// scalastyle:off
import de.qaware.findfacts.common.dt.EtField._
// scalastyle:on

// scalastyle:off scaladoc
sealed trait ShortEt {
  val id: EtField.Id.FieldType
  val file: SourceFile.FieldType
  val src: SourceText.FieldType
  val kind: EtKind.Value
  val entities: ChildShorts.FieldType
}

/** Short source code block entry. */
final case class ShortBlockEt(
    override val id: EtField.Id.FieldType,
    override val file: SourceFile.FieldType,
    override val src: SourceText.FieldType,
    override val entities: ChildShorts.FieldType)
    extends ShortEt
    with Discriminator[EtKind, Kind.type, EtKind.Block.type] {
  override val kind: EtKind.Value = EtKind.Block
}

/** Short documentation entity */
final case class DocumentationShortEt(
    override val id: EtField.Id.FieldType,
    override val file: SourceFile.FieldType,
    override val src: SourceText.FieldType,
) extends ShortEt
    with Discriminator[EtKind, Kind.type, EtKind.Documentation.type] {
  override val kind: EtKind.Value = EtKind.Documentation
  override val entities: ChildShorts.FieldType = List.empty
}

/** Short entry. */
sealed trait ShortThyEt {
  val id: Id.FieldType
  val kind: EtKind.Value
  val name: Name.FieldType
  val proposition: Proposition.FieldType

  /** Short description for the entity, e.g. to display in results. */
  val shortDescription: String
}

/** Short constant entity. */
final case class ConstantShortEt(
    override val id: EtField.Id.FieldType,
    override val name: Name.FieldType,
    override val proposition: Proposition.FieldType,
    constType: ConstantType.FieldType,
) extends ShortThyEt
    with Discriminator[EtKind, Kind.type, EtKind.Constant.type] {
  override val shortDescription: String = s"$name :: $constType"
  override val kind: EtKind.Value = EtKind.Constant
}

/** Short fact entity. */
final case class FactShortEt(
    override val id: EtField.Id.FieldType,
    override val name: Name.FieldType,
    override val proposition: Proposition.FieldType,
) extends ShortThyEt
    with Discriminator[EtKind, Kind.type, EtKind.Fact.type] {
  override val shortDescription: String = name
  override val kind: EtKind.Value = EtKind.Fact
}

/** Short type entity. */
final case class TypeShortEt(
    override val id: EtField.Id.FieldType,
    override val name: Name.FieldType,
    override val proposition: Proposition.FieldType,
) extends ShortThyEt
    with Discriminator[EtKind, Kind.type, EtKind.Type.type] {
  override val shortDescription: String = name
  override val kind: EtKind.Value = EtKind.Type
}
