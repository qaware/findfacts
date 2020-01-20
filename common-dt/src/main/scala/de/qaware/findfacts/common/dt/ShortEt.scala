package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.da.api.Variant.Discriminator
// scalastyle:off
import de.qaware.findfacts.common.dt.EtField._
// scalastyle:on

// scalastyle:off scaladoc
sealed trait ShortEt {
  val kind: EtKind.Value
}

/** Short entry. */
sealed trait ShortThyEt extends ShortEt {
  val id: Id.FieldType
  val name: Name.FieldType
  val proposition: Proposition.FieldType

  /** Short description for the entity, e.g. to display in results. */
  val shortDescription: String
}

/** Short source code block entry. */
final case class ShortBlockEt(
    sourceFile: SourceFile.FieldType,
    sourceText: SourceText.FieldType,
    childEntities: ChildShorts.FieldType)
    extends ShortEt
    with Discriminator[EtKind, Kind.type, EtKind.Block.type] {
  override val kind: EtKind.Value = EtKind.Block
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

/** Short documentation entity */
final case class DocumentationShortEt(
    sourceText: SourceText.FieldType,
) extends ShortEt
    with Discriminator[EtKind, Kind.type, EtKind.Documentation.type] {
  override val kind: EtKind.Value = EtKind.Documentation
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
