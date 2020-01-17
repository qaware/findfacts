package de.qaware.findfacts.core

import de.qaware.findfacts.common.da.api.Variant.Discriminator
import de.qaware.findfacts.common.dt.{EtField, EtKind}
// scalastyle:off
import de.qaware.findfacts.common.dt.EtField._
// scalastyle:on

// scalastyle:off scaladoc

/** Short entry, with kind as parameter but not type.
  *
  * @param shortDescription Short description of the entity, as in [[ShortEntry]].
  */
final case class UntypedShortEntry(
    id: EtField.Id.FieldType,
    kind: Kind.FieldType,
    sourceFile: SourceFile.FieldType,
    startPosition: StartPosition.FieldType,
    endPosition: EndPosition.FieldType,
    shortDescription: String,
)

/** Short entry. */
sealed trait ShortEntry {
  val id: EtField.Id.FieldType
  val kind: Kind.FieldType
  val sourceFile: SourceFile.FieldType
  val startPosition: StartPosition.FieldType
  val endPosition: EndPosition.FieldType

  /** Short description for the entity, e.g. to display in results. */
  val shortDescription: String

  /** Creates an untyped representation.
    *
    * @return untyped short entry
    */
  def asUntyped: UntypedShortEntry =
    UntypedShortEntry(id, kind, sourceFile, startPosition, endPosition, shortDescription)
}

final case class ConstantShortEntry(
    override val id: EtField.Id.FieldType,
    override val sourceFile: SourceFile.FieldType,
    override val startPosition: StartPosition.FieldType,
    override val endPosition: EndPosition.FieldType,
    private val name: Name.FieldType,
    private val constType: ConstantType.FieldType
) extends ShortEntry
    with Discriminator[EtKind, Kind.type, EtKind.Constant.type] {
  override val shortDescription: String = s"$name :: $constType"
  override val kind: Kind.FieldType = Kind(EtKind.Constant)
}

final case class FactShortEntry(
    override val id: EtField.Id.FieldType,
    override val sourceFile: SourceFile.FieldType,
    override val startPosition: StartPosition.FieldType,
    override val endPosition: EndPosition.FieldType,
    private val name: Name.FieldType,
) extends ShortEntry
    with Discriminator[EtKind, Kind.type, EtKind.Fact.type] {
  override val shortDescription: String = name
  override val kind: Kind.FieldType = Kind(EtKind.Fact)

}

final case class DocumentationShortEntry(
    override val id: EtField.Id.FieldType,
    override val sourceFile: SourceFile.FieldType,
    override val startPosition: StartPosition.FieldType,
    override val endPosition: EndPosition.FieldType,
    private val sourceText: SourceText.FieldType)
    extends ShortEntry
    with Discriminator[EtKind, Kind.type, EtKind.Documentation.type] {
  override val shortDescription: String = sourceText.take(20) + "..."
  override val kind: Kind.FieldType = Kind(EtKind.Documentation)
}

final case class TypeShortEntry(
    override val id: EtField.Id.FieldType,
    override val sourceFile: SourceFile.FieldType,
    override val startPosition: StartPosition.FieldType,
    override val endPosition: EndPosition.FieldType,
    private val name: Name.FieldType
) extends ShortEntry
    with Discriminator[EtKind, Kind.type, EtKind.Type.type] {
  override val shortDescription: String = name
  override val kind: Kind.FieldType = Kind(EtKind.Type)
}
