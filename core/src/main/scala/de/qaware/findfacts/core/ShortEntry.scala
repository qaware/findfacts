package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.{EtField, EtKind}
import shapeless.tag.Tagged

// scalastyle:off scaladoc

/** Short entry, with kind as parameter but not type.
  *
  * @param shortDescription Short description of the entity, as in [[ShortEntry]].
  */
case class UntypedShortEntry(
    id: EtField.Id.FieldType,
    kind: EtField.Kind.FieldType,
    sourceFile: EtField.SourceFile.FieldType,
    startPosition: EtField.StartPosition.FieldType,
    endPosition: EtField.EndPosition.FieldType,
    shortDescription: String,
)

/** Short entry. */
sealed trait ShortEntry {
  val id: EtField.Id.FieldType
  val kind: EtField.Kind.FieldType
  val sourceFile: EtField.SourceFile.FieldType
  val startPosition: EtField.StartPosition.FieldType
  val endPosition: EtField.EndPosition.FieldType

  /** Short description for the entity, e.g. to display in results. */
  val shortDescription: String

  /** Creates an untyped representation.
    *
    * @return untyped short entry
    */
  def asUntyped: UntypedShortEntry =
    UntypedShortEntry(id, kind, sourceFile, startPosition, endPosition, shortDescription)
}

case class ConstantShortEntry(
    override val id: EtField.Id.FieldType,
    override val sourceFile: EtField.SourceFile.FieldType,
    override val startPosition: EtField.StartPosition.FieldType,
    override val endPosition: EtField.EndPosition.FieldType,
    private val name: EtField.Name.FieldType,
    private val constType: EtField.ConstantType.FieldType
) extends ShortEntry
    with Tagged[EtKind.Constant.type] {
  override val shortDescription: String = s"$name :: $constType"
  override val kind: EtField.Kind.FieldType = EtKind.Constant.asInstanceOf[EtField.Kind.FieldType]
}

case class FactShortEntry(
    override val id: EtField.Id.FieldType,
    override val sourceFile: EtField.SourceFile.FieldType,
    override val startPosition: EtField.StartPosition.FieldType,
    override val endPosition: EtField.EndPosition.FieldType,
    private val name: EtField.Name.FieldType,
) extends ShortEntry
    with Tagged[EtKind.Fact.type] {
  override val shortDescription: String = name
  override val kind: EtField.Kind.FieldType = EtKind.Fact.asInstanceOf[EtField.Kind.FieldType]

}

case class DocumentationShortEntry(
    override val id: EtField.Id.FieldType,
    override val sourceFile: EtField.SourceFile.FieldType,
    override val startPosition: EtField.StartPosition.FieldType,
    override val endPosition: EtField.EndPosition.FieldType,
    private val sourceText: EtField.SourceText.FieldType)
    extends ShortEntry
    with Tagged[EtKind.Documentation.type] {
  override val shortDescription: String = sourceText.take(20) + "..."
  override val kind: EtField.Kind.FieldType = EtKind.Documentation.asInstanceOf[EtField.Kind.FieldType]
}

case class TypeShortEntry(
    override val id: EtField.Id.FieldType,
    override val sourceFile: EtField.SourceFile.FieldType,
    override val startPosition: EtField.StartPosition.FieldType,
    override val endPosition: EtField.EndPosition.FieldType,
    private val name: EtField.Name.FieldType,
) extends ShortEntry
    with Tagged[EtKind.Type.type] {
  override val shortDescription: String = name
  override val kind: EtField.Kind.FieldType = EtKind.Type.asInstanceOf[EtField.Kind.FieldType]
}
