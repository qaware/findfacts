package de.qaware.findfacts.common.solr

import de.qaware.findfacts.common.dt.{EtField, EtFields}

/** All solr fields. */
object SolrSchema {
  // scalastyle:off scaladoc Justification: better explained in the entity classes.
  final val Id = "id"
  final val Serial = "serial"
  final val SourceFile = "source_file"
  final val StartPosition = "start_pos"
  final val EndPosition = "end_pos"
  final val Kind = "kind"
  final val Name = "name"
  final val ConstantType = "const_type"
  final val Proposition = "prop"
  final val SourceText = "source_text"
  final val DocumentationKind = "doc_kind"
  final val PropositionUses = "uses"
  final val TypeUses = "type_uses"
  final val ProofUses = "proof_uses"
  final val Related = "related"
  // scalastyle:on scaladoc

  /** Get the solr name literal for an entity field.
    *
    * @param field to get name for
    * @return string name
    */
  def getFieldName(field: EtField): String = field match {
    case EtFields.Id => Id
    case EtFields.Kind => Kind
    case EtFields.SourceFile => SourceFile
    case EtFields.StartPosition => StartPosition
    case EtFields.EndPosition => EndPosition
    case EtFields.Name => Name
    case EtFields.Proposition => Proposition
    case EtFields.SourceText => SourceText
    case EtFields.ConstantType => ConstantType
    case EtFields.PropositionUses => PropositionUses
    case EtFields.TypeUses => TypeUses
    case EtFields.Related => Related
    case EtFields.ProofUses => ProofUses
    case EtFields.DocumentationKind => DocumentationKind
  }
}
