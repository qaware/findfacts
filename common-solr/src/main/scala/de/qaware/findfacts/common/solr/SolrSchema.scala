package de.qaware.findfacts.common.solr

import de.qaware.findfacts.common.dt.EtField

/** All solr fields. */
object SolrSchema {
  // scalastyle:off scaladoc Justification: better explained in the entity classes.
  final val Id = "id"
  final val Kind = "kind"
  final val SourceFile = "source_file"
  final val StartPosition = "start_pos"
  final val EndPosition = "end_pos"
  final val SourceText = "source_text"
  final val Name = "name"
  final val Proposition = "prop"
  final val PropositionUses = "uses"
  final val ConstantType = "const_type"
  final val DocumentationKind = "doc_kind"
  final val TypeUses = "type_uses"
  final val ProofUses = "proof_uses"
  final val Children = "children"
  // scalastyle:on scaladoc

  /** Get the solr name literal for an entity field.
    *
    * @param field to get name for
    * @return string name
    */
  def getFieldName(field: EtField): String = field match {
    case EtField.Id => Id
    case EtField.Kind => Kind
    case EtField.Children => Children
    case EtField.SourceFile => SourceFile
    case EtField.StartPosition => StartPosition
    case EtField.EndPosition => EndPosition
    case EtField.Name => Name
    case EtField.Proposition => Proposition
    case EtField.SourceText => SourceText
    case EtField.ConstantType => ConstantType
    case EtField.PropositionUses => PropositionUses
    case EtField.TypeUses => TypeUses
    case EtField.ProofUses => ProofUses
    case EtField.DocumentationKind => DocumentationKind
  }
}
