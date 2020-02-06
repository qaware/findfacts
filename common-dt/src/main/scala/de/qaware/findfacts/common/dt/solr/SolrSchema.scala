package de.qaware.findfacts.common.dt.solr

/** All solr fields. */
object SolrSchema {
  // scalastyle:off scaladoc Justification: better explained in the entity classes.
  final val Id = "id"
  final val TheoryKind = "thy_kind"
  final val CommandKind = "cmd_kind"
  final val SourceTheory = "theory"
  final val StartPosition = "start_pos"
  final val EndPosition = "end_pos"
  final val StartLine = "start_line"
  final val SourceText = "src"
  final val Name = "name"
  final val Proposition = "prop"
  final val PropositionUses = "uses"
  final val ConstantType = "type"
  final val DocumentationKind = "doc_kind"
  final val TypeUses = "type_uses"
  final val ProofUses = "proof_uses"
  final val Children = "children"
  // scalastyle:on scaladoc
}
