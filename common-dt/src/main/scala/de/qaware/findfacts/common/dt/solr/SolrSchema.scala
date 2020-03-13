package de.qaware.findfacts.common.dt.solr

/** All solr fields. */
object SolrSchema {
  // scalastyle:off scaladoc Justification: better explained in the entity classes.
  final val Id = "id"
  final val DocKind = "kind"
  final val ParentFlag = "parent_flag"
  final val TheoryKind = "thy_kind"
  final val Command = "command"
  final val SourceTheory = "theory"
  final val StartLine = "start_line"
  final val SourceTextBefore = "src_before"
  final val SourceText = "src"
  final val SourceTextAfter = "src_after"
  final val Name = "name"
  final val NameFacet = "name_facet"
  final val Uses = "uses"
  final val ConstantType = "type"
  final val ConstantTypeFacet = "type_facet"
  final val Children = "children"
  // scalastyle:on scaladoc
}
