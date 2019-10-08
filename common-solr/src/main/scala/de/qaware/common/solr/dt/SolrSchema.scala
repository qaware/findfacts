package de.qaware.common.solr.dt

/** All solr fields and enums. */
object SolrSchema extends Enumeration {
  final val ID = "id"
  final val SOURCE_FILE = "sourceFile"
  final val START_POS = "startPos"
  final val END_POS = "endPos"
  final val KIND = "kind"
  final val NAME = "name"
  final val CONST_TYPE = "const_type"
  final val TERM = "term"
  final val TEXT = "text"
  final val USES = "uses"
}

/** Kinds of solr documents */
object EntityKind extends Enumeration {

  val
  /** Type definition. */
  Type,
  /** Constants, including constructors. */
  Constant,
  /** Single propositions. */
  Axiom,
  /** Multiple propositions. */
  Fact,
  /** Comments, sections, titles etc. */
  Documentation = Value
}
