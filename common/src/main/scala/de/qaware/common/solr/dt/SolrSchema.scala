package de.qaware.common.solr.dt

object SolrSchema extends Enumeration {
  final val ID = "id"
}

object EntityKind extends Enumeration {
  val Type, Const, Axiom, Fact, Locale = Value
}