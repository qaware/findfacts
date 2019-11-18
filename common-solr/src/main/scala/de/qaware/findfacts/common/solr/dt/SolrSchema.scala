package de.qaware.findfacts.common.solr.dt

import scala.language.implicitConversions

/** All solr fields and enums. */
object SolrSchema {
  // scalastyle:off scaladoc Justification: better explained in the entity classes.
  final val Id = "id"
  final val Serial = "serial"
  final val SourceFile = "source_file"
  final val StartPos = "start_pos"
  final val EndPos = "end_pos"
  final val Kind = "kind"
  final val Name = "name"
  final val ConstType = "const_type"
  final val Term = "term"
  final val Text = "text"
  final val DocType = "doc_type"
  final val Uses = "uses"
  final val TypeUses = "type_uses"
  final val Related = "related"
  // scalastyle:on scaladoc
}

/** Kinds of solr documents. Java enum as scala enums won't be recognized by default solr mapper. */
object EntityKind extends Enumeration {

  /** Type definition. */
  final val Type = Value("Type")

  /** Constants, including constructors. */
  final val Constant = Value("Constant")

  /** Some propositions. */
  final val Fact = Value("Fact")

  /** Comments, sections, titles etc. */
  final val Documentation = Value("Documentation")
}

/** Types of documentation. */
object DocumentationType extends Enumeration {

  /** Comments in the meta-language, (* ... *) */
  final val Meta = Value("Meta")

  /** Latex documentation: sections, etc. */
  final val Latex = Value("Latex")

  /** Inline comments, usually in cartouches. */
  final val Inline = Value("Inline")
}
