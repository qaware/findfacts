package de.qaware.common.solr.dt

import scala.language.implicitConversions

/** All solr fields and enums. */
object SolrSchema {
  // scalastyle:off scaladoc
  // justification: better explained in the entity classes.
  final val ID = "id"
  final val SOURCE_FILE = "sourceFile"
  final val START_POS = "startPos"
  final val END_POS = "endPos"
  final val KIND = "kind"
  final val NAME = "name"
  final val CONST_TYPE = "const_type"
  final val TERM = "term"
  final val TEXT = "text"
  final val DOCTYPE = "doc_type"
  final val USES = "uses"
  // scalastyle:on scaladoc
}

/** Kinds of solr documents. Java enum as scala enums won't be recognized by default solr mapper. */
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

  /** String conversion method for use with entities, since solrj can not directly handle enum fields.
    *
    * @param entityKind to convert to string
    * @return string name of entityKind
    */
  implicit def toString(entityKind: EntityKind.Value): String = entityKind.toString
}

/** Types of documentation. */
object DocumentationType extends Enumeration {
  val
  /** Comments in the meta-language, (* ... *) */
  Meta,
  /** Latex documentation: sections, etc. */
  Latex,
  /** Inline comments, usually in cartouches. */
  Inline = Value

  /** String conversion method for use with entities, since solrj can not directly handle enum fields.
    *
    * @param docType to convert to string
    * @return string name of documentation type
    */
  implicit def toString(docType: DocumentationType.Value): String = docType.toString
}
