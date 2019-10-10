package de.qaware.common.solr.dt

import scala.annotation.meta.field

import de.qaware.common.solr.dt.SolrSchema._
import org.apache.solr.client.solrj.beans.Field

// scalastyle:off

/** Entity class for type definitions.
  *
  * @param id solr doc id
  * @param sourceFile relative path to file containing entity
  * @param startPos token in src at which entity begins
  * @param endPos token in src at which entity ends
  * @param kind kind of the entity, i.e. [[EntityKind.Type]] for this entity
  * @param typeName name of the defined type
  * @param constructors ids of constructors this type has
  */
case class TypeEntity(
    @(Field @field)(ID) id: String,
    @(Field @field)(SOURCE_FILE) sourceFile: String,
    @(Field @field)(START_POS) startPos: Int,
    @(Field @field)(END_POS) endPos: Int,
    @(Field @field)(NAME) typeName: String,
    @(Field @field)(USES) constructors: Array[String],
    @(Field @field)(KIND) kind: String = EntityKind.Type
) {
  def this() {
    this(null, null, -1, -1, null, null)
  }
}

/** Entity class for constants.
  *
  * @param id solr doc id
  * @param sourceFile relative path to file containing entity
  * @param startPos token in src at which entity begins
  * @param endPos token in src at which entity ends
  * @param kind kind of the entity, i.e. [[EntityKind.Constant]] for this entity
  * @param name name of the constant
  * @param constType functional type of the constant
  * @param definition definition term in string representation
  * @param uses ids of entities that the constant uses
  */
case class ConstEntity(
    @(Field @field)(ID) id: String,
    @(Field @field)(SOURCE_FILE) sourceFile: String,
    @(Field @field)(START_POS) startPos: Int,
    @(Field @field)(END_POS) endPos: Int,
    @(Field @field)(NAME) name: String,
    @(Field @field)(CONST_TYPE) constType: String,
    @(Field @field)(TERM) definition: String,
    @(Field @field)(USES) uses: Array[String],
    @(Field @field)(KIND) kind: String = EntityKind.Constant
) {
  def this() {
    this(null, null, -1, -1, null, null, null, null)
  }
}

/** Entity class for axiomatic facts.
  *
  * @param id solr doc id
  * @param sourceFile relative path to file containing entity
  * @param startPos token in src at which entity begins
  * @param endPos token in src at which entity ends
  * @param kind kind of the entity, i.e. [[EntityKind.Axiom]] for this entity
  * @param term term of the axiom as string representation
  * @param uses ids of entities that the axiom uses
  */
case class AxiomEntity(
    @(Field @field)(ID) id: String,
    @(Field @field)(SOURCE_FILE) sourceFile: String,
    @(Field @field)(START_POS) startPos: Int,
    @(Field @field)(END_POS) endPos: Int,
    @(Field @field)(TERM) term: String,
    @(Field @field)(USES) uses: Array[String],
    @(Field @field)(KIND) kind: String = EntityKind.Axiom
) {
  def this() {
    this(null, null, -1, -1, null, null, null)
  }
}

/** Entity class for proven lemmas and theories.
  *
  * @param id solr doc id
  * @param sourceFile relative path to file containing entity
  * @param startPos token in src at which entity begins
  * @param endPos token in src at which entity ends
  * @param kind kind of the entity, i.e. [[EntityKind.Fact]] for this entity
  * @param term all terms of the fact as string representation
  * @param uses ids of entities that the fact uses
  */
case class FactEntity(
    @(Field @field)(ID) id: String,
    @(Field @field)(SOURCE_FILE) sourceFile: String,
    @(Field @field)(START_POS) startPos: Int,
    @(Field @field)(END_POS) endPos: Int,
    @(Field @field)(TERM) term: String,
    @(Field @field)(USES) uses: Array[String],
    @(Field @field)(KIND) kind: String = EntityKind.Fact
) {
  def this() {
    this(null, null, -1, -1, null, null)
  }
}

/** Entity class for documentation and comments.
  *
  * @param id solr doc id
  * @param sourceFile relative path to file containing entity
  * @param startPos token in src at which entity begins
  * @param endPos token in src at which entity ends
  * @param kind kind of the entity, i.e. [[EntityKind.Documentation]] for this entity
  * @param text text of the documentation
  */
case class DocumentationEntity(
    @(Field @field)(ID) id: String,
    @(Field @field)(SOURCE_FILE) sourceFile: String,
    @(Field @field)(START_POS) startPos: Int,
    @(Field @field)(END_POS) endPos: Int,
    @(Field @field)(TEXT) text: String,
    @(Field @field)(KIND) kind: String = EntityKind.Documentation
) {
  def this() {
    this(null, null, -1, -1, null)
  }
}
