package de.qaware.common.solr.dt

import scala.annotation.meta.field

import de.qaware.common.solr.dt.SolrSchema._
import org.apache.solr.client.solrj.beans.Field

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
    @Field(ID) id: String,
    @Field(SOURCE_FILE) sourceFile: String,
    @Field(START_POS) startPos: Int,
    @Field(END_POS) endPos: Int,
    @Field(KIND) kind: EntityKind.Value,
    @Field(NAME) typeName: String,
    @Field(USES) constructors: List[String]
)

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
    @field @Field(ID) id: String,
    @field @Field(SOURCE_FILE) sourceFile: String,
    @field @Field(START_POS) startPos: Int,
    @field @Field(END_POS) endPos: Int,
    @field @Field(KIND) kind: EntityKind.Value,
    @field @Field(NAME) name: String,
    @field @Field(CONST_TYPE) constType: String,
    @field @Field(TERM) definition: String,
    @field @Field(USES) uses: List[String]
)

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
    @Field(ID) id: String,
    @Field(SOURCE_FILE) sourceFile: String,
    @Field(START_POS) startPos: Int,
    @Field(END_POS) endPos: Int,
    @Field(KIND) kind: EntityKind.Value,
    @Field(TERM) term: String,
    @Field(USES) uses: List[String]
)

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
    @Field(ID) id: String,
    @Field(SOURCE_FILE) sourceFile: String,
    @Field(START_POS) startPos: Int,
    @Field(END_POS) endPos: Int,
    @Field(KIND) kind: EntityKind.Value,
    @Field(TERM) term: List[String],
    @Field(USES) uses: List[String]
)

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
    @Field(ID) id: String,
    @Field(SOURCE_FILE) sourceFile: String,
    @Field(START_POS) startPos: Int,
    @Field(END_POS) endPos: Int,
    @Field(KIND) kind: EntityKind.Value,
    @Field(TEXT) text: String
)
