package de.qaware.common.solr.dt

import scala.annotation.meta.field

import de.qaware.common.solr.dt.SolrSchema._
import org.apache.solr.client.solrj.beans.Field
import org.apache.solr.update.processor.Lookup3Signature

/** Base entity class. ID is generated within solr from sourceFile, startPos and endPos.
  *
  * @param sourceFile relative path to file containing entity
  * @param startPos token in src at which entity begins
  * @param endPos token in src at which entity ends
  * @param kind of this entity. String value for solr compability from [[EntityKind]] enum
  */
sealed abstract class Entity(
    @(Field @field)(SOURCE_FILE) val sourceFile: String,
    @(Field @field)(START_POS) val startPos: Int,
    @(Field @field)(END_POS) val endPos: Int,
    @(Field @field)(KIND) val kind: String
) {

  /** Registers the key fields of this entity. Must be called in subclasses once.
    *
    * @param values values of fields to use as composite key
    */
  def registerKeyFields(values: Seq[Any]): Unit = {
    keyFields = values
    // Evaluate lazy var
    id
  }
  private var keyFields: Seq[Any] = Seq.empty

  /** Generates composite unique ID. Unfortunately, solrj reads beans by reflection, so this is not ever called -
    * hence subclasses should call [[registerKeyFields]] at creation time.
    *
    * @return solr id of this entity
    */
  @(Field @field)(ID)
  lazy val id: String = {
    val sig = new Lookup3Signature()

    keyFields.foreach { f =>
      if (f != null) {
        sig.add(f.toString)
      }
    }

    val bytes = sig.getSignature
    bytes.map("%02x".format(_)).mkString
  }
}

// scalastyle:off
/** Entity class for type definitions.
  *
  * @param typeName name of the defined type
  * @param constructors ids of constructors this type has
  */
final case class TypeEntity(
    override val sourceFile: String,
    override val startPos: Int,
    override val endPos: Int,
    @(Field @field)(NAME) typeName: String,
    @(Field @field)(USES) constructors: Array[String]
) extends Entity(sourceFile, startPos, endPos, EntityKind.Type) {
  @SuppressWarnings(Array("NullParameter")) // Justification: Entity classes are mapped into solr document
  def this() {
    this(null, -1, -1, null, null)
  }
  registerKeyFields(Seq(sourceFile, startPos, endPos, kind))
}

/** Entity class for constants.
  *
  * @param name name of the constant
  * @param constType functional type of the constant
  * @param definitions definition(s) term in string representation
  * @param uses ids of entities that the constant uses
  */
final case class ConstEntity(
    override val sourceFile: String,
    override val startPos: Int,
    override val endPos: Int,
    @(Field @field)(NAME) name: String,
    @(Field @field)(CONST_TYPE) constType: String,
    @(Field @field)(TERM) definitions: Array[String],
    @(Field @field)(USES) uses: Array[String]
) extends Entity(sourceFile, startPos, endPos, EntityKind.Constant) {
  @SuppressWarnings(Array("NullParameter")) // Justification: Entity classes are mapped into solr document
  def this() {
    this(null, -1, -1, null, null, null, null)
  }
  registerKeyFields(Seq(sourceFile, startPos, endPos, kind, name))
}

/** Entity class for proven lemmas and theories.
  *
  * @param term all terms of the fact as string representation
  * @param uses ids of entities that the fact uses
  */
final case class FactEntity(
    override val sourceFile: String,
    override val startPos: Int,
    override val endPos: Int,
    @(Field @field)(NAME) name: String,
    @(Field @field)(TERM) term: String,
    @(Field @field)(USES) uses: Array[String]
) extends Entity(sourceFile, startPos, endPos, EntityKind.Fact) {
  @SuppressWarnings(Array("NullParameter")) // Justification: Entity classes are mapped into solr document
  def this() {
    this(null, -1, -1, null, null, null)
  }
  registerKeyFields(Seq(sourceFile, startPos, endPos, kind, name))
}

/** Entity class for documentation and comments.
  *
  * @param text text of the documentation
  * @param docType type of the documentation entry
  */
final case class DocumentationEntity(
    override val sourceFile: String,
    override val startPos: Int,
    override val endPos: Int,
    @(Field @field)(TEXT) text: String,
    @(Field @field)(DOCTYPE) docType: String
) extends Entity(sourceFile, startPos, endPos, EntityKind.Documentation) {
  @SuppressWarnings(Array("NullParameter")) // Justification: Entity classes are mapped into solr document
  def this() {
    this(null, -1, -1, null, null)
  }
  registerKeyFields(Seq(sourceFile, startPos, endPos, kind))
}
