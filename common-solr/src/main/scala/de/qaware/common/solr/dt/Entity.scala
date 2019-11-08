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
    @(Field @field)(TERM) constructors: Array[String],
    @(Field @field)(USES) uses: Array[String])
    extends Entity(sourceFile, startPos, endPos, EntityKind.Type) {

  registerKeyFields(Seq(sourceFile, startPos, endPos, kind))

  @SuppressWarnings(Array("NullParameter")) // Justification: Entity classes are mapped into solr document
  def this() {
    this(null, -1, -1, null, null, null)
  }

  override def toString = s"TypeEntity($sourceFile, $startPos, $endPos, $typeName, ${constructors.deep}, ${uses.deep})"

  override def equals(other: Any): Boolean = other match {
    case that: TypeEntity =>
      sourceFile == that.sourceFile &&
        startPos == that.startPos &&
        endPos == that.endPos &&
        typeName == that.typeName &&
        (constructors sameElements that.constructors) &&
        (uses sameElements that.uses)
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(sourceFile, startPos, endPos, typeName, constructors.deep, uses.deep)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
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
    @(Field @field)(USES) uses: Array[String])
    extends Entity(sourceFile, startPos, endPos, EntityKind.Constant) {

  registerKeyFields(Seq(sourceFile, startPos, endPos, kind, name))

  @SuppressWarnings(Array("NullParameter")) // Justification: Entity classes are mapped into solr document
  def this() {
    this(null, -1, -1, null, null, null, null)
  }

  override def toString = s"ConstEntity($sourceFile, $startPos, $endPos, $name, $constType, $definitions, ${uses.deep})"

  override def equals(other: Any): Boolean = other match {
    case that: ConstEntity =>
      sourceFile == that.sourceFile &&
        startPos == that.startPos &&
        endPos == that.endPos &&
        name == that.name &&
        constType == that.constType &&
        (definitions sameElements that.definitions) &&
        (uses sameElements that.uses)
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(sourceFile, startPos, endPos, name, constType, definitions.deep, uses.deep)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
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
    @(Field @field)(USES) uses: Array[String])
    extends Entity(sourceFile, startPos, endPos, EntityKind.Fact) {

  registerKeyFields(Seq(sourceFile, startPos, endPos, kind, name))
  @SuppressWarnings(Array("NullParameter")) // Justification: Entity classes are mapped into solr document
  def this() {
    this(null, -1, -1, null, null, null)
  }

  override def toString = s"FactEntity($sourceFile, $startPos, $endPos, $name, $term, ${uses.deep})"

  override def equals(other: Any): Boolean = other match {
    case that: FactEntity =>
      sourceFile == that.sourceFile &&
        startPos == that.startPos &&
        endPos == that.endPos &&
        name == that.name &&
        term == that.term &&
        (uses sameElements that.uses)
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(sourceFile, startPos, endPos, name, term, uses.deep)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
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

  registerKeyFields(Seq(sourceFile, startPos, endPos, kind))

  @SuppressWarnings(Array("NullParameter")) // Justification: Entity classes are mapped into solr document
  def this() {
    this(null, -1, -1, null, null)
  }
}
