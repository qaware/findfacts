package de.qaware.findfacts.common.solr.dt

import scala.annotation.meta.field

import de.qaware.findfacts.common.solr.dt.Entity.Id
// scalastyle:off UnderscoreImportChecker
import de.qaware.findfacts.common.solr.dt.SolrSchema._
// scalastyle:on UnderscoreImportChecker
import de.qaware.findfacts.scalautils.NullableArray.deep
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
    @(Field @field)(SourceFile) val sourceFile: String,
    @(Field @field)(StartPos) val startPos: Int,
    @(Field @field)(EndPos) val endPos: Int,
    @(Field @field)(Kind) val kind: String
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
  @(Field @field)(Id)
  lazy val id: Id = {
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
object Entity {

  /** Ids are stored as strings. */
  type Id = String
}

// scalastyle:off
/** Sum type for semantic theory entities.
  *
  * @param name of the entity
  * @param terms semantic representation of the entity
  * @param related ids of entities defined at the same position
  * @param source text of entity
  */
sealed abstract class TheoryEntity(
    override val sourceFile: String,
    override val startPos: Int,
    override val endPos: Int,
    override val kind: String,
    @(Field @field)(Name) val name: String,
    @(Field @field)(Term) val terms: Array[String],
    @(Field @field)(Related) val related: Array[Id],
    @(Field @field)(Text) val source: String)
    extends Entity(sourceFile, startPos, endPos, kind)

/** Entity class for type definitions.
  *
  * @param uses id of types that this type definition uses
  */
@SuppressWarnings(Array("NullParameter", "NullAssignment")) // Justification: Mapped to solr document
final case class TypeEntity(
    override val sourceFile: String,
    override val startPos: Int,
    override val endPos: Int,
    override val name: String,
    override val terms: Array[String] = null,
    @(Field @field)(TypeUses) uses: Array[Id] = null,
    override val related: Array[Id] = null,
    override val source: String = null
) extends TheoryEntity(sourceFile, startPos, endPos, EntityKind.Type.toString, name, terms, related, source) {

  registerKeyFields(Seq(sourceFile, startPos, kind, name))

  def this() {
    this(null, -1, -1, null)
  }

  override def toString: String =
    s"TypeEntity($sourceFile, $startPos, $endPos, " +
      s"$name, ${deep(terms)}, ${deep(uses)}, ${deep(related)}, $source)"

  override def equals(other: Any): Boolean = other match {
    case that: TypeEntity =>
      sourceFile == that.sourceFile &&
        startPos == that.startPos &&
        endPos == that.endPos &&
        name == that.name &&
        deep(terms) == deep(that.terms) &&
        source == that.source &&
        deep(uses) == deep(that.uses) &&
        deep(related) == deep(that.related)
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(sourceFile, startPos, endPos, name, deep(terms), source, deep(uses), deep(related))
    state.filter(_ != null).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/** Entity class for constants.
  *
  * @param name of the constant
  * @param constType functional type of the constant
  * @param typeUses ids of types that the type of this constant uses
  * @param source source code of the constant declaration
  * @param terms definition(s) term in string representation
  * @param defUses ids of entities that the constant uses
  * @param related ids of entities that stem from the same source
  */
@SuppressWarnings(Array("NullParameter", "NullAssignment")) // Justification: Mapped to solr document
final case class ConstEntity(
    override val sourceFile: String,
    override val startPos: Int,
    override val endPos: Int,
    override val name: String,
    @(Field @field)(ConstType) constType: String = null,
    @(Field @field)(TypeUses) typeUses: Array[Id] = null,
    override val terms: Array[String] = null,
    @(Field @field)(Uses) defUses: Array[Id] = null,
    override val related: Array[Id] = null,
    override val source: String = null
) extends TheoryEntity(sourceFile, startPos, endPos, EntityKind.Constant.toString, name, terms, related, source) {

  registerKeyFields(Seq(sourceFile, startPos, kind, name))

  def this() {
    this(null, -1, -1, null)
  }

  override def toString: String = {
    s"ConstEntity($sourceFile, $startPos, $endPos, $name, $constType, ${deep(typeUses)}, " +
      s"${deep(terms)}, ${deep(defUses)}, ${deep(related)}, $source)"
  }

  override def equals(other: Any): Boolean = other match {
    case that: ConstEntity =>
      sourceFile == that.sourceFile &&
        startPos == that.startPos &&
        endPos == that.endPos &&
        name == that.name &&
        constType == that.constType &&
        deep(typeUses) == deep(that.defUses) &&
        source == that.source &&
        deep(terms) == deep(that.terms) &&
        deep(defUses) == deep(that.defUses) &&
        deep(related) == deep(that.related)
    case _ => false
  }

  override def hashCode(): Int = {
    val state =
      Seq(
        sourceFile,
        startPos,
        endPos,
        name,
        constType,
        deep(typeUses),
        source,
        deep(terms),
        deep(defUses),
        deep(related))
    state.filter(_ != null).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/** Entity class for proven lemmas and theories.
  *
  * @param name of this fact
  * @param terms all terms of the fact as string representation
  * @param source term of this fact
  * @param uses ids of entities that the fact uses
  * @param related ids of entities that stem from the same source
  */
@SuppressWarnings(Array("NullParameter", "NullAssignment")) // Justification: Mapped to solr document
final case class FactEntity(
    override val sourceFile: String,
    override val startPos: Int,
    override val endPos: Int,
    override val name: String,
    override val terms: Array[String] = null,
    @(Field @field)(Uses) uses: Array[Id] = null,
    override val related: Array[Id] = null,
    override val source: String = null
) extends TheoryEntity(sourceFile, startPos, endPos, EntityKind.Fact.toString, name, terms, related, source) {

  registerKeyFields(Seq(sourceFile, startPos, kind, name))
  def this() {
    this(null, -1, -1, null)
  }

  override def toString: String =
    s"FactEntity($sourceFile, $startPos, $endPos, $name, ${deep(terms)}, ${deep(uses)}, ${deep(related)}, $source)"

  override def equals(other: Any): Boolean = other match {
    case that: FactEntity =>
      sourceFile == that.sourceFile &&
        startPos == that.startPos &&
        endPos == that.endPos &&
        name == that.name &&
        deep(terms) == deep(that.terms) &&
        source == that.source &&
        deep(uses) == deep(that.uses) &&
        deep(related) == deep(that.related)
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(sourceFile, startPos, endPos, name, terms, source, deep(uses), deep(related))
    state.filter(_ != null).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/** Entity class for documentation and comments.
  *
  * @param text text of the documentation
  * @param docType type of the documentation entry
  */
@SuppressWarnings(Array("NullParameter", "NullAssignment")) // Justification: Mapped to solr document
final case class DocEntity(
    override val sourceFile: String,
    override val startPos: Int,
    override val endPos: Int,
    @(Field @field)(Text) text: String,
    @(Field @field)(DocType) docType: String
) extends Entity(sourceFile, startPos, endPos, EntityKind.Documentation.toString) {

  registerKeyFields(Seq(sourceFile, startPos, kind))

  def this() {
    this(null, -1, -1, null, null)
  }
}
