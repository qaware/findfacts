package de.qaware.findfacts.common.solr

import de.qaware.findfacts.common.dt.EtKind
import de.qaware.findfacts.common.solr.Record.Id

import scala.annotation.meta.field
// scalastyle:off UnderscoreImportChecker
import de.qaware.findfacts.common.solr.SolrSchema._
// scalastyle:on UnderscoreImportChecker
import de.qaware.findfacts.scalautils.NullableArray.deep
import org.apache.solr.client.solrj.beans.Field
import org.apache.solr.update.processor.Lookup3Signature

/** Base record class. ID is generated within solr from sourceFile, startPos and endPos.
  *
  * @param sourceFile relative path to file containing record
  * @param startPosition   token in src at which record begins
  * @param endPosition     token in src at which record ends
  * @param kind       of this record. String value for solr compatibility from [[de.qaware.findfacts.common.dt.EtKind]] enum
  */
class Record(
    @(Field @field)(SourceFile) val sourceFile: String,
    @(Field @field)(StartPosition) val startPosition: Int,
    @(Field @field)(EndPosition) val endPosition: Int,
    @(Field @field)(Kind) val kind: String
) {
  def this() = this(null, 0, 0, null)

  /** Registers the key fields of this record. Must be called in subclasses once.
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
    * @return solr id of this record
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
object Record {

  /** Ids are stored as strings. */
  type Id = String
}

// scalastyle:off
/** Sum type for semantic theory records.
  *
  * @param name of the record
  * @param proposition semantic representation of the record
  * @param related ids of records defined at the same position
  * @param sourceText of record
  */
sealed abstract class TheoryRecord(
    override val sourceFile: String,
    override val startPosition: Int,
    override val endPosition: Int,
    override val kind: String,
    @(Field @field)(Name) val name: String,
    @(Field @field)(Proposition) val proposition: String,
    @(Field @field)(Related) val related: Array[Id],
    @(Field @field)(SourceText) val sourceText: String)
    extends Record(sourceFile, startPosition, endPosition, kind)

/** Record class for type definitions.
  *
  * @param uses id of types that this type definition uses
  */
@SuppressWarnings(Array("NullParameter", "NullAssignment")) // Justification: Mapped to solr document
final case class TypeRecord(
    override val sourceFile: String,
    override val startPosition: Int,
    override val endPosition: Int,
    override val name: String,
    override val proposition: String = null,
    @(Field @field)(TypeUses) uses: Array[Id] = null,
    override val related: Array[Id] = null,
    override val sourceText: String = null
) extends TheoryRecord(
      sourceFile,
      startPosition,
      endPosition,
      EtKind.Type.toString,
      name,
      proposition,
      related,
      sourceText) {
  registerKeyFields(Seq(sourceFile, startPosition, kind, name))

  def this() {
    this(null, -1, -1, null)
  }

  override def toString: String =
    s"TypeRecord($sourceFile, $startPosition, $endPosition, " +
      s"$name, $proposition, ${deep(uses)}, ${deep(related)}, $sourceText)"

  override def equals(other: Any): Boolean = other match {
    case that: TypeRecord =>
      sourceFile == that.sourceFile &&
        startPosition == that.startPosition &&
        endPosition == that.endPosition &&
        name == that.name &&
        proposition == that.proposition &&
        sourceText == that.sourceText &&
        deep(uses) == deep(that.uses) &&
        deep(related) == deep(that.related)
    case _ => false
  }

  override def hashCode(): Int = {
    val state =
      Seq(sourceFile, startPosition, endPosition, name, proposition, sourceText, deep(uses), deep(related))
    state.filter(_ != null).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/** Record class for constants.
  *
  * @param constType functional type of the constant
  * @param typeUses ids of types that the type of this constant uses
  * @param proposition definition(s) term in string representation
  */
@SuppressWarnings(Array("NullParameter", "NullAssignment")) // Justification: Mapped to solr document
final case class ConstRecord(
    override val sourceFile: String,
    override val startPosition: Int,
    override val endPosition: Int,
    override val name: String,
    @(Field @field)(ConstantType) constType: String = null,
    @(Field @field)(TypeUses) typeUses: Array[Id] = null,
    override val proposition: String = null,
    @(Field @field)(PropositionUses) propositionUses: Array[Id] = null,
    override val related: Array[Id] = null,
    override val sourceText: String = null
) extends TheoryRecord(
      sourceFile,
      startPosition,
      endPosition,
      EtKind.Constant.toString,
      name,
      proposition,
      related,
      sourceText) {
  registerKeyFields(Seq(sourceFile, startPosition, kind, name))

  def this() {
    this(null, -1, -1, null)
  }

  override def toString: String = {
    s"ConstRecord($sourceFile, $startPosition, $endPosition, $name, $constType, ${deep(typeUses)}, " +
      s"$proposition, ${deep(propositionUses)}, ${deep(related)}, $sourceText)"
  }

  override def equals(other: Any): Boolean = other match {
    case that: ConstRecord =>
      sourceFile == that.sourceFile &&
        startPosition == that.startPosition &&
        endPosition == that.endPosition &&
        name == that.name &&
        constType == that.constType &&
        deep(typeUses) == deep(that.propositionUses) &&
        sourceText == that.sourceText &&
        proposition == that.proposition &&
        deep(propositionUses) == deep(that.propositionUses) &&
        deep(related) == deep(that.related)
    case _ => false
  }

  override def hashCode(): Int = {
    val state =
      Seq(
        sourceFile,
        startPosition,
        endPosition,
        name,
        constType,
        deep(typeUses),
        sourceText,
        proposition,
        deep(propositionUses),
        deep(related))
    state.filter(_ != null).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/** Record class for proven lemmas and theories.
  *
  * @param sourceText term of this fact
  * @param uses ids of records that the fact uses
  */
@SuppressWarnings(Array("NullParameter", "NullAssignment")) // Justification: Mapped to solr document
final case class FactRecord(
    override val sourceFile: String,
    override val startPosition: Int,
    override val endPosition: Int,
    override val name: String,
    override val proposition: String = null,
    @(Field @field)(PropositionUses) uses: Array[Id] = null,
    @(Field @field)(ProofUses) proofUses: Array[Id] = null,
    override val related: Array[Id] = null,
    override val sourceText: String = null
) extends TheoryRecord(
      sourceFile,
      startPosition,
      endPosition,
      EtKind.Fact.toString,
      name,
      proposition,
      related,
      sourceText) {
  registerKeyFields(Seq(sourceFile, startPosition, kind, name))

  def this() {
    this(null, -1, -1, null)
  }

  override def toString: String =
    s"FactRecord($sourceFile, $startPosition, $endPosition, $name, $proposition, " +
      s"${deep(uses)}, ${deep(proofUses)}, ${deep(related)}, $sourceText)"

  override def equals(other: Any): Boolean = other match {
    case that: FactRecord =>
      sourceFile == that.sourceFile &&
        startPosition == that.startPosition &&
        endPosition == that.endPosition &&
        name == that.name &&
        proposition == that.proposition &&
        sourceText == that.sourceText &&
        deep(uses) == deep(that.uses) &&
        deep(proofUses) == deep(that.proofUses) &&
        deep(related) == deep(that.related)
    case _ => false
  }

  override def hashCode(): Int = {
    val state =
      Seq(
        sourceFile,
        startPosition,
        endPosition,
        name,
        proposition,
        sourceText,
        deep(uses),
        deep(proofUses),
        deep(related))
    state.filter(_ != null).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/** Record class for documentation and comments.
  *
  * @param text text of the documentation
  * @param documentationType type of the documentation entry
  */
@SuppressWarnings(Array("NullParameter", "NullAssignment")) // Justification: Mapped to solr document
final case class DocRecord(
    override val sourceFile: String,
    override val startPosition: Int,
    override val endPosition: Int,
    @(Field @field)(SourceText) text: String,
    @(Field @field)(DocumentationKind) documentationType: String
) extends Record(sourceFile, startPosition, endPosition, EtKind.Documentation.toString) {
  registerKeyFields(Seq(sourceFile, startPosition, kind))

  def this() {
    this(null, -1, -1, null, null)
  }
}
