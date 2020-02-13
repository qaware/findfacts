package de.qaware.findfacts.common.solr.mapper

import java.util.{List => JList}

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.Try

import de.qaware.findfacts.common.da.api.Variant.Discriminator
import de.qaware.findfacts.common.da.api.{
  ChildrenField,
  Field,
  MultiValuedField,
  OptionalField,
  SingleValuedField,
  Variant
}
import enumeratum.EnumEntry
import org.apache.solr.common.SolrDocument
import shapeless.tag.{@@, Tagged}
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy, Witness}

/** Solr document mapper type class.
  *
  * @tparam A typed representation
  */
trait FromSolrDoc[A] {

  /** Maps a solr document to a representation type.
    *
    * @param doc solr document to map
    * @return typed result
    */
  def fromSolrDoc(doc: SolrDocument): Try[A]

  /** Gets the set of solr fields required for this mapping.
    *
    * @return set of required solr fields
    */
  def getSolrFields: Set[Field]
}

/** Typeclass object providing idiomatic typeclass members and implicits. */
object FromSolrDoc {
  // scalastyle:off scaladoc Justification: idiomatic typeclass
  def apply[A](implicit mapper: FromSolrDoc[A]): FromSolrDoc[A] = mapper
  def instance[A](fields: Set[Field], mapFn: SolrDocument => Try[A]): FromSolrDoc[A] = new FromSolrDoc[A] {
    override def fromSolrDoc(doc: SolrDocument): Try[A] = mapFn(doc)
    override val getSolrFields: Set[Field] = fields
  }

  /** HNil impl. */
  implicit val hnilFromSolrDoc: FromSolrDoc[HNil] = instance(Set.empty, _ => Try(HNil))

  /* HList impl */
  implicit def hlistFromSolrDoc[H, T <: HList](
      implicit hMapper: FromSolrDoc[H],
      tMapper: FromSolrDoc[T]): FromSolrDoc[H :: T] =
    instance(hMapper.getSolrFields ++ tMapper.getSolrFields, { doc =>
      hMapper.fromSolrDoc(doc).flatMap(h => tMapper.fromSolrDoc(doc).map(h :: _))
    })

  /** CNil impl. */
  implicit val cnilFromSolrDoc: FromSolrDoc[CNil] = instance(Set.empty, doc => {
    Try { throw new IllegalStateException(s"Coproduct variant not handled: $doc") }
  })

  /** Coproduct impl for discriminator tags only */
  @SuppressWarnings(Array("AsInstanceOf", "BoundedByFinalType"))
  implicit def genCoProdFromSolrDoc[
      B <: EnumEntry,
      V <: B,
      F <: SingleValuedField[B],
      L <: Tagged[Variant[B, F, V]],
      R <: Coproduct](
      implicit
      fieldWitness: Witness.Aux[F],
      variantWitness: Witness.Aux[V],
      lMapper: Lazy[FromSolrDoc[L]],
      rMapper: FromSolrDoc[R]): FromSolrDoc[(L with Discriminator[B, F, V]) :+: R] = instance(
    rMapper.getSolrFields ++ lMapper.value.getSolrFields + fieldWitness.value,
    doc => {
      Try {
        val variantField = fieldWitness.value
        val actualVariantValue = doc.getFieldValue(variantField.name)
        val expectedVariant: B = variantWitness.value

        // For complex coproducts, the variant type of the document might be null for the expected variant enum.
        // This is because the other types in the coproduct might define other variant enums in nested types.
        if (actualVariantValue != null && variantField.fromJsonString(actualVariantValue.toString) == expectedVariant) {
          // Right type of entity found (it is L)
          lMapper.value.fromSolrDoc(doc).map(_.asInstanceOf[L @@ Variant[_, F, V]]).map(Inl(_))
        } else {
          // Type has to be in coproduct, or does not exist.
          rMapper.fromSolrDoc(doc).map(Inr(_))
        }
      }.flatten
    }
  )

  /** Labelled generic impl. */
  implicit def genericFromSolrDoc[A, Repr](
      implicit generic: Generic.Aux[A, Repr],
      hlMapper: Lazy[FromSolrDoc[Repr]]): FromSolrDoc[A] =
    instance(hlMapper.value.getSolrFields, hlMapper.value.fromSolrDoc(_).map(generic.from))

  // Concrete field type impls
  @SuppressWarnings(Array("AsInstanceOf"))
  def fieldFromSolrDoc[F <: Field, A](field: F, moreFields: Set[Field] = Set.empty)(
      mapFn: (SolrDocument, F) => A): FromSolrDoc[A @@ F] =
    instance(moreFields + field, doc => Try(mapFn(doc, field).asInstanceOf[A @@ F]))

  // scalastyle:off null
  /** Single-valued field impl. */
  @SuppressWarnings(Array("AsInstanceOf"))
  implicit def singleValuedFromSolrDoc[F <: SingleValuedField[_], A](
      implicit witness: Witness.Aux[F]): FromSolrDoc[A @@ F] = fieldFromSolrDoc(witness.value) {
    case (doc, field) =>
      doc.get(field.name) match {
        case null => throw new IllegalArgumentException(s"Doc did not contain field ${field.name}")
        case _: JList[_] =>
          throw new IllegalArgumentException(s"Got multi-valued result for single-valued field ${field.name}")
        case solrField: Any => field.fromJsonString(solrField.toString).asInstanceOf[A]
      }
  }

  /** Optional field impl. */
  implicit def hlistOptionalFromSolrDoc[A, F <: OptionalField[A]](
      implicit witness: Witness.Aux[F]): FromSolrDoc[Option[A] @@ F] = fieldFromSolrDoc(witness.value) {
    case (doc, field) =>
      doc.get(field.name) match {
        case null => None
        case _: JList[_] =>
          throw new IllegalArgumentException(s"Got multi-valued result for single-valued field ${field.name}")
        case solrField: Any => Some(field.fromJsonString(solrField.toString))
      }
  }

  /** Multi-valued field impl. */
  implicit def hlistMultiValueFromSolrDoc[B, F <: MultiValuedField[B]](
      implicit witness: Witness.Aux[F]): FromSolrDoc[List[B] @@ F] = fieldFromSolrDoc(witness.value) {
    case (doc, field) =>
      doc.get(field.name) match {
        case null => List.empty
        case solrField: JList[_] => solrField.asScala.toList.map(_.toString).map(field.fromJsonString)
        case _ =>
          throw new IllegalArgumentException(s"Got single-valued result for multi-valued field ${field.name}")
      }
  }

  /** Children field impl. */
  @SuppressWarnings(Array("TryGet"))
  implicit def hlistChildFromSolrDoc[C, F <: ChildrenField[C]](
      implicit witness: Witness.Aux[F],
      cMapper: FromSolrDoc[C]): FromSolrDoc[List[C] @@ F] = fieldFromSolrDoc(witness.value, cMapper.getSolrFields) {
    case (doc, _) =>
      doc.getChildDocuments match {
        case null => List.empty
        case children: JList[SolrDocument] => children.asScala.map(cMapper.fromSolrDoc(_).get).toList
      }
  }
}
