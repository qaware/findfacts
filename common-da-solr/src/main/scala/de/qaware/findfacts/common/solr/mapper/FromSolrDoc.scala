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
import shapeless.labelled.FieldType
import shapeless.tag.{@@, Tagged}
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}

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
}

/** Typeclass object providing idiomatic typeclass members and implicits. */
object FromSolrDoc {
  // scalastyle:off scaladoc Justification: idiomatic typeclass
  def apply[A](implicit mapper: FromSolrDoc[A]): FromSolrDoc[A] = mapper
  def instance[A](mapFn: SolrDocument => Try[A]): FromSolrDoc[A] = (doc: SolrDocument) => mapFn(doc)

  /** HNil impl. */
  implicit val hnilFromSolrDoc: FromSolrDoc[HNil] = instance(_ => Try(HNil))

  /** HList impls. */
  def hlistFromSolrDoc[F <: Field, K, H, T <: HList](witness: Witness.Aux[F], tMapper: FromSolrDoc[T])(
      mapFn: (SolrDocument, F) => FieldType[K, H @@ F]): FromSolrDoc[FieldType[K, H @@ F] :: T] =
    instance { doc =>
      // Get field anme
      val field = witness.value

      // Extract information
      Try {
        // Map field to typed representation
        val head = mapFn(doc, field)

        // Build complete product type
        tMapper.fromSolrDoc(doc).map(head :: _)
      }.flatten
    }

  // scalastyle:off null
  /** Single-valued field impl. */
  @SuppressWarnings(Array("AsInstanceOf"))
  implicit def hlistSingleValueFromSolrDoc[F <: SingleValuedField[_], K, H, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: FromSolrDoc[T]
  ): FromSolrDoc[FieldType[K, H @@ F] :: T] = hlistFromSolrDoc(witness, tMapper) {
    case (doc, field) =>
      (doc.get(field.name) match {
        case null => throw new IllegalArgumentException(s"Doc did not contain field ${field.name}")
        case _: JList[_] =>
          throw new IllegalArgumentException(s"Got multi-valued result for single-valued field ${field.name}")
        case solrField: Any => field.fromJsonString(solrField.toString)
      }).asInstanceOf[FieldType[K, H @@ F]]
  }

  /** Optional field impl. */
  @SuppressWarnings(Array("AsInstanceOf"))
  implicit def hlistOptionalFromSolrDoc[B, F <: OptionalField[B], K, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: FromSolrDoc[T]
  ): FromSolrDoc[FieldType[K, Option[B] @@ F] :: T] = hlistFromSolrDoc(witness, tMapper) {
    case (doc, field) =>
      (doc.get(field.name) match {
        case null => None
        case _: JList[_] =>
          throw new IllegalArgumentException(s"Got multi-valued result for single-valued field ${field.name}")
        case solrField: Any => Some(field.fromJsonString(solrField.toString))
      }).asInstanceOf[FieldType[K, Option[B] @@ F]]
  }

  /** Multi-valued field impl. */
  @SuppressWarnings(Array("AsInstanceOf"))
  implicit def hlistMultiValueFromSolrDoc[B, F <: MultiValuedField[B], K, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: FromSolrDoc[T]
  ): FromSolrDoc[FieldType[K, List[B] @@ F] :: T] = hlistFromSolrDoc(witness, tMapper) {
    case (doc, field) =>
      (doc.get(field.name) match {
        case null => List.empty
        case solrField: JList[_] => solrField.asScala.toList.map(_.toString).map(field.fromJsonString)
        case _ =>
          throw new IllegalArgumentException(s"Got single-valued result for multi-valued field ${field.name}")
      }).asInstanceOf[FieldType[K, List[B] @@ F]]
  }

  /** Children field impl. */
  @SuppressWarnings(Array("AsInstanceOf", "TryGet"))
  implicit def hlistChildFromSolrDoc[C, F <: ChildrenField[C], K, T <: HList](
      implicit
      witness: Witness.Aux[F],
      cMapper: FromSolrDoc[C],
      tMapper: FromSolrDoc[T]
  ): FromSolrDoc[FieldType[K, List[C] @@ F] :: T] = hlistFromSolrDoc(witness, tMapper) {
    case (doc, _) =>
      (doc.getChildDocuments match {
        case null => List.empty
        case children: JList[SolrDocument] => children.asScala.map(cMapper.fromSolrDoc(_).get).toList
      }).asInstanceOf[FieldType[K, List[C] @@ F]]
  }

  /** CNil impl. */
  implicit val cnilFromSolrDoc: FromSolrDoc[CNil] = instance { doc =>
    Try { throw new IllegalStateException(s"Coproduct variant not handled: $doc") }
  }

  /** Coproduct impl. */
  @SuppressWarnings(Array("AsInstanceOf", "BoundedByFinalType"))
  implicit def genCoProdFromSolrDoc[
      B <: EnumEntry,
      V <: B,
      F <: SingleValuedField[B],
      L <: Tagged[Variant[B, F, V]],
      K <: Symbol,
      R <: Coproduct](
      implicit
      fieldWitness: Witness.Aux[F],
      variantWitness: Witness.Aux[V],
      lMapper: Lazy[FromSolrDoc[L]],
      rMapper: FromSolrDoc[R]): FromSolrDoc[FieldType[K, L with Discriminator[B, F, V]] :+: R] = instance { doc =>
    Try {
      val docVarField = fieldWitness.value
      val docVariant: B = docVarField.fromJsonString(doc.getFieldValue(docVarField.name).toString)
      val typeVariant: B = variantWitness.value

      if (docVariant == typeVariant) {
        // Right type of entity found (it is L)
        lMapper.value.fromSolrDoc(doc).map(_.asInstanceOf[FieldType[K, L @@ Variant[_, F, V]]]).map(Inl(_))
      } else {
        // Type has to be in coproduct, or does not exist.
        rMapper.fromSolrDoc(doc).map(Inr(_))
      }
    }.flatten
  }

  /** Labelled generic impl. */
  implicit def genericFromSolrDoc[A, Repr](
      implicit generic: LabelledGeneric.Aux[A, Repr],
      hlMapper: Lazy[FromSolrDoc[Repr]]): FromSolrDoc[A] = instance(hlMapper.value.fromSolrDoc(_).map(generic.from))
}
