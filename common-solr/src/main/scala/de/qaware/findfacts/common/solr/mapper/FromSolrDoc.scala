package de.qaware.findfacts.common.solr.mapper

import java.util.{List => JList}

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.Try

import de.qaware.findfacts.common.dt.EtField.Kind
import de.qaware.findfacts.common.dt.{
  ChildrenField,
  EtField,
  EtKind,
  MultiValuedField,
  OptionalField,
  SingleValuedField
}
import de.qaware.findfacts.common.solr.SolrSchema
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
  def apply[A](implicit mapper: FromSolrDoc[A]): FromSolrDoc[A] = mapper
  def instance[A](mapFn: SolrDocument => Try[A]): FromSolrDoc[A] = (doc: SolrDocument) => mapFn(doc)

  // scalastyle:off scaladoc

  /** HNil impl. */
  implicit val hnilFromSolrDoc: FromSolrDoc[HNil] = instance(_ => Try(HNil))

  /** HList impls. */
  def hlistFromSolrDoc[F <: EtField, K, H, T <: HList](witness: Witness.Aux[F], tMapper: FromSolrDoc[T])(
      mapFn: (SolrDocument, F, String) => FieldType[K, H @@ F]): FromSolrDoc[FieldType[K, H @@ F] :: T] =
    instance { doc =>
      // Get field anme
      val field = witness.value

      // Extract information
      Try {
        val fieldName = SolrSchema.getFieldName(field)

        // Map field to typed representation
        val head = mapFn(doc, field, fieldName)

        // Build complete product type
        tMapper.fromSolrDoc(doc).map(head :: _)
      }.flatten
    }

  /** Single-valued field impl. */
  implicit def hlistSingleValueFromSolrDoc[F <: EtField with SingleValuedField[_], K, H, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: FromSolrDoc[T]
  ): FromSolrDoc[FieldType[K, H @@ F] :: T] = hlistFromSolrDoc(witness, tMapper) {
    case (doc, field, fieldName) =>
      (doc.get(fieldName) match {
        case null => throw new IllegalArgumentException(s"Doc did not contain field $fieldName")
        case _: JList[_] =>
          throw new IllegalArgumentException(s"Got multi-valued result for single-valued field $fieldName")
        case solrField => field.fromJsonString(solrField.toString)
      }).asInstanceOf[FieldType[K, H @@ F]]
  }

  /** Optional field impl. */
  implicit def hlistOptionalFromSolrDoc[B, F <: EtField with OptionalField[B], K, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: FromSolrDoc[T]
  ): FromSolrDoc[FieldType[K, Option[B] @@ F] :: T] = hlistFromSolrDoc(witness, tMapper) {
    case (doc, field, fieldName) =>
      (doc.get(fieldName) match {
        case null => None
        case _: JList[_] =>
          throw new IllegalArgumentException(s"Got multi-valued result for single-valued field $fieldName")
        case solrField => Some(field.fromJsonString(solrField.toString))
      }).asInstanceOf[FieldType[K, Option[B] @@ F]]
  }

  /** Multi-valued field impl. */
  implicit def hlistMultiValueFromSolrDoc[B, F <: EtField with MultiValuedField[B], K, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: FromSolrDoc[T]
  ): FromSolrDoc[FieldType[K, List[B] @@ F] :: T] = hlistFromSolrDoc(witness, tMapper) {
    case (doc, field, fieldName) =>
      (doc.get(fieldName) match {
        case null => List.empty
        case solrField: JList[_] => solrField.asScala.toList.map(_.toString).map(field.fromJsonString)
        case _ =>
          throw new IllegalArgumentException(s"Got single-valued result for multi-valued field $fieldName")
      }).asInstanceOf[FieldType[K, List[B] @@ F]]
  }

  /** Children field impl. */
  implicit def hlistChildFromSolrDoc[C, F <: EtField with ChildrenField[C], K, T <: HList](
      implicit
      witness: Witness.Aux[F],
      cMapper: FromSolrDoc[C],
      tMapper: FromSolrDoc[T]
  ): FromSolrDoc[FieldType[K, List[C] @@ F] :: T] = hlistFromSolrDoc(witness, tMapper) {
    case (doc, _, _) =>
      (doc.getChildDocuments match {
        case null => List.empty
        case children: JList[SolrDocument] => children.asScala.map(cMapper.fromSolrDoc(_).get).toList
      }).asInstanceOf[FieldType[K, List[C] @@ F]]
  }

  /** CNil impl. */
  implicit val cnilFromSolrDoc: FromSolrDoc[CNil] = instance { doc =>
    EtKind
      .fromString(doc.getFieldValue(SolrSchema.getFieldName(Kind)).toString)
      .map(kind => throw new IllegalStateException(s"Kind $kind not handled!"))
  }

  /** Coproduct impl. */
  implicit def genCoProdFromSolrDoc[K <: Symbol, EK <: EtKind.Value, L <: Tagged[EK], R <: Coproduct](
      implicit
      witness: Witness.Aux[EK],
      // Strip any tags while searching for the FromSolrDoc of L (makes search easier)
      lMapper: Lazy[FromSolrDoc[L]],
      rMapper: FromSolrDoc[R]): FromSolrDoc[FieldType[K, L @@ EK] :+: R] = instance { doc =>
    EtKind.fromString(doc.getFieldValue(SolrSchema.getFieldName(Kind)).toString) flatMap { kind =>
      if (kind == witness.value) {
        // Right type of entity found (it is L)
        lMapper.value.fromSolrDoc(doc).map(_.asInstanceOf[FieldType[K, L @@ EK]]).map(Inl(_))
      } else {
        // Type has to be in coproduct, or does not exist.
        rMapper.fromSolrDoc(doc).map(Inr(_))
      }
    }
  }

  /** Labelled generic impl. */
  implicit def genericFromSolrDoc[A, Repr](
      implicit generic: LabelledGeneric.Aux[A, Repr],
      hlMapper: Lazy[FromSolrDoc[Repr]]): FromSolrDoc[A] = instance(hlMapper.value.fromSolrDoc(_).map(generic.from))
}
