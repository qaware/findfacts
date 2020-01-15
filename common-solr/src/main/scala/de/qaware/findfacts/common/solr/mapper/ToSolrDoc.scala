package de.qaware.findfacts.common.solr.mapper

import scala.collection.JavaConverters._

import de.qaware.findfacts.common.dt.{
  ChildrenField,
  EtField,
  EtKind,
  MultiValuedField,
  OptionalField,
  SingleValuedField
}
import de.qaware.findfacts.common.solr.SolrSchema
import org.apache.solr.common.SolrInputDocument
import shapeless.labelled.FieldType
import shapeless.tag.{@@, Tagged}
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, LabelledGeneric, Lazy, Witness}

/** Solr document mapper type class.
  *
  * @tparam A typed representation
  */
trait ToSolrDoc[A] {

  /** Maps an object of conrete type to solr document.
    *
    * @param elem to map
    * @return mapped solr input document
    */
  def toSolrDoc(elem: A): SolrInputDocument
}

/** Typeclass object providing idiomatic typeclass members and implicits. */
object ToSolrDoc {
  def apply[A](implicit mapper: ToSolrDoc[A]): ToSolrDoc[A] = mapper
  def instance[A](mapFn: A => SolrInputDocument): ToSolrDoc[A] = (elem: A) => mapFn(elem)

  // scalastyle:off scaldoc

  /** HNil impl. */
  implicit val hnilToSolrDoc: ToSolrDoc[HNil] = instance(_ => new SolrInputDocument())

  /** HList impls. */
  def hlistToSolrDoc[F <: EtField, K, H, T <: HList](witness: Witness.Aux[F], tMapper: ToSolrDoc[T])(
      mapFn: (String, H, SolrInputDocument) => SolrInputDocument): ToSolrDoc[FieldType[K, H @@ F] :: T] = instance {
    elem: (H :: T) =>
      val fieldWitness: EtField = witness.value
      val fieldName = SolrSchema.getFieldName(fieldWitness)
      val doc = tMapper.toSolrDoc(elem.tail)
      mapFn(fieldName, elem.head, doc)
  }

  /** Single-valued field impl. */
  implicit def hlistSingleValueToSolrDoc[F <: EtField with SingleValuedField[_], K, H, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: ToSolrDoc[T]
  ): ToSolrDoc[FieldType[K, H @@ F] :: T] = hlistToSolrDoc(witness, tMapper) {
    case (fieldName, value, doc) =>
      doc.addField(fieldName, value.toString)
      doc
  }

  /** Optional field impl. */
  implicit def hlistOptionalToSolrDoc[B, F <: EtField with OptionalField[B], K, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: ToSolrDoc[T]
  ): ToSolrDoc[FieldType[K, Option[B] @@ F] :: T] = hlistToSolrDoc(witness, tMapper) {
    case (fieldName, value, doc) =>
      doc.addField(fieldName, value.map(_.toString).orNull)
      doc
  }

  /** Multi-valued field impl. */
  implicit def hlistMultiValueToSolrDoc[B, F <: EtField with MultiValuedField[B], K, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: ToSolrDoc[T]
  ): ToSolrDoc[FieldType[K, List[B] @@ F] :: T] = hlistToSolrDoc(witness, tMapper) {
    case (fieldName, values, doc) =>
      doc.addField(fieldName, values.map(_.toString).toArray)
      doc
  }

  /** Children field impl. */
  implicit def hChildrenToSolrDoc[C, F <: EtField with ChildrenField[C], K, T <: HList](
      implicit
      witness: Witness.Aux[F],
      cMapper: ToSolrDoc[C],
      tMapper: ToSolrDoc[T]): ToSolrDoc[FieldType[K, List[C] @@ F] :: T] = hlistToSolrDoc(witness, tMapper) {
    case (_, children, doc) =>
      doc.addChildDocuments(children.map(cMapper.toSolrDoc).asJava)
      doc
  }

  /** CNil impl. */
  implicit val cnilToSolrDoc: ToSolrDoc[CNil] = instance(_.impossible)

  /** Coproduct impl. */
  implicit def genCoProdToSolrDoc[K <: Symbol, EK <: EtKind.Value, L <: Tagged[EK], R <: Coproduct](
      implicit
      witness: Witness.Aux[EK],
      lMapper: Lazy[ToSolrDoc[L]],
      rMapper: ToSolrDoc[R]): ToSolrDoc[FieldType[K, L @@ EK] :+: R] =
    instance(_.eliminate(elem => {
      val doc = lMapper.value.toSolrDoc(elem)
      doc.addField(SolrSchema.Kind.toString, witness.value.toString)
      doc
    }, rMapper.toSolrDoc))

  /** Labelled generic impl. */
  implicit def genericToSolrDoc[A, Repr](
      implicit generic: LabelledGeneric.Aux[A, Repr],
      hlMapper: Lazy[ToSolrDoc[Repr]]): ToSolrDoc[A] = instance(elem => hlMapper.value.toSolrDoc(generic.to(elem)))
}
