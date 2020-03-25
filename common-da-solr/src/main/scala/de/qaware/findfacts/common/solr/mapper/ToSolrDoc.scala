package de.qaware.findfacts.common.solr.mapper

import scala.collection.JavaConverters._

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
import org.apache.solr.common.SolrInputDocument
import shapeless.labelled.FieldType
import shapeless.tag.{@@, Tagged}
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, LabelledGeneric, Lazy, Witness}

/** Solr document mapper type class.
  *
  * @tparam A typed representation
  */
trait ToSolrDoc[A] {

  /** Maps an object of concrete type to solr document.
    *
    * @param elem to map
    * @return mapped solr input document
    */
  def toSolrDoc(elem: A): SolrInputDocument
}

/** Typeclass object providing idiomatic typeclass members and implicits. */
object ToSolrDoc {
  // scalastyle:off scaladoc Justification: idiomatic typeclass
  def apply[A](implicit mapper: ToSolrDoc[A]): ToSolrDoc[A] = mapper
  def instance[A](mapFn: A => SolrInputDocument): ToSolrDoc[A] = (elem: A) => mapFn(elem)

  /** HNil impl. */
  implicit val hnilToSolrDoc: ToSolrDoc[HNil] = instance(_ => new SolrInputDocument())

  /** HList impls. */
  def hlistToSolrDoc[F <: Field, K, H, T <: HList](witness: Witness.Aux[F], tMapper: ToSolrDoc[T])(
      mapFn: (F, H, SolrInputDocument) => SolrInputDocument): ToSolrDoc[FieldType[K, H @@ F] :: T] = instance {
    elem: H :: T =>
      val field = witness.value
      val doc = tMapper.toSolrDoc(elem.tail)
      mapFn(field, elem.head, doc)
  }

  /** Single-valued field impl. */
  implicit def hlistSingleValueToSolrDoc[F <: SingleValuedField[_], K, H, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: ToSolrDoc[T]
  ): ToSolrDoc[FieldType[K, H @@ F] :: T] = hlistToSolrDoc(witness, tMapper) {
    case (field, value, doc) =>
      doc.addField(field.name, value.toString)
      doc
  }

  /** Optional field impl. */
  implicit def hlistOptionalToSolrDoc[B, F <: OptionalField[B], K, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: ToSolrDoc[T]
  ): ToSolrDoc[FieldType[K, Option[B] @@ F] :: T] = hlistToSolrDoc(witness, tMapper) {
    case (field, value, doc) =>
      doc.addField(field.name, value.map(_.toString).orNull)
      doc
  }

  /** Multi-valued field impl. */
  implicit def hlistMultiValueToSolrDoc[B, F <: MultiValuedField[B], K, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: ToSolrDoc[T]
  ): ToSolrDoc[FieldType[K, List[B] @@ F] :: T] = hlistToSolrDoc(witness, tMapper) {
    case (field, values, doc) =>
      doc.addField(field.name, values.map(_.toString).toArray)
      doc
  }

  /** Children field impl. */
  implicit def hChildrenToSolrDoc[C, F <: ChildrenField[C], K, T <: HList](
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
  @SuppressWarnings(Array("BoundedByFinalType"))
  implicit def genCoProdToSolrDoc[
      B <: EnumEntry,
      V <: B,
      F <: SingleValuedField[B],
      L <: Tagged[Variant[B, F, V]],
      K <: Symbol,
      R <: Coproduct](
      implicit
      variantWitness: Witness.Aux[V],
      varFieldWitness: Witness.Aux[F],
      lMapper: Lazy[ToSolrDoc[L]],
      rMapper: ToSolrDoc[R]): ToSolrDoc[FieldType[K, L with Discriminator[B, F, V]] :+: R] =
    instance(_.eliminate(elem => {
      val doc = lMapper.value.toSolrDoc(elem)
      doc.addField(varFieldWitness.value.name, variantWitness.value.toString)
      doc
    }, rMapper.toSolrDoc))

  /** Labelled generic impl. */
  implicit def genericToSolrDoc[A, Repr](
      implicit generic: LabelledGeneric.Aux[A, Repr],
      hlMapper: Lazy[ToSolrDoc[Repr]]): ToSolrDoc[A] = instance(elem => hlMapper.value.toSolrDoc(generic.to(elem)))
}
