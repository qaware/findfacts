package de.qaware.findfacts.core.solrimpl

import java.util

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.Try

import de.qaware.findfacts.common.dt.EtField.Kind
import de.qaware.findfacts.common.dt.{EtField, EtKind, MultiValuedField, OptionalField, SingleValuedField}
import de.qaware.findfacts.common.solr.SolrSchema
import org.apache.solr.common.SolrDocument
import shapeless.labelled.FieldType
import shapeless.tag.{@@, Tagged}
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness}

/** Solr document mapper type class.
  *
  * @tparam A typed representation
  */
trait SolrDocMapper[A] {

  /** Maps a solr document to a representation type
    *
    * @param doc solr document to map
    * @return typed result
    */
  def mapSolrDocument(doc: SolrDocument): Try[A]
}

/** Typeclass object providing typeclass members and implicits. */
object SolrDocMapper {
  def apply[A](implicit mapper: SolrDocMapper[A]): SolrDocMapper[A] = mapper
  def instance[A](mapFn: SolrDocument => Try[A]): SolrDocMapper[A] = (doc: SolrDocument) => mapFn(doc)

  // scalastyle:off scaladoc

  /** HNil impl. */
  implicit val hnilSolrDocMapper: SolrDocMapper[HNil] = instance(_ => Try(HNil))

  /** HList impl. */
  implicit def hlistSolrDocMapper[F <: EtField, K, H, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: SolrDocMapper[T]
  ): SolrDocMapper[FieldType[K, H @@ F] :: T] = {
    val fieldWitness: EtField = witness.value
    instance { doc =>
      Try {
        val fieldName = SolrSchema.getFieldName(fieldWitness)

        // Map field to typed representation
        val head = (doc.get(fieldName) match {
          case null =>
            fieldWitness match {
              case _: SingleValuedField => throw new IllegalArgumentException(s"Doc did not contain field $fieldName")
              case _: OptionalField => None
              case _: MultiValuedField => List.empty
            }
          case solrField: util.List[_] =>
            fieldWitness match {
              case _: SingleValuedField | _: OptionalField =>
                throw new IllegalArgumentException(s"Got multi-valued result for single-valued field $fieldName")
              case _: MultiValuedField => solrField.asScala.toList
            }
          case solrField =>
            fieldWitness match {
              case _: SingleValuedField => solrField
              case _: OptionalField => Some(solrField)
              case _: MultiValuedField =>
                throw new IllegalArgumentException(s"Got single-valued fresult for multi-valued field $fieldName")
            }
        }).asInstanceOf[FieldType[K, H @@ F]]

        // Build complete product type
        tMapper.mapSolrDocument(doc).map(head :: _)
      }.flatten
    }
  }

  /** CNil impl. */
  implicit val cnilSolrDocMapper: SolrDocMapper[CNil] = instance { doc =>
    EtKind
      .fromString(doc.getFieldValue(SolrSchema.getFieldName(Kind)).toString)
      .map(kind => throw new IllegalStateException(s"Kind $kind not handled!"))
  }

  /** Coproduct impl. */
  implicit def genCoProdSolrDocMapper[K0 <: Symbol, K <: EtKind, L <: Tagged[K], R <: Coproduct](
      implicit
      witness: Witness.Aux[K],
      // Strip any tags while searching for the SolrDocMapper of L (makes search easier)
      lMapper: Lazy[SolrDocMapper[L]],
      rMapper: SolrDocMapper[R]): SolrDocMapper[FieldType[K0, L @@ K] :+: R] = instance { doc =>
    EtKind.fromString(doc.getFieldValue(SolrSchema.getFieldName(Kind)).toString) flatMap { kind =>
      if (kind == witness.value) {
        // Right type of entity found (it is L)
        lMapper.value.mapSolrDocument(doc).map(_.asInstanceOf[FieldType[K0, L @@ K]]).map(Inl(_))
      } else {
        // Type has to be in coproduct, or does not exist.
        rMapper.mapSolrDocument(doc).map(Inr(_))
      }
    }
  }

  /** Labelled generic impl. */
  implicit def genericSolrDocMapper[A, Repr](
      implicit generic: LabelledGeneric.Aux[A, Repr],
      hlMapper: Lazy[SolrDocMapper[Repr]]): SolrDocMapper[A] = {
    instance { doc =>
      val gen = hlMapper.value.mapSolrDocument(doc)
      gen.map(generic.from)
    }
  }
}
