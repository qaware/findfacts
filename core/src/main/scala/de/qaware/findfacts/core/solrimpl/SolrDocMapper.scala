package de.qaware.findfacts.core.solrimpl

import java.util

import scala.collection.JavaConverters._
import scala.language.postfixOps
import scala.util.Try

import de.qaware.findfacts.common.dt.{EtField, MultiValuedField}
import de.qaware.findfacts.common.solr.SolrSchema
import org.apache.solr.common.SolrDocument
import shapeless.labelled.FieldType
import shapeless.tag.@@
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}

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

  /** HNil impl. */
  implicit def hnilSolrDocMapper: SolrDocMapper[HNil] = instance(_ => Try(HNil))

  /** HList impl. */
  implicit def hlistSolrDocMapper[F <: EtField, K, H, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: SolrDocMapper[T],
  ): SolrDocMapper[FieldType[K, H @@ F] :: T] = {
    val field: EtField = witness.value
    instance { doc =>
      Try {
        val fieldName = SolrSchema.getFieldName(field)
        val solrField = doc.get(fieldName)

        val head = (solrField match {
          // Solr uses null values for missing fields or empty multi-valued fields
          case null if field.isInstanceOf[MultiValuedField] => List.empty
          case null => throw new IllegalArgumentException(s"Doc did not contain field $fieldName")
          case solrField: util.List[_] if field.isInstanceOf[MultiValuedField] => solrField.asScala.toList
          case _ => solrField
        }).asInstanceOf[FieldType[K, H @@ F]]

        tMapper.mapSolrDocument(doc).map(head :: _)
      }.flatten
    }
  }

  /** Generic impl. */
  implicit def genericSolrDocMapper[A, Repr](
      implicit generic: LabelledGeneric.Aux[A, Repr],
      hlMapper: Lazy[SolrDocMapper[Repr]]): SolrDocMapper[A] = {
    instance { doc =>
      val gen = hlMapper.value.mapSolrDocument(doc)
      gen.map(generic.from)
    }
  }
}
