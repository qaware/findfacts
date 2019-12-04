package de.qaware.findfacts.core.solrimpl

import de.qaware.findfacts.common.dt.EtField
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
  def mapSolrDocument(doc: SolrDocument): A
}

/** Typeclass object providing typeclass members and implicits. */
object SolrDocMapper {
  def apply[A](implicit mapper: SolrDocMapper[A]): SolrDocMapper[A] = mapper
  def instance[A](mapFn: SolrDocument => A): SolrDocMapper[A] = (doc: SolrDocument) => mapFn(doc)

  /** HNil impl. */
  implicit def hnilSolrDocMapper: SolrDocMapper[HNil] = instance(_ => HNil)

  /** HList impl. */
  implicit def hlistSolrDocMapper[F <: EtField, K, H, T <: HList](
      implicit
      witness: Witness.Aux[F],
      tMapper: SolrDocMapper[T],
  ): SolrDocMapper[FieldType[K, H @@ F] :: T] = {
    val fieldName = SolrSchema.getFieldName(witness.value)
    instance { doc =>
      val head = doc.get(fieldName).asInstanceOf[FieldType[K, H @@ F]]
      val tail = tMapper.mapSolrDocument(doc)
      head :: tail
    }
  }

  /** Generic impl. */
  implicit def genericSolrDocMapper[A, Repr](
      implicit generic: LabelledGeneric.Aux[A, Repr],
      hlMapper: Lazy[SolrDocMapper[Repr]]): SolrDocMapper[A] = {
    instance { doc =>
      generic.from(hlMapper.value.mapSolrDocument(doc))
    }
  }
}
