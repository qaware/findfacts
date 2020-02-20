package de.qaware.findfacts.core.solrimpl

import scala.util.Try

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.common.utils.TryUtils._
import de.qaware.findfacts.core.solrimpl.SolrQueryLiterals.{ParentTag, TagParam}
import de.qaware.findfacts.core.{FieldFilter, Filter}

/** Case class for filters and child filters.
  *
  * @param fqs filter queries
  * @param childFqs child filter queries
  */
case class Filters(fqs: Seq[String], childFqs: Seq[String])

/** Mapper to map field filters to solr filter strings.
  *
  * @param filterMapper to map filters
  */
class SolrFieldFilterMapper(filterMapper: SolrFilterMapper) {

  private def mapChildFilter(field: EtField, filter: Filter)(implicit queryService: SolrQueryService): Try[String] = {
    filterMapper.mapFilter(filter).map(f => s"{!$TagParam=${field.name}}${field.name}:$f")
  }

  private def mapParentFilter(field: EtField, filter: Filter)(implicit queryService: SolrQueryService): Try[String] = {
    filterMapper.mapFilter(filter).map(f => s"{!$TagParam=$ParentTag,${field.name}}${field.name}:$f")
  }

  /** Map a list of filters to tagged parent and child solr query strings.
    *
    * @param filters all filters to filter for
    * @param queryService for recursive queries
    * @return query strings seperated into 'fq's and 'child.fq's, or error if recursive query failed.
    */
  def mapFieldFilters(filters: List[FieldFilter])(implicit queryService: SolrQueryService): Try[Filters] = {
    val (childFilters, parentFilters) = filters.partition(!_.field.isParent)

    val fqs: Try[Seq[String]] = parentFilters.map(f => mapParentFilter(f.field, f.filter))
    val childFqs: Try[Seq[String]] = childFilters.map(f => mapChildFilter(f.field, f.filter))

    for {
      fqParams <- fqs
      childFqParams <- childFqs
    } yield {
      Filters(fqParams, childFqParams)
    }
  }
}
