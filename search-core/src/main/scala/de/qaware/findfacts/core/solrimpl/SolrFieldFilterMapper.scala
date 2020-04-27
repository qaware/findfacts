package de.qaware.findfacts.core.solrimpl

import scala.util.Try

import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._

import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.{FieldFilter, Filter}

/**
 * Case class for filters and child filters.
 *
 * @param fqs filter queries
 * @param childFqs child filter queries
 */
final case class Filters(fqs: Seq[String], childFqs: Seq[String])

/**
 * Mapper to map field filters to solr filter strings.
 *
 * @param filterMapper to map filters
 */
class SolrFieldFilterMapper(filterMapper: SolrFilterMapper) {

  private def mapChildFilter(field: EtField, filter: Filter)(implicit
      index: String,
      queryService: SolrQueryService): Try[String] = {
    filterMapper.mapFilter(filter).map(f => s"{!tag=${field.name}}${field.name}:$f")
  }

  private def mapParentFilter(field: EtField, filter: Filter)(implicit
      index: String,
      queryService: SolrQueryService): Try[String] = {
    filterMapper.mapFilter(filter).map(f => s"{!tag=top,${field.name}}${field.name}:$f")
  }

  /**
   * Map a list of filters to tagged parent and child solr query strings.
   *
   * @param filters all filters to filter for
   * @param index for recursive queries
   * @param queryService for recursive queries
   * @return query strings separated into 'fq's and 'child.fq's, or error if recursive query failed.
   */
  def mapFieldFilters(
      filters: List[FieldFilter])(implicit index: String, queryService: SolrQueryService): Try[Filters] = {
    val (childFilters, parentFilters) = filters.partition(!_.field.isParent)

    val fqs = parentFilters.map(f => mapParentFilter(f.field, f.filter)).sequence
    val childFqs = childFilters.map(f => mapChildFilter(f.field, f.filter)).sequence

    for {
      fqParams <- fqs
      childFqParams <- childFqs
    } yield {
      Filters(fqParams, childFqParams)
    }
  }
}
