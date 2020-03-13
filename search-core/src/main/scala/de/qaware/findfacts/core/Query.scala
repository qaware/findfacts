package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.EtField

/** Filter for a field. */
case class FieldFilter(field: EtField, filter: Filter)

/** Union type for all filters. */
sealed trait Filter

/** Inverts a filter.
  *
  * @param filter to invert
  */
final case class Not(filter: Filter) extends Filter

/** Disjunction of filters.
  *
  * @param f1 first
  * @param f2 second
  * @param fn more filters
  */
final case class Or(f1: Filter, f2: Filter, fn: Filter*) extends Filter

/** Conjunction of filters.
  *
  * @param f1 first
  * @param f2 second
  * @param fn more filters
  */
final case class And(f1: Filter, f2: Filter, fn: Filter*) extends Filter

/** Fuzzy filter term. May use '*' and '?' Wildcards. Space (usually) splits tokens.
  *
  * @param inner string
  */
final case class Term(inner: String) extends Filter

/** Exact filter term. Wildcards will be escaped.
  *
  * @param inner string
  */
final case class Exact(inner: String) extends Filter

/** Range query for numerical fields
  *
  * @param from start of range, inclusive
  * @param to end of range, inclusive
  */
final case class InRange(from: Int, to: Int) extends Filter

/** Filters for ids in a recursive query.
  *
  * @param ofField to extract in query results
  * @param query filters in which results to search
  */
final case class InResult(ofField: EtField, query: List[FieldFilter]) extends Filter

/** Query to facet on, i.e. list number of occurrences for each distinct value.
  *
  * @param filters to apply before facet
  * @param fields to facet on
  * @param maxFacets maximum number of facet values. Fields for which this value is exceeded will be ignored
  */
final case class FacetQuery(filters: List[FieldFilter], fields: Set[EtField], maxFacets: Int = 100)

/** Query to filter for results.
  *
  * @param filters to apply
  * @param pageSize maximum number of results to stream back
  * @param cursor to specify which page should be fetched, if it isn't the first
  */
final case class FilterQuery(filters: List[FieldFilter], pageSize: Int = 10, cursor: Option[String] = None)
