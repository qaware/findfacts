package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.EtField

/** Union type for terms to filter a single field. */
sealed trait FilterTerm

/** Abstract data type for filters queries. */
sealed trait AbstractFQ

// Primitives
/** Filters by terms, seperated by whitespace.
 *
 * @param inner string
 */
final case class Term(inner: String) extends FilterTerm

/** Filters by exact terms.
 *
 * @param inner string
 */
final case class Exact(inner: String) extends FilterTerm

// Filter Terms
/** Matches range for numerical fields.
  *
  * @param from start value, inclusive
  * @param to max value, inclusive
  */
final case class InRange(from: Int, to: Int) extends FilterTerm

/** Matches if the field contains any value that from the result set of another query.
  *
  * @param fq filter query yielding ids to compare field to
  */
final case class AnyInResult(fq: AbstractFQ) extends FilterTerm

/** Matches if the field contains all values from the result set of another query.
  *
  * @param fq filter query yieldings ids to compare field to
  */
final case class AllInResult(fq: AbstractFQ) extends FilterTerm

// Filters
/** Concrete filter.
  *
  * @param fieldTerms a filter term per field
  */
final case class Filter(fieldTerms: Map[EtField, FilterTerm]) extends AbstractFQ

/** Intersection of multiple filters.
  *
  * @param f1 first
  * @param f2 second
  * @param fn rest
  */
final case class FilterIntersection(f1: AbstractFQ, f2: AbstractFQ, fn: AbstractFQ*) extends AbstractFQ

/** Union of multiple filters.
  *
  * @param f1 first
  * @param f2 second
  * @param fn rest
  */
final case class FilterUnion(f1: AbstractFQ, f2: AbstractFQ, fn: AbstractFQ*) extends AbstractFQ

/** Complement of a filter.
  *
  * @param filter to complement
  */
final case class FilterComplement(filter: AbstractFQ) extends AbstractFQ

// Queries
/** Query to facet on, i.e. list number of occurrences for each distinct value.
  *
  * @param filter to apply before facet
  * @param fields to facet on
  * @param maxFacets maximum number of facet values. Fields for which this value is exceeded will be ignored
  */
final case class FacetQuery(filter: AbstractFQ, fields: Set[EtField], maxFacets: Int = 100)

/** Query to filter for results.
  *
  * @param filter to apply
  * @param maxResults maximum number of results to stream back
  */
final case class FilterQuery(filter: AbstractFQ, maxResults: Int)
