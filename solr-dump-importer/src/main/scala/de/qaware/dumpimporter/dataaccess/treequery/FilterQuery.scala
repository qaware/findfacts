package de.qaware.dumpimporter.dataaccess.treequery

/** Simple boolean query filters, evaluated on single nodes.
  *
  * @param matches filter function
  * @tparam N type of nodes
  */
final case class FilterQuery[N <: Node[N]](matches: N => Boolean) {

  /** Select everything matched by this and another filter.
    *
    * @param other filter to intersect
    * @return new filter for the 'and' intersection
    */
  def and(other: FilterQuery[N]): FilterQuery[N] = {
    FilterQuery[N]((n: N) => matches(n) && other.matches(n))
  }

  /** Select everything matched by this filter or another.
    *
    * @param other filter to union
    * @return new filter for the 'or' union
    */
  def or(other: FilterQuery[N]): FilterQuery[N] = {
    FilterQuery[N]((n: N) => matches(n) || other.matches(n))
  }

  /** Select everything matched by this filter, but not the other.
    *
    * @param other filter to take the difference
    * @return new filter matching this without other
    */
  def without(other: FilterQuery[N]): FilterQuery[N] = {
    FilterQuery[N]((n: N) => matches(n) && !other.matches(n))
  }
}
