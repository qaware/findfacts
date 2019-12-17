package de.qaware.findfacts.dumpimporter.dataaccess.treequery

/** Connective for the query DSL. */
sealed trait Connective

/** Encapsulates query DSL types and constructors. */
object QueryDsl {

  /** Connective 'of'. */
  case object of extends Connective // scalastyle:ignore

  /** Connective 'of one'. */
  case object ofOne extends Connective // scalastyle:ignore

  /** Builds a query node to find a single element, for fluent DSL interface.
    * Returns an error if the query finds more than one result.
    *
    * @tparam N type of the nodes to query
    * @return an initial query node
    */
  def single[N <: Node[N]]: ChainNode[N, Either[QueryException, N]] = Single()

  /** Builds a query node to find all elements.
    *
    * @tparam N type of the nodes to query
    * @return an initial query node
    */
  def all[N <: Node[N]]: ChainNode[N, Seq[N]] = All()

  /** Select everything not matched by this filter.
    *
    * @param fq filter to complement
    * @tparam N type of the nodes
    * @return new filter matching the complement of fq
    */
  def not[N <: Node[N]](fq: FilterQuery[N]): FilterQuery[N] = FilterQuery[N](!fq.matches(_))

  /** Directly filter by a function.
    *
    * @param filter function to use
    * @tparam N type of the nodes
    * @return new filter wrapper object
    */
  def property[N <: Node[N]](filter: N => Boolean): FilterQuery[N] = FilterQuery[N](filter)
}

/** Generic immutable tree node.
  *
  * @tparam T type of whole node
  */
trait Node[T <: Node[T]] {

  /** Children of this tree node. */
  val children: Seq[T]
}

/** Wrapper class for nodes and flags to indicate whether they have been selected by the query.
  *
  * @param inner underlying node
  * @param children of the underlying node, wrapped
  * @param selected flag to indicate whether node has been selected by the query
  * @tparam N type of the underlying node
  */
final case class ResultNode[N <: Node[N]](inner: N, children: Seq[ResultNode[N]], selected: Boolean)

/** Companion object to build result nodes. */
object ResultNode {

  /** Builds a result node from a node [[N]], initially with all nodes selected.
    *
    * @param tree to wrap
    * @tparam N type of node to wrap
    * @return wrapped result node
    */
  def fromTree[N <: Node[N]](tree: N): ResultNode[N] =
    ResultNode(tree, tree.children.map(fromTree), selected = true)
}
