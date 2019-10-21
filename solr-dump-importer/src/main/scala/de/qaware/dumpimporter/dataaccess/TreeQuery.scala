package de.qaware.dumpimporter.dataaccess

import scala.collection.mutable.ListBuffer

/** Generic immutable tree node.
  *
  * @tparam A type of inner element.
  */
trait Node[+A, T <: Node[A, T]] { self: T =>

  /** Inner element. */
  val data: A

  /** Children of this tree node. */
  val children: Seq[T]
}

/** Functional representation of a query to a tree consisting of [[Node]]s.
  *
  * @param filter function to check whether a node matches the query
  * @param nodeChildFilter partial function to filter which of the node's children are used in the recursive traversal
  */
case class TreeQuery[N <: Node[_, N]](
    filter: N => Boolean,
    nodeChildFilter: N => PartialFunction[N, Boolean] = (n: N) =>
      ({
        case c: Any if n.children.contains(c) => true
      }: PartialFunction[N, Boolean])) {

  /** Stops the recursion after the first match. Not that this does NOT mean that a query will only match one element!
    *
    * @return the new query
    */
  def first(): TreeQuery[N] = {
    TreeQuery[N](filter, n => if (filter(n)) nodeChildFilter(n).andThen(_ => false) else nodeChildFilter(n))
  }

  /** Finds the parent of matched elements.
    *
    * @return the new query
    */
  def parent(): TreeQuery[N] = {
    TreeQuery[N](node => node.children.filter(nodeChildFilter(node)).exists(filter), nodeChildFilter)
  }

  /** Returns elements that two queries match.
    *
    * @param query the other query
    * @return the new query
    */
  def and(query: TreeQuery[N]): TreeQuery[N] = {
    TreeQuery[N](
      n => filter(n) && query.filter(n),
      (n: N) => {
        val f1 = nodeChildFilter(n)
        val f2 = query.nodeChildFilter(n);
        { case n: N if f1.isDefinedAt(n) && f2.isDefinedAt(n) => f1(n) && f2(n) }
      }
    )
  }

  /** Execute the query on a given single tree.
    *
    * @ param tree to execute on
    * @return all matches
    */
  def find(tree: N): Iterable[N] = {
    find(Seq(tree))
  }

  /** Executes the query on a given forest.
    *
    * @param forest to execute on
    * @return all matches
    */
  def find(forest: Iterable[N]): Iterable[N] = {
    val result = ListBuffer[N]()
    findRec(forest, result)
    result
  }

  /** Finds a single element in the given tree. Throws [[IllegalArgumentException]] if result is no single argument.
    *
    * @param tree to execute on
    * @return single match
    */
  def findSingle(tree: N): N = find(Seq(tree)).toSeq match {
    case Seq(e) => e
    case res: Any =>
      throw new IllegalArgumentException(f"Result $res was no single element!")
  }

  @scala.annotation.tailrec
  private def findRec(forest: Iterable[N], results: ListBuffer[N]): Unit = {
    results.appendAll(forest.filter(filter))
    val children = forest.flatMap(t => t.children.filter(c => nodeChildFilter(t)(c)))
    if (children.nonEmpty) {
      findRec(children, results)
    }
  }
}
