package de.qaware.dumpimporter.dataaccess

import scala.collection.mutable.ListBuffer

/** Generic immutable tree node.
  *
  * @tparam A type of inner element.
  */
trait Node[A] {

  /** Inner element. */
  val data: A

  /** Children of this tree node. */
  val children: Seq[Node[A]]
}

/** Functional representation of a query to a tree consisting of [[Node]]s.
  *
  * @param filter function to check whether a node matches the query
  * @param nodeChildFilter partial function to filter which of the node's children are used in the recursive traversal
  * @tparam A element type
  */
case class TreeQuery[A](
    filter: Node[A] => Boolean,
    nodeChildFilter: Node[A] => PartialFunction[Node[A], Boolean] = (node: Node[A]) => {
      case child: Node[A] if node.children.contains(child) => true
    }: PartialFunction[Node[A], Boolean]) {

  /** Stops the recursion after the first match. Not that this does NOT mean that a query will only match one element!
    *
    * @return the new query
    */
  def first(): TreeQuery[A] = {
    TreeQuery[A](filter, n => if (filter(n)) nodeChildFilter(n).andThen(_ => false) else nodeChildFilter(n))
  }

  /** Finds the parent of matched elements.
    *
    * @return the new query
    */
  def parent(): TreeQuery[A] = {
    TreeQuery[A](node => node.children.filter(nodeChildFilter(node)).exists(filter), nodeChildFilter)
  }

  /** Returns elements that two queries match.
    *
    * @param query the other query
    * @return the new query
    */
  def and(query: TreeQuery[A]): TreeQuery[A] = {
    TreeQuery[A](
      n => filter(n) && query.filter(n),
      (n: Node[A]) => {
        val f1 = nodeChildFilter(n)
        val f2 = query.nodeChildFilter(n);
        { case n: Node[A] if f1.isDefinedAt(n) && f2.isDefinedAt(n) => f1(n) && f2(n) }
      }
    )
  }

  /** Executes the query on a given forest.
    *
    * @param forest to execute on
    * @param ev conversion method from nodes [[B]] of the forest to [[Node]][[A]]s
    * @tparam B nodes of the forest before conversion
    * @return all matches
    */
  def find[B](forest: Iterable[B])(implicit ev: B => Node[A]): Iterable[Node[A]] = {
    val result = ListBuffer[Node[A]]()
    findRec(forest.map(ev), result)
    result
  }

  @scala.annotation.tailrec
  private def findRec(forest: Iterable[Node[A]], results: ListBuffer[Node[A]]): Unit = {
    results.addAll(forest.filter(filter))
    val children = forest.flatMap(t => t.children.filter(c => nodeChildFilter(t)(c)))
    if (children.nonEmpty) {
      findRec(children, results)
    }
  }
}
