package de.qaware.dumpimporter.dataacess

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
  * @param children for the recursive call of the tree traversal
  * @tparam A element type
  */
case class Query[A](filter: Node[A] => Boolean, children: Node[A] => Seq[Node[A]]) {

  /** Stops the recursion after the first match. Not that this does NOT mean that a query will only match one element!
    *
    * @return the new query
    */
  def first(): Query[A] = Query[A](filter, n => if (filter(n)) Seq.empty else n.children)

  /** Finds the parent of matched elements.
    *
    * @return the new query
    */
  def parent(): Query[A] = Query[A](node => children(node).exists(filter), children)

  /** Returns elements that two queries match.
    *
    * @param query the other query
    * @return the new query
    */
  def and(query: Query[A]): Query[A] =
    Query[A](n => filter(n) && query.filter(n), n => children(n).intersect(query.children(n)))

  /** Returns elements matched by either query.
    *
    * @param query the other query
    * @return the new query
    */
  def or(query: Query[A]): Query[A] =
    Query(n => filter(n) || query.filter(n), n => children(n).concat(query.children(n)))

  /** Executes the query on a given forest.
    *
    * @param forest to execute on
    * @param ev conversion method from nodes [[B]] of the forest to [[Node]][[A]]s
    * @tparam B nodes of the forest before conversion
    * @return all matches
    */
  def execute[B](forest: Iterable[B])(implicit ev: B => Node[A]): Iterable[Node[A]] = TreeWalker.find(forest, this)(ev)
}

/** Looks for elements in yxml forests. */
object TreeWalker {

  /** Find elements.
    *
    * @param forest to search
    * @param query specifying what to search for
    * @param ev implicit conversion to [[Node]]s
    * @tparam A type of nodes
    * @tparam B type of convertible
    * @return nodes that match
    */
  def find[A, B](forest: Iterable[B], query: Query[A])(implicit ev: B => Node[A]): Iterable[Node[A]] = {
    val result = ListBuffer[Node[A]]()
    findRec(forest.map(ev), query, result)
    result
  }

  @scala.annotation.tailrec
  private def findRec[A](forest: Iterable[Node[A]], query: Query[A], results: ListBuffer[Node[A]]): Unit = {
    results.addAll(forest.filter(query.filter))
    val children = forest.flatMap(c => query.children(c))
    if (children.nonEmpty) {
      findRec(children, query, results)
    }
  }
}
