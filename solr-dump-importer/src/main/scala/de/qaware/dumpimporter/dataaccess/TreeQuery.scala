package de.qaware.dumpimporter.dataaccess

import scala.collection.mutable.ListBuffer
import scala.language.{higherKinds, implicitConversions}

import de.qaware.dumpimporter.dataaccess
import org.apache.commons.lang3.NotImplementedException

trait Reducible[+A, M[_] <: Reducible[_, M]] extends Any {
  def apply[B](elems: Iterable[M[B]]): M[B]
  def flatMap[B](f: A => M[B]): M[B]
}
class IterableReducible[A](val inner: Iterable[A]) extends AnyVal with Reducible[A, IterableReducible] {
  override def apply[B](elems: Iterable[IterableReducible[B]]): IterableReducible[B] = new IterableReducible(elems).flatMap(identity)
  override def flatMap[B](f: A => IterableReducible[B]): IterableReducible[B] = new IterableReducible(inner.flatMap(e => f(e).inner))
}
object IterableReducible {
  implicit def toInner[A](outer: IterableReducible[A]): Iterable[A] = outer.inner
}
class SingletonReducible[A](val inner: A) extends AnyVal with Reducible[A, SingletonReducible] {
  override def apply[B](elems: Iterable[SingletonReducible[B]]): SingletonReducible[B] = elems.toSeq match {
    case Seq(singleton) => singleton
    case iterable => throw new IllegalArgumentException(s"Tried to build singleton reducible on non-singleton iterable $iterable")
  }
  override def flatMap[B](f: A => SingletonReducible[B]): SingletonReducible[B] = f(inner)
}
object SingletonReducible {
  implicit def toInner[A](outer: SingletonReducible[A]): A = outer.inner
}

/** Generic immutable tree node.
  *
  * @tparam A type of inner element
  * @tparam T type of whole node
  */
trait Node[+A, T <: Node[A, T]] {
  /** Inner element. */
  val data: A

  /** Children of this tree node. */
  val children: Seq[T]
}

/** Value class for the lengthier node to partial selection function. */
//class ChildFilter[N <: Node[_, N]](val inner: N => PartialFunction[N, Boolean]) extends AnyVal
sealed trait Connective
case object of extends Connective
case object ofOne extends Connective

final case class FilterQuery[N <: Node[_, N]](result: N => Boolean, recursion: N => N => Boolean = (_: N) => (_: N) => true) {
  def and(other: FilterQuery[N]): FilterQuery[N] = {
    FilterQuery((n: N) => result(n) && other.result(n), (n: N) => (c: N) => recursion(n)(c) && other.recursion(n)(c))
  }
  def without(other: FilterQuery[N]): FilterQuery[N] = {
    FilterQuery((n: N) => result(n) && !other.result(n), recursion)
  }
}
object FilterQuery {
  def not[N <: Node[_, N]](fq: FilterQuery[N]): FilterQuery[N] = {
    FilterQuery(!fq.result(_))
  }
  def property[N <: Node[_, N]](filter: N => Boolean): FilterQuery[N] = {
    FilterQuery(filter)
  }
}

final case class ResultTransformQuery[N <: Node[_, N], R[_] <: Reducible[_, R]](transform: N => N => Iterable[N])(implicit ev: R[N]) {
  def in(forest: Iterable[N]): R[N] = { // TODO root node
    val resultIterable = for {
      node <- forest
      child <- node.children
    } yield transform(node)(child)
    ev(resultIterable)
  }

  def in(tree: N): R[N] = in(Seq(tree))

  def thats(filter: FilterQuery[N]): ResultTransformQuery[N, R] = {
    ResultTransformQuery { n: N => c: N =>
      if (filter.recursion(n)(c)) {
        transform(n)(c).filter(filter.result)
      } else {
        Seq()
      }
    }
  }

  def first(of: Connective): ResultTransformQuery[N, R] = ???
  def root(of: Connective): ResultTransformQuery[N, R] = ???
  def previous(of: Connective): ResultTransformQuery[N, R] = ???
  def next(of: Connective): ResultTransformQuery[N, R] = ???
  def parent(of: Connective): ResultTransformQuery[N, R] = ???
}
object ResultTransformQuery {
  def findRec[N <: Node[_, N]](node: N, child: N): Iterable[N] = {
    child.children.flatMap(grandChild => findRec(child, grandChild))
  }
  private def getQuery[N <: Node[_, N], R[_] <: Reducible[_, R]](implicit ev: R[N]) = {
    ResultTransformQuery((node: N) => (child: N) => findRec(node, child))(ev)
  }
  def single[N <: Node[_, N]](implicit ev: N): ResultTransformQuery[N, SingletonReducible] = {
    getQuery(new SingletonReducible[N](ev))
  }
  def all[N <: Node[_, N]](implicit ev: N): ResultTransformQuery[N, IterableReducible] = {
    getQuery(new IterableReducible[N](Seq(ev)))
  }
}

/** Functional representation of a query to a tree consisting of [[Node]]s.
  *
  * @param filter function to check whether a node matches the query
  * @param nodeChildFilter partial function to filter which of the node's children are used in the recursive traversal
  * @tparam N type of node
  */
final case class TreeQuery[N <: Node[_, N]](
    filter: N => Boolean,
    nodeChildFilter: N => PartialFunction[N, Boolean] = (n: N) =>
      ({
        case c: N if n.children.contains(c) => true
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
    * @param tree to execute on
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
