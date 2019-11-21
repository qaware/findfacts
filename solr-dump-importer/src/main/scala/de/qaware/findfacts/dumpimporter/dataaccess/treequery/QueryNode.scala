package de.qaware.findfacts.dumpimporter.dataaccess.treequery

/** Encapsulates an error in the execution of a query.
  *
  * @param msg error message
  */
final case class QueryError(msg: String) extends Exception(msg)

/** Trait for final query nodes, to allow only execution of the query after certain query constructions in the DSL.
  *
  * @tparam N type of the nodes to query
  * @tparam R result type
  */
trait FinalNode[N <: Node[N], R] {

  /** Executes the query on a given tree.
    *
    * @param tree to execute query on
    * @return result converted to [[R]]
    */
  def in(tree: N): R = in(Seq(tree))

  /** Executes the query on a given forest.
    *
    * @param forest to execute query on
    * @return result converted to [[R]]
    */
  def in(forest: Seq[N]): R
}

/** Trait for all query nodes. A query node can be executed on a result tree (a tree where it is marked whether nodes
  *  are selected by the query or not) and transforms that tree recursively.
  *  All query nodes are final, execution can be started from each.
  *
  * @tparam N type of the nodes to query
  * @tparam R result type
  */
sealed trait QueryNode[N <: Node[N], R] extends FinalNode[N, R] {
  override def in(forest: Seq[N]): R = {
    val resultForest = executeQuery(forest.map(ResultNode.fromTree))
    convertResult(resultForest.flatMap(extractSelectedNodes))
  }

  /** Extracts selected nodes recursively out of the result node.
    *
    * @param resultNode to extract selected nodes from
    * @return sequence of selected nodes
    */
  def extractSelectedNodes(resultNode: ResultNode[N]): Seq[N] = {
    if (resultNode.selected)
      resultNode.inner +: resultNode.children.flatMap(extractSelectedNodes)
    else
      resultNode.children.flatMap(extractSelectedNodes)
  }

  /** Invokes recursive execution of the whole query, on a forest of result nodes.
    *
    * @param forest to execute query on, already transformed from sequence of [[N]]s
    * @return results for each tree
    */
  def executeQuery(forest: Seq[ResultNode[N]]): Seq[ResultNode[N]] = forest.map(transformRecursively)

  /** Recursively transforms a result tree. Should only alter the 'selected' parameters, not the tree structure itself,
    * unless all children of a node become unselected (then pruning is ok, as currently no implementors could re-select
    * those nodes).
    *
    * @param tree to transform
    * @return transformed tree. Default implementation is no-op (identity).
    */
  def transformRecursively(tree: ResultNode[N]): ResultNode[N] = identity(tree)

  /** Converts a query result to the parametrized type.
    *
    * @param res result nodes to convert
    * @return attempt to convert the result
    */
  def convertResult(res: Seq[N]): R
}

/** Mixin trait for nodes that can be chained, for fluent DSL interface.
  *
  * @tparam N type of nodes to query
  * @tparam R result type
  */
sealed trait ChainNode[N <: Node[N], R] extends QueryNode[N, R] {

  /** Builds a node to stop searching in a subtree after finding the first result.
    *
    * @param of connective for fluent DSL interface
    * @return new [[First]]
    */
  def first(of: Connective): First[N, R] = First(this)

  /** Builds a node to stop the whole search after finding the first result
    *
    * @param of connective for fluent DSL interface
    * @return new [[FirstInOrder]]
    */
  def firstInOrder(of: Connective): FirstInOrder[N, R] = FirstInOrder(this)

  /** Builds a node to search only in the top level of given tree.
    *
    * @param of connective for fluent DSL interface
    * @return new [[Root]]
    */
  def root(of: Connective): Root[N, R] = Root(this)

  /** Builds a node to find the parents of results.
    *
    * @param of connective for fluent DSL interface
    * @return new [[Parent]]
    */
  def parent(of: Connective): Parent[N, R] = Parent(this)

  /** Builds a node to find all right siblings of results.
    *
    * @param of connective for fluent DSL interface
    * @return new [[Next]]
    */
  def next(of: Connective): Next[N, R] = Next(this)

  /** Builds a node to find all on the right to a result.
    *
    * @param of connective for fluent DSL interface
    * @return new [[AnyNext]]
    */
  def anyNext(of: Connective): AnyNext[N, R] = AnyNext(this)

  /** Builds a node to find all left siblings of results.
    *
    * @param of connective for fluent DSL interface
    * @return new [[Prev]]
    */
  def previous(of: Connective): Prev[N, R] = Prev(this)

  /** Builds a node to find all left to a result.
    *
    * @param of connective for fluent DSL interface
    * @return new [[AnyPrev]]
    */
  def anyPrevious(of: Connective): AnyPrev[N, R] = AnyPrev(this)

  /** Builds a node to find all that match the filter query.
    *
    * @param fq filter to match nodes
    * @return new [[Thats]]
    */
  def thats(fq: FilterQuery[N]): Thats[N, R] = Thats(this, fq)

  /** Builds a node to remove all that match the filter query from the search.
    *
    * @param fq to match all to remove
    * @return new [[Without]]
    */
  def without(fq: FilterQuery[N]): Without[N, R] = Without(this, fq)
}

/** Mixing trait for nodes that are upstream of others.
  *
  * @tparam N type of nodes to query
  * @tparam R result type
  */
sealed trait UpstreamNode[N <: Node[N], R] extends QueryNode[N, R] {

  /** Query Node that is downstream of this node, i.e. transformations are executed first on this,
    * then fed into downstream.
    */
  protected val downstream: QueryNode[N, R]

  override def executeQuery(forest: Seq[ResultNode[N]]): Seq[ResultNode[N]] = {
    // First execute own transformations, then feed result into downstream
    downstream.executeQuery(forest.map(transformRecursively))
  }
  override def convertResult(res: Seq[N]): R = downstream.convertResult(res)
}

// Implementors
/** Query-node to get a single result. Returns [[QueryError]] if result is no singleton.
  *
  * @tparam N type of the nodes to query
  */
final case class Single[N <: Node[N]]()
    extends QueryNode[N, Either[QueryError, N]]
    with ChainNode[N, Either[QueryError, N]] {
  override def convertResult(res: Seq[N]): Either[QueryError, N] = res match {
    case Seq(e) => Right(e)
    case _ => Left(QueryError(s"Was not single: $res"))
  }
}

/** Query-node to get all results.
  *
  * @tparam N type of the nodes to query
  */
final case class All[N <: Node[N]]() extends QueryNode[N, Seq[N]] with ChainNode[N, Seq[N]] {
  override def convertResult(res: Seq[N]): Seq[N] = identity(res)
}

/** Query-node to stop the recursive tree search after finding the first result.
  *
  * @param downstream query node to pass transformed results into
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class First[N <: Node[N], R](downstream: QueryNode[N, R]) extends UpstreamNode[N, R] with ChainNode[N, R] {
  override def transformRecursively(node: ResultNode[N]): ResultNode[N] = {
    if (node.selected) {
      // Cut off all children
      node.copy(children = Seq.empty)
    } else
      node.copy(children = node.children.map(transformRecursively))
  }
}

/** Query-node to stop all search after finding the first results.
  *
  * @param downstream query node to pass transformed results into
  * @param stop flag to indicate whether query node has stopped
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class FirstInOrder[N <: Node[N], R](downstream: ChainNode[N, R], var stop: Boolean = false)
    extends UpstreamNode[N, R]
    with ChainNode[N, R] {
  override def transformRecursively(node: ResultNode[N]): ResultNode[N] = {
    if (stop)
      node.copy(selected = false, children = Seq.empty)
    else if (node.selected) {
      stop = true
      node.copy(children = Seq.empty)
    } else
      node.copy(children = node.children map transformRecursively)
  }
}

/** Query-node to find only root nodes of the forest. Stops the recursive search.
  *
  * @param downstream query node to pass transformed results into
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class Root[N <: Node[N], R](downstream: QueryNode[N, R]) extends UpstreamNode[N, R] with ChainNode[N, R] {
  override def executeQuery(forest: Seq[ResultNode[N]]): Seq[ResultNode[N]] = {
    // Cut off all non-root nodes
    val thisRes = forest.map(_.copy(children = Seq.empty))
    super.executeQuery(thisRes)
  }
}

/** Query-node to find parents of nodes.
  *
  * @param downstream query node to pass transformed results into
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class Parent[N <: Node[N], R](downstream: QueryNode[N, R]) extends UpstreamNode[N, R] with ChainNode[N, R] {
  override def transformRecursively(node: ResultNode[N]): ResultNode[N] = {
    node.copy(selected = node.children.exists(_.selected), children = node.children.map(transformRecursively))
  }
}

/** Base class for sibling transformation upstream query-nodes.
  *
  * @tparam N type of nodes to query
  * @tparam R result type
  */
sealed trait SiblingTransformationNode[N <: Node[N], R] extends UpstreamNode[N, R] {

  /** Function to describe how to transform siblings.
    *
    * @param siblings to transform
    * @return transformed siblings
    */
  protected def transformSiblings(siblings: IndexedSeq[ResultNode[N]]): Seq[ResultNode[N]]
  override def executeQuery(forest: Seq[ResultNode[N]]): Seq[ResultNode[N]] = {
    downstream.executeQuery(transformSiblings(forest.map(transformRecursively).toIndexedSeq))
  }
  override def transformRecursively(node: ResultNode[N]): ResultNode[N] = {
    // This does only work for children, so do it once more in [[executeQuery]]!
    node.copy(children = transformSiblings(node.children.map(transformRecursively).toIndexedSeq))
  }
}

/** Query-node to find right siblings of selected nodes.
  *
  * @param downstream query node to pass transformed results into
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class Next[N <: Node[N], R](downstream: QueryNode[N, R])
    extends SiblingTransformationNode[N, R]
    with ChainNode[N, R] {
  protected override def transformSiblings(siblings: IndexedSeq[ResultNode[N]]): Seq[ResultNode[N]] = {
    siblings.take(1).map(_.copy(selected = false)) ++ (siblings.drop(1).zip(siblings.dropRight(1)) map { cn =>
      cn._1.copy(selected = cn._2.selected)
    })
  }
}

/** Query-node to find all that are right to a selected node.
  *
  * @param downstream query node to pass transformed results into
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class AnyNext[N <: Node[N], R](downstream: QueryNode[N, R])
    extends SiblingTransformationNode[N, R]
    with ChainNode[N, R] {
  override protected def transformSiblings(siblings: IndexedSeq[ResultNode[N]]): Seq[ResultNode[N]] = {
    siblings.indexWhere(_.selected) match {
      case -1 => siblings
      case firstIdx: Any =>
        siblings.take(firstIdx) ++ (siblings(firstIdx).copy(selected = false)
          +: siblings.drop(firstIdx + 1).map(_.copy(selected = true)))
    }
  }
}

/** Query-node to find left siblings of selected nodes.
  *
  * @param downstream query node to pass transformed results into
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class Prev[N <: Node[N], R](downstream: QueryNode[N, R])
    extends SiblingTransformationNode[N, R]
    with ChainNode[N, R] {
  override protected def transformSiblings(siblings: IndexedSeq[ResultNode[N]]): Seq[ResultNode[N]] = {
    (siblings.dropRight(1).zip(siblings.drop(1)).map(cn => cn._1.copy(selected = cn._2.selected))
      ++ siblings.takeRight(1).map(_.copy(selected = false)))
  }
}

/** Query-node to find all that are left to a selected node.
  *
  * @param downstream query nodes to pass transformed results into
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class AnyPrev[N <: Node[N], R](downstream: QueryNode[N, R])
    extends SiblingTransformationNode[N, R]
    with ChainNode[N, R] {
  override protected def transformSiblings(siblings: IndexedSeq[ResultNode[N]]): Seq[ResultNode[N]] = {
    siblings.lastIndexWhere(_.selected) match {
      case -1 => siblings
      case lastIdx: Any =>
        ((siblings.take(lastIdx).map(_.copy(selected = true)) :+ siblings(lastIdx).copy(selected = false))
          ++ siblings.drop(lastIdx + 1))
    }
  }
}

/** Query-node to find nodes according to a filter query.
  *
  * @param downstream query node to pass filtered results into
  * @param fq filter query to select nodes
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class Thats[N <: Node[N], R](downstream: QueryNode[N, R], fq: FilterQuery[N])
    extends UpstreamNode[N, R]
    with ChainNode[N, R] {
  override def transformRecursively(node: ResultNode[N]): ResultNode[N] = {
    node.copy(selected = node.selected && fq.matches(node.inner), children = node.children.map(transformRecursively))
  }
}

/** Query-node to filter out complete sub-trees where the root node was selected before
  * and is selected by the filter query.
  *
  * @param downstream query node pass filtered results into
  * @param fq query to filter out nodes
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class Without[N <: Node[N], R](downstream: QueryNode[N, R], fq: FilterQuery[N])
    extends UpstreamNode[N, R]
    with ChainNode[N, R] {
  override def transformRecursively(node: ResultNode[N]): ResultNode[N] = {
    if (node.selected && fq.matches(node.inner))
      node.copy(selected = false, children = Seq.empty)
    else
      node.copy(children = node.children.map(transformRecursively))
  }
}
