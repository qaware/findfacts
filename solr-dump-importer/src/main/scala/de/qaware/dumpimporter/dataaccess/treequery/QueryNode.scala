package de.qaware.dumpimporter.dataaccess.treequery

/** Trait for final query nodes, to allow only execution of the query after certain query constructions in the DSL.
  *
  * @tparam N type of the nodes to query
  * @tparam R result type
  */
trait FinalNode[N <: Node[N], R] {

  /** Underlying query node. Not exposed so only methods of this trait can be used. */
  protected val query: QueryNode[N, R]

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
  override val query: QueryNode[N, R] = this
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
    if (resultNode.selected) {
      resultNode.inner +: resultNode.children.flatMap(extractSelectedNodes)
    } else {
      resultNode.children.flatMap(extractSelectedNodes)
    }
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
    * @return converted result
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
  def first(of: Connective): ChainNode[N, R] = First(this)

  /** Builds a node to search only in the top level of given tree.
    *
    * @param of connective for fluent DSL interface
    * @return new [[Root]]
    */
  def root(of: Connective): ChainNode[N, R] = Root(this)

  /** Builds a node to find the parents of results.
    *
    * @param of connective for fluent DSL interface
    * @return new [[Parent]]
    */
  def parent(of: Connective): ChainNode[N, R] = Parent(this)

  /** Builds a node to find all right siblings of results.
    *
    * @param of connective for fluent DSL interface
    * @return new [[Next]]
    */
  def next(of: Connective): ChainNode[N, R] = Next(this)

  /** Builds a node to find all left siblings of results.
    *
    * @param of connective for fluent DSL interface
    * @return new [[Prev]]
    */
  def previous(of: Connective): ChainNode[N, R] = Prev(this)

  /** Builds a node to find all that match the filter query.
    *
    * @param fq filter to match nodes
    * @return new [[Thats]]
    */
  def thats(fq: FilterQuery[N]): QueryNode[N, R] = Thats(this, fq)
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
/** Query node to get a single result. Throw [[IllegalArgumentException]] if result is no singleton.
  *
  * @tparam N type of the nodes to query
  */
final case class Single[N <: Node[N]]() extends QueryNode[N, N] with ChainNode[N, N] {
  override def convertResult(res: Seq[N]): N = {
    res match {
      case Seq(e) => e
      case _ => throw new IllegalArgumentException("Was not single")
    }
  }
}

/** Query node to get all results.
  *
  * @tparam N type of the nodes to query
  */
final case class All[N <: Node[N]]() extends QueryNode[N, Seq[N]] with ChainNode[N, Seq[N]] {
  override def convertResult(res: Seq[N]): Seq[N] = identity(res)
}

/** Query node to stop the recursive tree search after finding the first result.
  *
  * @param downstream query node to pass transformed results into
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class First[N <: Node[N], R](downstream: QueryNode[N, R]) extends UpstreamNode[N, R] with ChainNode[N, R] {
  override def transformRecursively(node: ResultNode[N]): ResultNode[N] = {
    if (node.selected) {
      // Cut off all children
      node.copy(children = Seq())
    } else {
      node.copy(children = node.children.map(transformRecursively))
    }
  }
}

/** Query node to find only root nodes of the forest. Stops the recursive search.
  *
  * @param downstream query node to pass transformed results into
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class Root[N <: Node[N], R](downstream: QueryNode[N, R]) extends UpstreamNode[N, R] with ChainNode[N, R] {
  override def executeQuery(forest: Seq[ResultNode[N]]): Seq[ResultNode[N]] = {
    // Cut off all non-root nodes
    val thisRes = forest.map(n => n.copy(children = Seq()))
    super.executeQuery(thisRes)
  }
}

/** Query node to find parents of nodes.
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

/** Query node to find right siblings of nodes.
  *
  * @param downstream query node to pass transformed results into
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class Next[N <: Node[N], R](downstream: QueryNode[N, R]) extends UpstreamNode[N, R] with ChainNode[N, R] {
  private def selectNext(siblings: Seq[ResultNode[N]]): Seq[ResultNode[N]] = {
    siblings.take(1).map(_.copy(selected = false)) ++ (siblings.drop(1).zip(siblings.dropRight(1)) map { cn =>
      cn._1.copy(selected = cn._2.selected)
    })
  }
  override def executeQuery(forest: Seq[ResultNode[N]]): Seq[ResultNode[N]] = {
    downstream.executeQuery(selectNext(forest.map(transformRecursively)))
  }
  override def transformRecursively(node: ResultNode[N]): ResultNode[N] = {
    // This does only work for children, so do it once more in [[executeQuery]]!
    node.copy(children = selectNext(node.children.map(transformRecursively)))
  }
}

/** Query node to find left siblings of nodes.
  *
  * @param downstream query node to pass transformed results into
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class Prev[N <: Node[N], R](downstream: QueryNode[N, R]) extends UpstreamNode[N, R] with ChainNode[N, R] {
  private def selectPrevious(siblings: Seq[ResultNode[N]]): Seq[ResultNode[N]] = {
    (siblings.dropRight(1).zip(siblings.drop(1)) map { cn =>
      cn._1.copy(selected = cn._2.selected)
    }) ++ siblings.takeRight(1).map(_.copy(selected = false))
  }
  override def executeQuery(forest: Seq[ResultNode[N]]): Seq[ResultNode[N]] = {
    downstream.executeQuery(selectPrevious(forest.map(transformRecursively)))
  }
  override def transformRecursively(node: ResultNode[N]): ResultNode[N] = {
    // This does (again) only work for children, so do it once more in [[executeQuery]]!
    node.copy(children = selectPrevious(node.children.map(transformRecursively)))
  }
}

/** Query node to find nodes according to a filter query.
  *
  * @param downstream query node to pass filtered results into
  * @param fq filter query to select nodes
  * @tparam N type of nodes to query
  * @tparam R result type
  */
final case class Thats[N <: Node[N], R](downstream: QueryNode[N, R], fq: FilterQuery[N]) extends UpstreamNode[N, R] {
  override def transformRecursively(node: ResultNode[N]): ResultNode[N] = {
    node.copy(selected = fq.matches(node.inner), children = node.children.map(transformRecursively))
  }
}
