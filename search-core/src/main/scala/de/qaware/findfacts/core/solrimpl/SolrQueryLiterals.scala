package de.qaware.findfacts.core.solrimpl

/** Commonly used solr literals. */
object SolrQueryLiterals {

  /** Intersects two sub-queries or terms. */
  final val And = "&&"

  /** Unions two sub-queries or terms.  */
  final val Or = "||"

  /** Inverts a query or term. */
  final val Not = "!"

  /** Term to get all. */
  final val All = "*"

  /** Query to filter parent values. */
  final val QueryParent = "cmd_kind:*"

  /** Query to get all values. */
  final val QueryAll = "*:*"

  /** Tag local parameter. */
  final val TagParam = "tag"

  /** Tag for parent filters. */
  final val ParentTag = "top"
}
