package de.qaware.findfacts.core.solrimpl

import de.qaware.findfacts.common.dt.Kind
import de.qaware.findfacts.common.dt.solr.SolrSchema

/** Commonly used solr literals. */
object SolrQueryLiterals {

  /** Intersects two sub-queries or terms. */
  final val AND = "&&"

  /** Unions two sub-queries or terms.  */
  final val OR = "||"

  /** Inverts a query or term. */
  final val NOT = "!"

  /** Term to get all. */
  final val ALL = "*"

  /** Query to filter parent values. */
  final val QUERY_PARENT = s"${SolrSchema.DocKind}:${Kind.Block}"

  /** Query to get all values. */
  final val QUERY_ALL = s"$ALL:$ALL"
}
