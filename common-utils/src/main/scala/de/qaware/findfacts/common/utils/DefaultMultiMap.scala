package de.qaware.findfacts.common.utils

import scala.collection.mutable

/** Object for default multi-map constructor. */
object DefaultMultiMap {

  /** Creates an empty multi-map
    *
    * @tparam A key type
    * @tparam B value type
    * @return empty multimap
    */
  def empty[A, B]: mutable.MultiMap[A, B] = new mutable.HashMap[A, mutable.Set[B]] with mutable.MultiMap[A, B]
}
