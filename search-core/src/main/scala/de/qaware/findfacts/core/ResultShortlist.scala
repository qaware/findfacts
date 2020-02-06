package de.qaware.findfacts.core

import de.qaware.findfacts.common.dt.ShortCmdEt

/** Shortlist of results.
  *
  * @param values result values
  * @param count number of elements found
  * @param nextCursor for paging
  */
case class ResultShortlist(values: Vector[ShortCmdEt], count: Long, nextCursor: String)
