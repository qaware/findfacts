package de.qaware.findfacts.common.utils

import scala.collection.AbstractIterator

import com.typesafe.scalalogging.Logger

/** Collection interceptor to log progress. */
object ProgressLogger {
  private val logger = Logger[ProgressLogger.type]

  /** Logs progress when the collection is iterated. Exploits lazyness of collection iteration.
    *
    * @param coll iterable to wrap
    * @param granularity of the logging, in #steps
    * @tparam A type of the collection elements
    * @return an iterable that
    */
  def withProgress[A](coll: Iterable[A], granularity: Int = 20): Iterator[A] = new AbstractIterator[A] {
    private val total = coll.size
    private var idx = 0
    private val it = coll.iterator

    override def hasNext: Boolean = it.hasNext

    override def next(): A = {
      if (idx > 0 && idx % (total / granularity) == 0) {
        logger.info(s"Finished ${idx}/${total}")
      }
      idx += 1
      it.next()
    }
  }
}
