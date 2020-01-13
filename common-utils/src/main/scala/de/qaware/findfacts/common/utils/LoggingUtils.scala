package de.qaware.findfacts.common.utils

import com.typesafe.scalalogging.Logger

/** Logging helper utils. */
object LoggingUtils {
  private val logger = Logger[LoggingUtils.type]

  /** Log on info level and return object, for fluent interface.
    *
    * @param elem to log and return
    * @tparam A type of the object
    * @return the given object
    */
  def doInfo[A](elem: A): A = {
    logger.info(elem.toString)
    elem
  }

  /** log on debug level and return object, for fluent interface.
    *
    * @param elem to log and retunr
    * @tparam A type of the object
    * @return the given object
    */
  def doDebug[A](elem: A): A = {
    logger.debug(elem.toString)
    elem
  }
}
