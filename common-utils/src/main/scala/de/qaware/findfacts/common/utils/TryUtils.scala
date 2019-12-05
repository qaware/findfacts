package de.qaware.findfacts.common.utils

import scala.language.implicitConversions
import scala.util.Try

/** Utils for seqs of trys. */
object TryUtils {

  /** Convert a sequence of trys to a try of a sequence, where the try contains the first failure.
    *
    * @param seq to convert
    * @tparam A type of underlying elements
    * @return try of sequence
    */
  implicit def tryFailFirst[A](seq: Seq[Try[A]]): Try[Seq[A]] = {
    Try { seq.map(_.get) }
  }

  /** Since [[tryFailFirst]] on deeper nested try/seqs cannot flatten, this implicit works one level higher and flattens
    * results.
    *
    * @param seq to convert and flatten
    * @tparam A type of underlying elements
    * @return try of sequence
    */
  implicit def tryFlattenFailFirst[A](seq: Seq[Try[Seq[A]]]): Try[Seq[A]] = {
    tryFailFirst(seq).map(_.flatten)
  }
}
