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
    seq.foldLeft(Try(Seq.empty[A]))((acc, e) => acc.flatMap(acc => e.map(acc :+ _)))
  }

  /** Does [[tryFailFirst]] and flattens another try.
    *
    * @param seq to convert
    * @tparam A type of underlying elements
    * @return try of sequence
    */
  implicit def flattenTryFailFirst[A](seq: Try[Seq[Try[A]]]): Try[Seq[A]] =
    seq.flatMap(tryFailFirst)

  /** Does [[tryFailFirst]] and flattens another seq.
    *
    * @param seq to convert
    * @tparam A type of underlying elements
    * @return try of sequence
    */
  implicit def flattenSeqFailFirst[A](seq: Seq[Try[Seq[A]]]): Try[Seq[A]] =
    tryFailFirst(seq).map(_.flatten)

  /** Same as [[tryFailFirst]], on one more level.
    *
    * @param seq to convert and flatten
    * @tparam A type of underlying elements
    * @return try of sequence
    */
  implicit def tryFailFirst2[A](seq: Seq[Try[Seq[Try[A]]]]): Try[Seq[A]] =
    tryFailFirst(seq).map(_.flatten).flatMap(tryFailFirst)
}
