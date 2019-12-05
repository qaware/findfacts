package de.qaware.findfacts.common.utils

/** Utilities to work with Eithers. */
object EitherUtils {

  /** Maps a sequence of eithers to a either of sequence (as right type).
    *
    * @param single sequence of eithers
    * @tparam A left type
    * @tparam B right type
    * @return either of sequence
    */
  def eitherFailFirst[A, B](single: Seq[Either[A, B]]): Either[A, Seq[B]] = {
    single.foldLeft(Right(Nil): Either[A, Seq[B]]) { (acc, e) =>
      for (xs <- acc; x <- e) yield xs :+ x
    }
  }
}
