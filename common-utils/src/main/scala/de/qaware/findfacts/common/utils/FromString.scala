package de.qaware.findfacts.common.utils

import scala.util.Try

/**
 * Generic trait for values that can be built from an idiomatic string.
 *
 * @tparam A value type
 */
trait FromString[A] {

  /**
   * Try to build value from string.
   *
   * @param str to build from
   * @return value or exception
   */
  def apply(str: String): Try[A]
}

/** Companion object, defining idiomatic typeclass methods and implicits. */
object FromString {

  /**
   * Materializer.
   *
   * @param f implicit to return
   * @tparam A value type to materialize FromString for
   * @return FromString materialized for type A
   */
  def apply[A](implicit f: FromString[A]): FromString[A] = f

  /**
   * Instance constructor.
   *
   * @param f function to create value from string
   * @tparam A type of the value
   * @return FromString for the value type
   */
  def instance[A](f: String => Try[A]): FromString[A] = (s: String) => f(s)

  /** FromString for integers. */
  implicit def intFromString: FromString[Int] = instance(s => Try(s.toInt))

  /** FromString for Strings. */
  implicit def stringFromString: FromString[String] = instance(Try(_))
}
