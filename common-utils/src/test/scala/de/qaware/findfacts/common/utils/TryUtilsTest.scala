package de.qaware.findfacts.common.utils

import scala.util.Try

import org.scalatest.{FunSuite, Matchers, TryValues}

class TryUtilsTest extends FunSuite with Matchers with TryValues {

  test("try succeeding") {
    TryUtils.tryFailFirst(Seq()).success.value should equal(Seq())
    TryUtils.tryFailFirst(Seq(Try(1))).success.value should equal(Seq(1))
    TryUtils.tryFailFirst(Seq(Try(1), Try(2))).success.value should equal(Seq(1, 2))
  }

  test("try fail first") {
    val list = Seq(
      Try(1),
      Try(throw new IllegalArgumentException("2")),
      Try(3),
      Try(throw new IllegalStateException("4")),
      Try(5))
    TryUtils.tryFailFirst(list).failure.exception.getMessage should equal("2")
  }

  test("Test fail first on try seq try seq") {
    import TryUtils._
    val seq: Seq[Try[Seq[Try[Int]]]] = Seq(Try(Seq(Try(1), Try(2))), Try(Seq(Try(3))))

    val resolved: Try[Seq[Int]] = seq
    resolved.success.value should equal(Seq(1, 2, 3))
  }
}
