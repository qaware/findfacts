package de.qaware.findfacts.common.utils

import scala.util.Success

import org.scalatest.{FunSuite, Matchers}

class FromStringTest extends FunSuite with Matchers {

  test("test int from string") {
    FromString[Int].apply("1") should equal(Success(1))
    FromString[Int].apply("one").isFailure should be(true)
  }

  test("test string from string") {
    FromString[String].apply("some string") should equal(Success("some string"))
  }
}
