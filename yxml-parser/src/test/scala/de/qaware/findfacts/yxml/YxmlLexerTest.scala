package de.qaware.findfacts.yxml

import better.files.Resource
import org.scalatest.{FunSuite, Inside, Matchers}

class YxmlLexerTest extends FunSuite with Matchers {
  test("tokenize empty string") {
    YxmlLexer("") should equal(Right(List()))
  }

  test("tokenize normal string") {
    val yxml = "\u0005\t\r\n Some text 123 .,;+ = Space after \u0006"
    YxmlLexer(yxml) should equal(
      Right(
        List(
          X,
          new TS("\t\r\n Some text 123 .,;+ "),
          EQ,
          new TS(" Space after "),
          Y
        )))
  }

  test("tokenize string with newline") {
    YxmlLexer("\n\u0005") should equal(Right(List(new TS("\n"), X)))
  }

  test("tokenize unicode string") {
    YxmlLexer("abc\uD83D\uDE00\uD83D\uDCA9\uD83D\uDC6E\uD83C\uDFFF\u200D\u0006") should equal(
      Right(List(new TS("abc\uD83D\uDE00\uD83D\uDCA9\uD83D\uDC6E\uD83C\uDFFF\u200D"), Y)))
  }

  test("tokenize larger example") {
    val yxml = Resource.getAsString("markup.yxml")
    Inside.inside(YxmlLexer(yxml)) {
      case Right(list) =>
        list.length should be > 1000
    }
  }
}
