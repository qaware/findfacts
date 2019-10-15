package de.qaware.yxml

import better.files.Resource
import org.scalatest.{FunSuite, Inside, Matchers}

class YXMLLexerTest extends FunSuite with Matchers {
  test("tokenize empty string") {
    YXMLLexer("") should equal(Right(List()))
  }

  test("tokenize normal string") {
    val yxml = "\u0005\t\r\n Some text 123 .,;+ = Space after \u0006"
    YXMLLexer(yxml) should equal(
      Right(
        List(
          X(),
          WS("\t\r\n "),
          TEXT("Some text 123 .,;+"),
          WS(" "),
          EQUAL(),
          WS(" "),
          TEXT("Space after"),
          WS(" "),
          Y()
        )))
  }

  test("tokenize string with newline") {
    YXMLLexer("\n\u0005") should equal(Right(List(WS("\n"), X())))
  }

  test("tokenize unicode string") {
    YXMLLexer("abc\uD83D\uDE00\uD83D\uDCA9\uD83D\uDC6E\uD83C\uDFFF\u200D\u0006") should equal(
      Right(List(TEXT("abc\uD83D\uDE00\uD83D\uDCA9\uD83D\uDC6E\uD83C\uDFFF\u200D"), Y())))
  }

  test("tokenize larger example") {
    val yxml = Resource.getAsString("markup.yxml")
    Inside.inside(YXMLLexer(yxml)) {
      case Right(list) =>
        list.length should be > 1000
    }
  }
}
