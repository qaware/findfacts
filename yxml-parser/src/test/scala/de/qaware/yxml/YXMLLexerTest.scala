package de.qaware.yxml

import org.scalatest.{FunSuite, Matchers}

class YXMLLexerTest extends FunSuite with Matchers {
  test("tokenize empty string") {
    YXMLLexer("") should equal(Right(List()))
  }

  test("tokenize normal string") {
    val yxml = "\u0005\t\r\n Some text 123 .,;+ = No Space after \u0006"
    YXMLLexer(yxml) should equal(Right(List(X, TEXT("Some text 123 .,;+ "), EQUAL, TEXT("No Space after "), Y)))
  }

  test("tokenize unicode string") {
    YXMLLexer("abc\uD83D\uDE00\uD83D\uDCA9\uD83D\uDC6E\uD83C\uDFFF\u200D\u0006") should equal(
      Right(List(TEXT("abc\uD83D\uDE00\uD83D\uDCA9\uD83D\uDC6E\uD83C\uDFFF\u200D"), Y)))
  }
}
