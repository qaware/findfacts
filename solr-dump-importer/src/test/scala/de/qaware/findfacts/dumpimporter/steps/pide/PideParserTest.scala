package de.qaware.findfacts.dumpimporter.steps.pide

import de.qaware.findfacts.common.dt.DocKind
import org.scalatest.{FunSuite, Matchers}

class PideParserTest extends FunSuite with Matchers {

  test("single-term definition") {
    val tokens = List(
      TypeDelimToken,
      StringToken("'type'"),
      WhereToken,
      DefToken("name", 123),
      NameDelimToken,
      StringToken("'definition text'"))

    PideParser.constantDef(tokens) should equal(Right(PosToken(Code("'definition text'"), 35)))
  }

  test("multi-term definition") {
    val tokens = List(
      WhereToken,
      StringToken("'def1'"),
      DefDelimToken,
      DefToken("name", 123),
      NameDelimToken,
      StringToken("'def2'")
    )

    PideParser.constantDef(tokens) should equal(Right(PosToken(Code("'def1' | 'def2'"), 23)))
  }

  test("definition with comments and whitespace") {
    val tokens = List(
      WhitespaceToken(" "),
      CommentToken("comment 1", DocKind.Meta),
      CommentToken("comment 2", DocKind.Meta),
      WhitespaceToken(" "),
      WhereToken,
      WhitespaceToken(" "),
      CommentToken("comment 3", DocKind.Meta),
      StringToken("'def'")
    )

    PideParser.constantDef(tokens) should equal(Right(PosToken(Code("'def'"), 40)))
  }

  test("definition with for") {
    val tokens = List(
      ForToken,
      DefToken("A", 1),
      WhitespaceToken(" "),
      DefToken("B", 2),
      WhereToken,
      StringToken("'def'")
    )

    PideParser.constantDef(tokens) should equal(Right(PosToken(Code("'def'"), 16)))
  }

  test("fail on unknown") {
    val tokens = List(
      WhereToken,
      UnknownToken("[...]"),
      StringToken("'def'")
    )

    PideParser.constantDef(tokens) should matchPattern { case Left(PideParserError(_)) => }
  }

}
