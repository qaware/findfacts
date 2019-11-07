package de.qaware.yxml

import better.files.Resource
import org.scalatest.{FunSuite, Inside, Matchers}

class YxmlParserTest extends FunSuite with Matchers {

  def parseShouldErrOn(yxml: String, errLine: Int, errCol: Int): Unit = {
    Inside.inside(YxmlParser(yxml)) {
      case Left(YxmlParserError(Location(line, col), _)) =>
        line should be(errLine)
        col should be(errCol)
    }
  }

  test("fail on empty yxml") {
    parseShouldErrOn("", 0, 0)
  }

  test("fail on empty name") {
    parseShouldErrOn("\u0005\u0006\u0005\u0005\u0006\u0006", 1, 3)
  }

  test("fail on empty key") {
    parseShouldErrOn("\u0005\u0006name\u0006=value\u0005\u0005\u0006\u0005", 1, 7)
  }

  test("fail on invalid structure") {
    parseShouldErrOn("\u0005\u0006name\u0005\u0006\u0005", 1, 8)
  }

  test("fail on unparseable token") {
    parseShouldErrOn("\u0005\u0006root\u0005\u0005\u0006\u0005\u0005", 1, 11)
  }

  test("empty key-value pairs") {
    val yxml = "\u0005\u0006root node\u0005\u0005\u0006\u0005"
    val ast = Yxml(Seq(Markup("root node")))
    YxmlParser(yxml) should equal(Right(ast))
  }

  test("header") {
    val yxml = "\u0005\u0006root node\u0006some tag 1=value with = \n\u0006tag2=val2\u0005\u0005\u0006\u0005"
    val ast = Yxml(Seq(Markup("root node", Seq(("some tag 1", "value with = \n"), ("tag2", "val2")))))
    YxmlParser(yxml) should equal(Right(ast))
  }

  test("content") {
    val yxml = "\u0005\u0006root\u0005\tSome freetext message\r\n\tWith = \u0005\u0006\u0005"
    val ast = Yxml(Seq(Markup("root", inner=Yxml(Seq(Text("\tSome freetext message\r\n\tWith = "))))))
    YxmlParser(yxml) should equal(Right(ast))
  }

  test("ast structure") {
    val yxml =
      """\u0005\u0006root\u0005Some body\u0005\u0006child1\u0006child key=child value\u0005Child body\u0005\u0006\u0005Some other body\u0005\u0006child2\u0005\u0005\u0006\u0005\u0005\u0006\u0005""".stripMargin
    val child1 = Markup("child1", Seq(("child key", "child value")), Yxml(Seq(Text("Child body"))))
    val child2 = Markup("child2")
    val ast = Yxml(Seq(Markup("root", inner=Yxml(Seq(Text("Some body"), child1, Text("Some other body"), child2)))))
    YxmlParser(yxml) should equal(Right(ast))
  }

  test("forest structure") {
    val yxml = "\u0005\u0006root1\u0005\u0005\u0006\u0005\n\u0005\u0006root2\u0005\u0005\u0006\u0005\n"
    YxmlParser(yxml) should equal(Right(Yxml(Seq(Markup("root1"), Text("\n"), Markup("root2"), Text("\n")))))
  }

  test("parse larger example") {
    val yxml = Resource.getAsString("markup.yxml")
    val res = YxmlParser(yxml)
    res should matchPattern { case Right(_) => }
  }
}
