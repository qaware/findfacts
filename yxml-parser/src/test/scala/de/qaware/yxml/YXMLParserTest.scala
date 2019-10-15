package de.qaware.yxml

import better.files.Resource
import org.scalatest.{FunSuite, Inside, Matchers}

class YXMLParserTest extends FunSuite with Matchers {

  def parseShouldErrOn(yxml: String, errLine: Int, errCol: Int): Unit = {
    Inside.inside(YXMLParser(yxml)) {
      case Left(YXMLParserError(Location(line, col), _)) =>
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
    parseShouldErrOn("\u0005\u0006root\u0005\u0005\u0006\u0005.", 1, 11)
  }

  test("empty key-value pairs") {
    val yxml = "\u0005\u0006root node\u0005\u0005\u0006\u0005"
    val tree = Tree(Tag(Value("root node")))
    YXMLParser(yxml) should equal(Right(YXML(Seq(tree))))
  }

  test("header") {
    val yxml = "\u0005\u0006root node\u0006some tag 1=value with = \n\u0006tag2=val2\u0005\u0005\u0006\u0005"
    val tree = Tree(
      Tag(
        Value("root node"),
        Seq(KVPair(Value("some tag 1"), Value("value with = \n")), KVPair(Value("tag2"), Value("val2")))))
    YXMLParser(yxml) should equal(Right(YXML(Seq(tree))))
  }

  test("content") {
    val yxml = "\u0005\u0006root\u0005\tSome freetext message\r\n\tWith = \u0005\u0006\u0005"
    val tree = Tree(Tag(Value("root")), Seq(Body("Some freetext message\r\n\tWith =")))
    YXMLParser(yxml) should equal(Right(YXML(Seq(tree))))
  }

  test("tree structure") {
    val yxml =
      """\u0005\u0006root\u0005Some body\u0005\u0006child1\u0006child key=child value\u0005Child body\u0005\u0006\u0005Some other body\u0005\u0006child2\u0005\u0005\u0006\u0005\u0005\u0006\u0005""".stripMargin
    val child1 =
      Tree(Tag(Value("child1"), Seq(KVPair(Value("child key"), Value("child value")))), Seq(Body("Child body")))
    val child2 = Tree(Tag(Value("child2")))
    val tree = Tree(Tag(Value("root")), Seq(Body("Some body"), child1, Body("Some other body"), child2))
    YXMLParser(yxml) should equal(Right(YXML(Seq(tree))))
  }

  test("forest structure") {
    val yxml = "\n\u0005\u0006root1\u0005\u0005\u0006\u0005\n\u0005\u0006root2\u0005\u0005\u0006\u0005\n"
    YXMLParser(yxml) should equal(Right(YXML(Seq(Tree(Tag(Value("root1"))), Tree(Tag(Value("root2")))))))
  }

  test("parse larger example") {
    val yxml = Resource.getAsString("markup.yxml")
    YXMLParser(yxml) should matchPattern { case Right(_) => }
  }
}
