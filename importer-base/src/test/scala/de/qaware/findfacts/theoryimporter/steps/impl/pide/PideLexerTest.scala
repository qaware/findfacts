package de.qaware.findfacts.theoryimporter.steps.impl.pide

import de.qaware.findfacts.yxml.{Markup, Text, Yxml}
import org.scalatest.{FunSuite, Matchers}

class PideLexerTest extends FunSuite with Matchers {
  test("Definition wrapped in xml_elem") {

    val body = new Yxml(Seq(new Text("entity name")))
    val xmlElem = new Yxml(Seq(Markup("xml_elem", inner = body)))
    val entity = new Yxml(Seq(Markup("entity", Seq(("def", "2"), ("name", "other name")), xmlElem)))
    val parent = new Yxml(Seq(Markup("entity", Seq(("def", "1"), ("name", "some name")), entity)))
    PideLexer(parent) should equal(Right(List(DefToken("entity name", 2))))
  }
}
