package de.qaware.dumpimporter.steps

import better.files.Resource
import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

case class TestSpec(mapping: TestSpecPosition, code: String)

class LoadPIDEMarkupStepTest extends FunSuite with Matchers {
  private final val SPEC_BEGIN = "(*SPEC:BEGIN:"

  test("Import example file") {
    var tests: Seq[TestSpec] = mutable.Seq.empty

    var thyFile = Resource.getAsString("Example.thy").linesIterator.toSeq.zipWithIndex
    while (thyFile.nonEmpty){
      val (remaining, test) = findTests(thyFile)
      thyFile = remaining
      test match {
        case Some(t) => tests :+= t
        case None =>
      }
    }

    // Create JIT-compiler
    val tb = currentMirror.mkToolBox()

    // Compile and run tests
    val testCommand = s"""new de.qaware.dumpimporter.steps.ImportStepITSpecExecutor({
                         |    case de.qaware.dumpimporter.steps.TestSpecContext(entity, begin, end) =>
                         |      new org.scalatest.FunSuite with org.scalatest.Matchers {
                         |        ${tests map { t => s"""test("PIDE Spec Test ${t.mapping.id}"){ ${t.code} }"""} mkString ("\n")}
                         |  }},
                         |  Seq(${tests.map(_.mapping.toFQNString).mkString(",")}),
                         |  new de.qaware.dumpimporter.steps.pide.LoadPIDEMarkupStep(
                         |    de.qaware.dumpimporter.Config(better.files.File("dump/example")))
                         |).execute()""".stripMargin
    tb.eval(tb.parse(testCommand))
  }

  private def findTests(elems: Seq[(String, Int)]): (Seq[(String, Int)], Option[TestSpec]) = {
    val begin = elems.dropWhile(!_._1.startsWith(SPEC_BEGIN))
    if (begin.isEmpty) return (Seq.empty, None)
    val id = begin.head._1.drop(SPEC_BEGIN.length).dropRight(2)
    val verify = begin.dropWhile(!_._1.startsWith(s"(*SPEC:$id:VERIFY"))
    val tests = verify.drop(1).takeWhile(!_._1.startsWith(s"SPEC:$id:END*)"))
    val spec = TestSpec(TestSpecPosition(id, begin.head._2+2,verify.head._2), tests.map(_._1).mkString("\n"))
    (begin.drop(1), Some(spec))
  }
}
/*val consts = context.consts map {e => (e.sourceFile, e.startPos, e.endPos, e.name, e.constType, e.kind)}
consts should have size 5
consts should contain ("Example.Example", 5, 7, "fun_const", "\"nat \\<Rightarrow> 'a\"", EntityKind.Constant.toString)
consts should contain ("Example.Example", 9, 10, "primrec_const", "\"nat \\<Rightarrow> 'a\"", EntityKind.Constant.toString)
consts should contain ("Example.Example", 12, 14, "function_const", null, EntityKind.Constant.toString)
consts should contain ("Example.Example", 17, 18, "abbreviation_const", "\"nat \\<Rightarrow> nat\"", EntityKind.Constant.toString)
consts should contain ("Example.Example", 22, 23, "definition_const", null, EntityKind.Constant.toString)*/
