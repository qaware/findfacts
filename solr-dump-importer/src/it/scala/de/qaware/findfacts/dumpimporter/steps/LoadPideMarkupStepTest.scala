package de.qaware.findfacts.dumpimporter.steps

import better.files.Resource
import org.scalatest.{DoNotDiscover, FunSuite, Matchers, Suite}

import scala.collection.{immutable, mutable}
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

case class TestSpec(mapping: TestSpecPosition, code: String)

@DoNotDiscover
class LoadPideMarkupStepTest extends FunSuite with Matchers {
  private final val SPEC_BEGIN = "(*SPEC:BEGIN:"

  override lazy val nestedSuites: immutable.IndexedSeq[Suite] = {
    var tests: Seq[TestSpec] = mutable.Seq.empty

    var thyFile = Resource.getAsString("Example.thy").linesIterator.toSeq.zipWithIndex
    while (thyFile.nonEmpty) {
      val (remaining, test) = findTests(thyFile)
      thyFile = remaining
      test match {
        case Some(t) => tests :+= t
        case None =>
      }
    }

    // Compile and run tests
    val testCommand =
      s"""new de.qaware.dumpimporter.steps.ImportStepITSpecExecutor({
       |    case de.qaware.dumpimporter.steps.TestSpecContext(entity, begin, end) =>
       |      new org.scalatest.FunSuite with org.scalatest.Matchers {
       |        ${tests map { t => s"""test("PIDE Spec Test ${t.mapping.id}") { ${t.code}
       |        }""".stripMargin} mkString ("\n")}
       |    }
       |  },
       |  Seq(${tests.map(_.mapping.toFQNString).mkString(",")}),
       |  new de.qaware.dumpimporter.steps.pide.LoadPIDEMarkupStep(
       |    de.qaware.dumpimporter.Config(better.files.File("dump/example")))
       |).buildSuite()""".stripMargin

    // Create JIT-compiler
    val tb = currentMirror.mkToolBox()
    val parsed = tb.parse(testCommand)
    val testSuite = tb.eval(parsed).asInstanceOf[Suite]
    immutable.IndexedSeq(testSuite)
  }

  private def findTests(elems: Seq[(String, Int)]): (Seq[(String, Int)], Option[TestSpec]) = {
    val begin = elems.dropWhile(!_._1.startsWith(SPEC_BEGIN))
    if (begin.isEmpty) return (Seq.empty, None)
    val id = begin.head._1.drop(SPEC_BEGIN.length).dropRight(2)
    val verify = begin.dropWhile(!_._1.startsWith(s"(*SPEC:$id:VERIFY"))
    val tests = verify.drop(1).takeWhile(!_._1.startsWith(s"SPEC:$id:END*)"))
    val spec = TestSpec(TestSpecPosition(id, begin.head._2 + 2, verify.head._2), tests.map(_._1).mkString("\n"))
    (begin.drop(1), Some(spec))
  }
}
