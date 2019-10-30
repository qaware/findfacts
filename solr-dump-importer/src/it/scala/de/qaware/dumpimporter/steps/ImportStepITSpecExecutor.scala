package de.qaware.dumpimporter.steps

import de.qaware.common.solr.dt.ConstEntity
import org.scalatest.{FunSuite, Matchers}

case class TestSpecPosition(id: String, startLine: Int, endLine: Int) {
  def toFQNString: String = s"""de.qaware.dumpimporter.steps.TestSpecPosition("${id}", ${startLine}, ${endLine})"""
}
case class TestSpecContext(
    entityById: Map[String, ConstEntity],
    startLineById: Map[String, Int],
    endLineById: Map[String, Int])

class ImportStepITSpecExecutor(
    val tests: TestSpecContext => FunSuite,
    val specPositions: Seq[TestSpecPosition],
    step: ImportStep)
    extends FunSuite
    with Matchers {
  test("Runner") {
    // Results are stored in contexts
    val context = StepContext.empty
    // Execute step on empty context
    step(context)

    val entityByPos = context.consts.groupBy { const =>
      (const.startPos, const.endPos)
    }

    // Assert that there is a match between tests and entities in the test-theory

    entityByPos.keySet should contain allElementsOf (specPositions map { e =>
      (e.startLine, e.endLine)
    })

    val entityById = (specPositions map { e =>
      (e.id, entityByPos((e.startLine, e.endLine)).head)
    }
      groupBy (_._1)
      mapValues (_.head._2))

    val byId = specPositions.groupBy(_.id)
    val startById = byId.mapValues(_.head.startLine)
    val endById = byId.mapValues(_.head.endLine)

    tests(TestSpecContext(entityById, startById, endById)).execute(stats = true)
  }
}
