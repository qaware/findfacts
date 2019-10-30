package de.qaware.dumpimporter.steps

import better.files.{File, Resource}
import de.qaware.dumpimporter.Config
import org.scalatest.{FunSuite, Matchers}

class LoadThyExportStepTest extends FunSuite with Matchers {
  test("Import example export") {
    val exampleDump = File(Resource.getUrl("Example.thy").getFile).parent.parent.parent.parent.parent / "dump/example/"

    val context = StepContext.empty
    //new LoadThyExportStep(Config(exampleDump))(context)
    //context.consts should have size(1)
  }
}
