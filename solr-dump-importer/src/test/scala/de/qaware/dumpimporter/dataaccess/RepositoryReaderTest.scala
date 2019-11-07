package de.qaware.dumpimporter.dataaccess

import better.files.{File, Resource}
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.LoneElement.convertToCollectionLoneElementWrapper

class RepositoryReaderTest extends FunSuite with Matchers {
  val reader = RepositoryReader(File(Resource.getUrl(".")))

  test("Get file") {
    val repoFiles = reader.readAll("testfile.*".r).toList
    repoFiles.loneElement should matchPattern { case RepositoryFile("/testfile/testfile.txt", _) => }
  }
  test("Filter") {
    reader.readAll("[a-z0-9]*".r).toList should be(empty)
  }
}
