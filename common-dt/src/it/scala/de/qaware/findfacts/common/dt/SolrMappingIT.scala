package de.qaware.findfacts.common.dt

import scala.jdk.CollectionConverters._

import de.qaware.findfacts.common.solr.LocalSolr
import de.qaware.findfacts.common.solr.mapper.{FromSolrDoc, ToSolrDoc}
import org.apache.solr.client.solrj.SolrQuery
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class SolrMappingIT extends AnyFunSuite with Matchers with BeforeAndAfterEach with BeforeAndAfterAll {

  final val solr = ITSolr()

  override def beforeAll(): Unit = {
    solr.createIndex(LocalSolr.DEFAULT_CORE_NAME)
  }

  override def beforeEach(): Unit = {
    solr.deleteByQuery("*:*")
    val status = solr.commit()
    status.getStatus should (be(200) or be(0))
  }

  override def afterAll(): Unit = solr.close()

  case class Base1()
  case class Base2()

  test("Writing to and reading from solr") {
    val const = ConstantEt("Const.file.const", "const", List("Hol.equiv", "Hol.inJ", "Nat"), "'a => Nat")
    val fact = FactEt("Fact.file.const_is_pos", "const_is_pos", List("Const.const", "Fact.Hol.Stuff", "Fact.Hol.other"))
    val typ = TypeEt("Type.fil1.Nat", "Nat", List("Nat"))

    val doc = CodeblockEt("file.5.7", "file", 2, "(*", "\n", "(* comment *)", "\n\n lemma...", List.empty)
    val block = CodeblockEt("file.8.11", "file", 6, "fun", "*)\n", "fun ...src text...", "\n\n", List(const, fact))
    val block1 = CodeblockEt("file1.0.6", "file1", 1, "other", "\n", "other src text", "\n", List(typ))

    val toMapper = ToSolrDoc[BaseEt]
    val fromMapper = FromSolrDoc[BaseEt]
    val docs = List(block, block1, doc).map(toMapper.toSolrDoc)

    // Add docs
    solr.add(docs.asJava)
    solr.commit()

    // Read docs from solrClient
    val resp = solr.query(new SolrQuery("*:*"))
    val resultDocs = resp.getResults.asScala.toList
    val result = resultDocs.map(fromMapper.fromSolrDoc).map(_.get)

    // Child docs are empty since *:* query does not join
    result should have size 6
    result should contain allElementsOf List(const, doc, fact, typ)
    val resultBlocks = result.collect { case c: CodeblockEt => c } map (_.id)
    resultBlocks should contain theSameElementsAs List(doc, block, block1).map(_.id)
  }
}
