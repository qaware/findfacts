package de.qaware.findfacts.common.dt

import scala.collection.JavaConverters._

import de.qaware.findfacts.common.solr.mapper.{FromSolrDoc, ToSolrDoc}
import org.apache.solr.client.solrj.SolrQuery
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FunSuite, Matchers}

class SolrMappingIT extends FunSuite with Matchers with BeforeAndAfterEach with BeforeAndAfterAll {
  final val solrClient = ITSolr.apply().solrConnection()

  override def beforeEach(): Unit = {
    solrClient.deleteByQuery("*:*")
    val status = solrClient.commit()
    status.getStatus should (be(200) or be(0))
  }

  override def afterAll(): Unit = solrClient.close()

  case class Base1()
  case class Base2()

  test("Writing to and reading from solr") {
    val const = new ConstantEt("const", List("Hol.equiv", "Hol.inJ", "Nat"), "'a => Nat")
    val fact = new FactEt("const_is_pos", List("Const.const", "Fact.Hol.Stuff", "Fact.Hol.other"))
    val typ = new TypeEt("Nat", List("Nat"))

    val doc = new CodeblockEt(5, 7, "file", 2, "(*", "\n", "(* comment *)", "\n\n lemma...")
    val block = new CodeblockEt(8, 11, "file", 6, "fun", "*)\n", "fun ...src text...", "\n\n", List(const, fact))
    val block1 = new CodeblockEt(0, 6, "file1", 1, "other", "\n", "other src text", "\n", List(typ))

    val toMapper = ToSolrDoc[BaseEt]
    val fromMapper = FromSolrDoc[BaseEt]
    val docs = List(block, block1, doc).map(toMapper.toSolrDoc)

    // Add docs
    solrClient.add(docs.asJava)
    solrClient.commit()

    // Read docs from solrClient
    val resp = solrClient.query(new SolrQuery("*:*"))
    val resultDocs = resp.getResults.asScala.toList
    val result = resultDocs.map(fromMapper.fromSolrDoc).map(_.get)

    // Child docs are empty since *:* query does not join
    result should have size 6
    result should contain allElementsOf List(const, doc, fact, typ)
    val resultBlocks = result.collect { case c: CodeblockEt => c } map (_.id)
    resultBlocks should contain theSameElementsAs List(doc, block, block1).map(_.id)
  }
}
