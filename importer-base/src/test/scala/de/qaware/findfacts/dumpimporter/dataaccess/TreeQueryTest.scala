package de.qaware.findfacts.dumpimporter.dataaccess

import scala.language.implicitConversions

import de.qaware.findfacts.dumpimporter.dataaccess.treequery.QueryDsl.{all => tAll, not => tNot, of => tOf, _}
import de.qaware.findfacts.dumpimporter.dataaccess.treequery.{FilterQuery, Node, QueryException}
import org.scalatest.LoneElement.convertToCollectionLoneElementWrapper
import org.scalatest.{FunSuite, Matchers}

/** Simple test node with integer elements. */
case class TestNode(i: Int, c: Seq[TestNode] = Seq()) extends Node[TestNode] {
  val data: Int = i
  override val children: Seq[TestNode] = c
}
object TestNode {
  implicit def toInt(t: TestNode): Int = t.data
}
object TestQuery {
  def number(matches: Int*): FilterQuery[TestNode] = {
    property[TestNode](i => matches.contains(i.data))
  }
}

class TreeQueryTest extends FunSuite with Matchers {
  import TestQuery._

  // Running example
  val tree: Seq[TestNode] = Seq(
    TestNode(0, Seq(TestNode(1, Seq(TestNode(3), TestNode(4, Seq(TestNode(5))))), TestNode(2))),
    TestNode(6),
    TestNode(7))

  private def shouldFind(query: Seq[TestNode], result: Seq[Int]) = {
    query.map(_.data) should contain theSameElementsAs result
  }
  private def shouldFind(query: Seq[TestNode], result: Int) = {
    query.map(_.data).loneElement should be(result)
  }

  test("Query should find single") {
    single thats number(2) in tree should matchPattern { case Right(TestNode(2, _)) => }
  }

  test("Query result should be in-order") {
    val res = tAll in tree
    res.map(_.data) should be(Seq(0, 1, 3, 4, 5, 2, 6, 7))
  }

  test("Querying single for multiple matches should give Error") {
    single thats number(1, 2) in tree should matchPattern { case Left(QueryException(_)) => }
  }

  test("Query thats on root level") {
    shouldFind(tAll thats (tNot(number(0))) in tree, Seq(1, 2, 3, 4, 5, 6, 7))
  }

  test("Query and filter") {
    shouldFind(tAll thats (number(1, 2) and number(2, 3)) in tree, 2)
  }

  test("Query or filter") {
    shouldFind(tAll thats (number(1) or number(2) or number(3)) in tree, Seq(1, 2, 3))
  }

  test("Query not filter") {
    shouldFind(tAll thats tNot(number(0)) in tree, Seq(1, 2, 3, 4, 5, 6, 7))
    shouldFind(tAll thats (tNot(number(1, 2)) and number(2, 3)) in tree, 3)
  }

  test("Query node thats (number i)") {
    shouldFind(tAll thats number(1, 4, 5) in tree, Seq(1, 4, 5))
  }

  test("Query next node") {
    shouldFind(tAll next ofOne thats number(1, 3, 4, 6) in tree, Seq(2, 4, 7))
    shouldFind(tAll next tOf next ofOne thats number(0, 1) in tree, 7)
  }

  test("Query previous node") {
    shouldFind(tAll previous ofOne in tree, Seq(0, 1, 3, 6))
    shouldFind(tAll previous ofOne thats number(6) in tree, 0)
  }

  test("Query parent node") {
    shouldFind(tAll parent ofOne thats number(0, 3, 4, 5) in tree, Seq(1, 4))
    shouldFind(tAll parent ofOne in tree, Seq(0, 1, 4))
    shouldFind(tAll parent tOf parent ofOne in tree, Seq(0, 1))
  }

  test("Query first hit") {
    shouldFind(tAll first ofOne thats number(1, 2, 3) in tree, Seq(1, 2))
    shouldFind(tAll first tOf first ofOne in tree, Seq(0, 6, 7))
  }

  test("Query root nodes") {
    shouldFind(tAll root ofOne thats number(2) in tree, Seq())
    shouldFind(tAll root ofOne thats number(0, 1) in tree, Seq(0))
  }

  test("Qury without") {
    shouldFind(tAll thats number(0, 1, 2, 3, 4, 5) without number(1) in tree, Seq(0, 2))
  }

  test("Combining queries") {
    shouldFind(tAll next tOf parent ofOne thats number(1, 3) in tree, Seq(2, 6))
    (single first ofOne parent ofOne previous ofOne thats number(4) in tree) should matchPattern {
      case Right(TestNode(1, _)) =>
    }
    shouldFind(tAll next tOf next tOf previous tOf previous ofOne in tree, 7)
    val filter = (number(1, 2, 3, 4) and tNot(number(1, 2, 3, 4) without number(2, 3)))
    shouldFind(tAll first tOf root tOf parent tOf next tOf previous ofOne thats filter in tree, 0)
  }
}
