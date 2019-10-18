package de.qaware.dumpimporter.dataaccess

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.LoneElement.convertToCollectionLoneElementWrapper


/** Simple test node with integer elements. */
case class TestNode(i: Int, c: Seq[TestNode] = Seq()) extends Node[Int, TestNode] {
  override val data: Int = i
  override val children: Seq[TestNode] = c
}

class TreeQueryTest extends FunSuite with Matchers {

  // Running example
  val tree: Seq[TestNode] = Seq(TestNode(0, Seq(
    TestNode(1, Seq(
      TestNode(3),
      TestNode(4)
    )),
    TestNode(2)
  )))

  def eval(query: TreeQuery[TestNode], forest: Seq[TestNode] = tree): Iterable[Int] = query.find(forest).map(_.data)

  test("find even nodes") {
    eval(TreeQuery[TestNode](_.data % 2 == 0)) should contain theSameElementsAs Seq(0, 2, 4)
  }

  test("find first") {
    eval(TreeQuery[TestNode](_.data > 0).first()) should contain theSameElementsAs Seq(1, 2)
  }

  test("find parent") {
    eval(TreeQuery[TestNode](e => e.data == 0 || e.data > 2).parent()).loneElement should be (1)
  }

  test("and two queries") {
    eval(TreeQuery[TestNode](_.data < 2).and(TreeQuery[TestNode](_.data == 4).parent())).loneElement should be(1)
    eval(TreeQuery[TestNode](_.data > 0).first().and(TreeQuery[TestNode](_.data > 2).parent())).loneElement should be (1)
  }
}
