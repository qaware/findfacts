package de.qaware.dumpimporter.steps.pide

import scala.language.implicitConversions

import de.qaware.dumpimporter.dataaccess.{Node, TreeQuery}
import de.qaware.yxml.{Body, Inner, Tree}

case class PIDEField(name: String)
object PIDEField extends Enumeration {
  final val COMMENT = PIDEField("comment")
  final val NAME = PIDEField("name")
  final val DEFINITION = PIDEField("definition")
  final val ENTITY = PIDEField("entity")
  final val REF = PIDEField("ref")

  implicit def toString(field: PIDEField): String = field.name
}

case class PIDENode(inner: Inner) extends Node[Inner] {
  override val data: Inner = inner

  override val children: Seq[Node[Inner]] = inner match {
    case _: Body => Seq.empty
    case t: Tree => t.inner.map(PIDENode.yxmlTree)
  }
}
object PIDENode {
  implicit def yxmlTree[A <: Inner](inner: A): PIDENode = PIDENode(inner)
}

object PIDEQuery {
  def body(): TreeQuery[Inner] = TreeQuery(_.data.getClass == classOf[Body])
  def tag(name: PIDEField): TreeQuery[Inner] = treeFilter(t => t.elem.name.str == name.name)
  def key(name: PIDEField): TreeQuery[Inner] = treeFilter(t => t.elem.kvPairs.exists(_.key.str == name.name))
  def value(content: String): TreeQuery[Inner] = treeFilter(t => t.elem.kvPairs.exists(_.value.str == content))
  def keyValue(key: PIDEField, value: String): TreeQuery[Inner] = {
    treeFilter(t => t.elem.kvPairs.exists(p => p.key.str == key.name && p.value.str == value))
  }
  private def treeFilter(filter: Tree => Boolean): TreeQuery[Inner] =
    TreeQuery(_.data match {
      case t: Tree => filter(t)
      case _ => false
    }, n => { case c: Node[Inner] if n.children.contains(c) => c.data.getClass == classOf[Tree] })
}
