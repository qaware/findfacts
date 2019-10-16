package de.qaware.dumpimporter.steps.pide

import scala.languageFeature.implicitConversions

import de.qaware.dumpimporter.dataacess.{Node, Query}
import de.qaware.yxml.{Body, Inner, Tree}

case class PIDEField(name: String)
object PIDEField extends Enumeration {
  final val COMMENT = PIDEField("comment")
  final val NAME = PIDEField("name")
  final val DEFINITION = PIDEField("definition")
  final val ENTITY = PIDEField("entity")

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
  def body(): Query[Inner] = Query(_.data.getClass == classOf[Body], _.children)
  def tag(name: PIDEField): Query[Inner] = treeFilter(t => t.elem.name.str == name.name)
  def key(name: PIDEField): Query[Inner] = treeFilter(t => t.elem.kvPairs.exists(_.key.str == name.name))
  def value(content: String): Query[Inner] = treeFilter(t => t.elem.kvPairs.exists(_.value.str == content))
  def keyValue(key: PIDEField, value: String): Query[Inner] =
    treeFilter(t => t.elem.kvPairs.exists(p => p.key.str == key.name && p.value.str == value))
  def treeFilter(filter: Tree => Boolean): Query[Inner] =
    Query(_.data match {
      case t: Tree => filter(t)
      case _ => false
    }, _.children.filter(_.data.getClass == classOf[Tree]))
}
