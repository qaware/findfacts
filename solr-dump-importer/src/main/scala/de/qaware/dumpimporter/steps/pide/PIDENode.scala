package de.qaware.dumpimporter.steps.pide

import scala.language.implicitConversions

import de.qaware.dumpimporter.dataaccess.{Node, TreeQuery}
import de.qaware.yxml.{Body, Inner, KVPair, Tag, Tree}

case class PIDEField(name: String)
object PIDEField extends Enumeration {
  final val COMMENT = PIDEField("comment")
  final val NAME = PIDEField("name")
  final val DEFINITION = PIDEField("definition")
  final val DEF = PIDEField("def")
  final val ENTITY = PIDEField("entity")
  final val REF = PIDEField("ref")
  final val KIND = PIDEField("kind")
  final val CONSTANT = PIDEField("constant")

  implicit def toString(field: PIDEField): String = field.name
}

case class PIDENode(inner: Inner) extends Node[Inner, PIDENode] {
  override val data: Inner = inner

  override val children: Seq[PIDENode] = inner match {
    case _: Body => Seq.empty
    case t: Tree => t.inner.map(e => PIDENode(e))
  }

  override def toString: String = f"P${inner.toString}"

  def getBody: String = data match {
    case b: Body => b.str
    case e: Any => throw new IllegalStateException(f"Was no body element: $e")
  }

  def getValue(key: PIDEField): String = data match {
    case Tree(Tag(_, kvs), _) =>
      kvs.filter(_.key.str == key.name) match {
        case Seq() => throw new IllegalStateException(f"Had no key $key")
        case Seq(KVPair(_, value)) => value.str
        case _ => throw new IllegalStateException(f"Multiple key $key")
      }
    case e: Any => throw new IllegalStateException(f"Was no tag element: $e")
  }
}

object PIDENode {
  implicit def yxmlTree[A <: Inner](inner: A): PIDENode = PIDENode(inner)
}

object PIDEQuery {
  def body(): TreeQuery[PIDENode] = TreeQuery[PIDENode](_.data.getClass == classOf[Body])
  def tag(name: PIDEField): TreeQuery[PIDENode] = treeFilter(t => t.elem.name.str == name.name)
  def key(name: PIDEField): TreeQuery[PIDENode] = treeFilter(t => t.elem.kvPairs.exists(_.key.str == name.name))
  def value(content: String): TreeQuery[PIDENode] = treeFilter(t => t.elem.kvPairs.exists(_.value.str == content))
  def keyValue(key: PIDEField, value: String): TreeQuery[PIDENode] = {
    treeFilter(t => t.elem.kvPairs.exists(p => p.key.str == key.name && p.value.str == value))
  }

  private def treeFilter(filter: Tree => Boolean): TreeQuery[PIDENode] =
    TreeQuery[PIDENode](_.data match {
      case t: Tree => filter(t)
      case _ => false
    }, n => { case c: Any if n.children.contains(c) => c.data.getClass == classOf[Tree] })
}
