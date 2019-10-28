package de.qaware.dumpimporter.steps.pide

import scala.language.implicitConversions

import de.qaware.dumpimporter.dataaccess.treequery.{FilterQuery, Node}
import de.qaware.yxml.{Inner, KVPair, Markup, Text, Yxml, YxmlAST}

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
  final val KEYWORD = PIDEField("keyword")
  final val KEYWORD1 = PIDEField("keyword1")
  final val KEYWORD2 = PIDEField("keyword2")
  final val WHERE = PIDEField("where")
  final val STRING = PIDEField("string")
  final val DELETE = PIDEField("delete")
  final val PLAIN_TEXT = PIDEField("plain_text")

  implicit def toString(field: PIDEField): String = field.name
}

case class PIDENode(inner: YxmlAST) extends Node[PIDENode] {
  override val children: Seq[PIDENode] = inner match {
    case Markup(_, _, inner) => inner.elems.map(e => PIDENode(e))
    case _ => Seq.empty
  }

  override def toString: String = s"P${inner.toString}"

  def getBody: String = inner match {
    case Text(str) => str
    case e: Any => throw new IllegalStateException(s"Was no body element: $e")
  }

  def getValue(key: PIDEField): String = inner match {
    case Markup(_, kvs, _) =>
      kvs.filter(_._1 == key.name) match {
        case Seq() => throw new IllegalStateException(s"Had no key $key")
        case Seq((_, value)) => value
        case _ => throw new IllegalStateException(s"Multiple key $key")
      }
    case e: Any => throw new IllegalStateException(s"Was no tag element: $e")
  }
}

object PIDENode {
  implicit def fromInner(inner: Inner): PIDENode = PIDENode(inner)
}

object PIDEQuery {
  def body(): FilterQuery[PIDENode] = {
    FilterQuery[PIDENode](_.inner.getClass == classOf[Text])
  }
  def body(content: String): FilterQuery[PIDENode] = {
    FilterQuery[PIDENode](_.inner match {
      case Text(str) if str == content => true
      case _ => false
    })
  }
  def tag(): FilterQuery[PIDENode] = {
    treeFilter(_ => true)
  }
  def tag(name: PIDEField): FilterQuery[PIDENode] = {
    treeFilter(_.tag == name.name)
  }
  def key(name: PIDEField): FilterQuery[PIDENode] = {
    treeFilter(_.kvs.exists(_._1 == name.name))
  }
  def value(content: String): FilterQuery[PIDENode] = {
    treeFilter(_.kvs.exists(_._2 == content))
  }
  def keyValue(key: PIDEField, value: String): FilterQuery[PIDENode] = {
    treeFilter(_.kvs.exists(p => p._1 == key.name && p._2 == value))
  }
  private def treeFilter(filter: Markup => Boolean): FilterQuery[PIDENode] = {
    FilterQuery(_.inner match {
      case t: Markup => filter(t)
      case _ => false
    })
  }
}
