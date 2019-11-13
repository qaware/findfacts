package de.qaware.dumpimporter.dataaccess.treequery

import scala.util.matching.Regex

import de.qaware.dumpimporter.steps.pide.PideField
import de.qaware.yxml.{Markup, Text, Yxml, YxmlAst}

/** Tree query node class and initial node constructors for yxml trees.  */
object YxmlTreeQuery {

  /** Implement [[Node]] trait for yxml syntax tree.
    *
    * @param inner yxml AST
    */
  implicit class YxmlNode(val inner: YxmlAst) extends Node[YxmlNode] {
    override val children: Seq[YxmlNode] = inner match {
      case Yxml(elems) => elems.map(new YxmlNode(_))
      case Markup(_, _, yxml) => yxml.elems.map(new YxmlNode(_))
      case _ => Seq.empty
    }

    /** Gets body text. Call only on element that is returned by filter query filtering for bodies.
      * Throws [[IllegalArgumentException]] if called on a non-body node.
      *
      * @return inner body text
      */
    def getBody: String = inner match {
      case Text(str) => str
      case e: Any => throw new IllegalArgumentException(s"Was no body element: $e")
    }

    /** Gets value for a specific key. Call only on element that is returned by filter query filtering for that key.
      * Throws [[IllegalArgumentException]] if called on a node that does not contain the key exactly once.
      *
      * @param key to search value for
      * @return value for given key
      */
    def getValue(key: PideField.Value): String = inner match {
      case Markup(_, kvs, _) =>
        kvs.filter(_._1 == key.toString) match {
          case Seq() => throw new IllegalArgumentException(s"Had no key $key")
          case Seq((_, value)) => value
          case _ => throw new IllegalArgumentException(s"Multiple key $key")
        }
      case e: Any => throw new IllegalArgumentException(s"Was no tag element: $e")
    }
  }

  /** Creates a Filter query looking body elements.
    *
    * @return new FilterQuery
    */
  def body(): FilterQuery[YxmlNode] = {
    FilterQuery[YxmlNode](_.inner.getClass == classOf[Text])
  }

  /** Creates a filter query looking for body elements matching the given text exactly.
    *
    * @param content to match the body text
    * @return new FilterQuery
    */
  def body(content: String): FilterQuery[YxmlNode] = {
    FilterQuery[YxmlNode] {
      _.inner match {
        case Text(str) => str == content
        case _ => false
      }
    }
  }

  /** Creates a filter query looking for body elements matching the regex.
    *
    * @param content regex to match the body text
    * @return new FilterQuery
    */
  def body(content: Regex): FilterQuery[YxmlNode] = {
    FilterQuery[YxmlNode](_.inner match {
      case Text(str) => content.pattern.matcher(str).matches()
      case _ => false
    })
  }

  /** Creates a filter query looking for markup any tags.
    *
    * @return new FilterQurey
    */
  def tag(): FilterQuery[YxmlNode] = {
    treeFilter(_ => true)
  }

  /** Creates a filter query looking for specified markup tag.
    *
    * @param name of the tag to search
    * @return new FilterQuery
    */
  def tag(name: PideField.Value): FilterQuery[YxmlNode] = {
    treeFilter(_.tag == name.toString)
  }

  /** Creates a filter query looking for tags containing the specified key.
    *
    * @param name of the key to search in the tags' key-value pairs
    * @return new FilterQuery
    */
  def key(name: PideField.Value): FilterQuery[YxmlNode] = {
    treeFilter(_.kvs.exists(_._1 == name.toString))
  }

  /** Creates a filter query looking for markup elements with key-value pairs with the specified value.
    *
    * @param content value to look for
    * @return new FilterQuery
    */
  def value(content: String): FilterQuery[YxmlNode] = {
    treeFilter(_.kvs.exists(_._2 == content))
  }

  /** Creates a filter query looking for the specified key-value combination.
    *
    * @param key to look for in markup elements
    * @param value that the key should be assigned to
    * @return new FilterQuery
    */
  def keyValue(key: PideField.Value, value: String): FilterQuery[YxmlNode] = {
    treeFilter(_.kvs.exists(p => p._1 == key.toString && p._2 == value))
  }

  private def treeFilter(filter: Markup => Boolean): FilterQuery[YxmlNode] = {
    FilterQuery(_.inner match {
      case t: Markup => filter(t)
      case _ => false
    })
  }
}
