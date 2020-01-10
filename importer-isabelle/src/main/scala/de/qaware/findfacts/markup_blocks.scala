/*  Title:      findfacts/markup_tree.scala
    Author:     Fabian Huch, TU Munich/QAware GmbH

Markup block partitioning by command-spans.
*/

package de.qaware.findfacts


import isabelle._


object Markup_Blocks
{
  sealed case class Block(range: Text.Range, body: String)

  def from_XML(body: XML.Body): Markup_Blocks =
  {
    val body_text = XML.content(body)
    val markup_tree = Markup_Tree.from_XML(body)

    val blocks = markup_tree.branches.toList map {
      case (range, markup) =>
        val text = XML.content(markup.subtree.to_XML(range, body_text, Markup.Elements.full))
        Block(range, text)
    }

    new Markup_Blocks(blocks)
  }
}

final class Markup_Blocks private(val blocks: List[Markup_Blocks.Block])
{
  import Markup_Blocks._

  def get_containing(range: Text.Range): Option[Block] =
    blocks.find(_.range.contains(range))

  override def toString: String = blocks match {
    case Nil => "Empty"
    case list => list.mkString("Block(", ",", ")")
  }
}
