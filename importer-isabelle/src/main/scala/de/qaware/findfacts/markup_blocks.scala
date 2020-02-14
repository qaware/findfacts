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
    val blocks = body.foldLeft(Nil: List[Block]) {
      case (acc, text) =>
        val start_index = if (acc.isEmpty) 1 else acc.last.range.stop
        val content = XML.content(text)
        val html = HTML.output(Symbol.decode_yxml(content), hidden = true, structural = true)

        acc :+ Block(Text.Range(start_index, start_index + Symbol.length(content)), html)
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
