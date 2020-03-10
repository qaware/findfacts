/*  Title:      findfacts/markup_tree.scala
    Author:     Fabian Huch, TU Munich/QAware GmbH

Markup block partitioning by command-spans.
*/

package de.qaware.findfacts


import isabelle._


object Markup_Blocks
{
  object Append_Token extends Enumeration
  {
    val ALSO = Value("also")
    val AND = Value("and")
    val APPLY = Value("apply")
    val ASSUME = Value("assume")
    val BY = Value("by")
    val CASE = Value("case")
    val CLOSE = Value("}")
    val DEFER = Value("defer")
    val DONE = Value("done")
    val END = Value("end")
    val FINALLY = Value("finally")
    val FIX = Value("fix")
    val FROM = Value("from")
    val HAVE = Value("have")
    val HENCE = Value("hence")
    val LET = Value("let")
    val MOREOVER = Value("moreover")
    val NEXT = Value("next")
    val NITPICK = Value("nitpick")
    val OBTAIN = Value("obtain")
    val OOPS = Value("oops")
    val PROOF = Value("proof")
    val QED = Value("qed")
    val SHOW = Value("show")
    val SUBGOAL = Value("subgoal")
    val TERMINATION = Value("termination")
    val THEN = Value("then")
    val THUS = Value("thus")
    val ULTIMATELY = Value("ultimately")
    val UNFOLDING = Value("unfolding")
    val USING = Value("using")
    val WHERE = Value("where")
    val WITH = Value("with")
  }

  sealed case class Block(range: Text.Range, start_line: Int, body: String)
  {
    def append(other: Block): Block =
    {
      require(range.stop == other.range.start)
      Block(range.try_join(other.range).get, start_line, body + other.body)
    }
  }

  def from_XML(body: XML.Body): Markup_Blocks =
  {
    var start_line = 1

    val blocks = body.foldLeft(Nil: List[Block]) {
      case (acc, text) =>
        val start_index = if (acc.isEmpty) 1 else acc.last.range.stop

        val content = XML.content(text)
        val html = HTML.output(Symbol.decode_yxml(content), hidden = false, structural = true)

        val block = Block(Text.Range(start_index, start_index + Symbol.length(content)), start_line, html)

        start_line += content.count {
          case '\n' => true
          case _ => false
        }

        val isAppendToken = content.isBlank || Append_Token.values.exists(tkn => content.trim.startsWith(tkn.toString))

        if (acc.nonEmpty && isAppendToken) {
          acc.dropRight(1) :+ acc.last.append(block)
        } else {
          acc :+ block
        }
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
