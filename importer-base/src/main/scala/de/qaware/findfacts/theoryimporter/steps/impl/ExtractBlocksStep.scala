package de.qaware.findfacts.theoryimporter.steps.impl

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.CodeblockEt
import de.qaware.findfacts.theoryimporter.ImportError
import de.qaware.findfacts.theoryimporter.TheoryView._
import de.qaware.findfacts.theoryimporter.steps.{ImportStep, StepContext}

/** Step to extract code blocks from theory view. */
class ExtractBlocksStep extends ImportStep {

  private val logger = Logger[ExtractBlocksStep]

  /** Maximum number of lines before and after the block. */
  private final val MaxContextLines = 5

  override def apply(theory: Theory)(implicit ctx: StepContext): List[ImportError] = {
    logger.debug(s"Importing ${theory.source.blocks.size} blocks.")

    val errors = theory.source.blocks.flatMap { src =>
      if (src.text.isBlank) {
        Some(ImportError(this, src.toString, "Empty block", ""))
      } else {
        val (before, inner, after) = getContext(theory.source, src)

        val cmdKind = getCommandKind(src.text)

        val block = new CodeblockEt(src.startPos, src.endPos, theory.name, src.startLine, cmdKind, before, inner, after)
        ctx.blocks.add(block)
        None
      }
    }

    logger.debug(s"Finished importing blocks.")

    errors
  }

  private def getCommandKind(src: String): String = {
    if (src.trim.startsWith("{")) {
      "{"
    } else if (src.trim.startsWith("(*")) {
      "(*"
    } else {
      src.trim.split("\\s+|\\(|\"|\\.|‹|\\{|\\[|%|✐|―").headOption.getOrElse("")
    }
  }

  private def getContext(source: Source, block: Block): (String, String, String) = {
    // Get source of the position before / after
    val blockBefore = source
      .get(new Position {
        override def offset: Int = block.startPos - 2
        override def endOffset: Int = block.startPos - 1
      })
      .map(_.text)
      .getOrElse("")

    val blockAfter = source
      .get(new Position {
        override def offset: Int = block.endPos + 1
        override def endOffset: Int = block.endPos + 2
      })
      .map(_.text)
      .getOrElse("")

    val before = blockBefore.linesWithSeparators.toList.takeRight(MaxContextLines).mkString
    val inner = block.text
    val after = blockAfter.linesWithSeparators.take(MaxContextLines).mkString

    (before, inner, after)
  }
}
