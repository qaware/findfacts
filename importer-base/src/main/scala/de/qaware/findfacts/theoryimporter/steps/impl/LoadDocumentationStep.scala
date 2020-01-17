package de.qaware.findfacts.theoryimporter.steps.impl

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.{DocKind, DocumentationEt}
import de.qaware.findfacts.theoryimporter.TheoryView
import de.qaware.findfacts.theoryimporter.steps.{ImportError, ImportStep, StepContext}

/** Import stap that loads documentation comments. */
class LoadDocumentationStep extends ImportStep {

  /** Keywords identifying latex documentation cartouches. */
  final val LatexKeywords =
    Set("chapter", "section", "subsection", "subsubsection", "paragraph", "subparagraph", "text", "txt", "text_raw")

  /** Cartouche opening isabelle character. */
  final val CartoucheOpen = "\\<open>"

  /** Cartouche closing isabelle character. */
  final val CartoucheClose = "\\<close>"

  /** Comment opening sequence. */
  final val CommentOpen = "(*"

  /** Comment clsoing sequence. */
  final val CommentClose = "*)"

  private val logger = Logger[LoadDocumentationStep]

  override def apply(theories: Seq[TheoryView.Theory])(implicit ctx: StepContext): List[ImportError] = {
    logger.info("Loading documentation...")

    val docs = for {
      thy <- theories
      block <- thy.source.blocks
      kind <- getDocKind(block.text)
    } yield new DocumentationEt(thy.name, block.start, block.stop, block.text, kind)

    docs.foreach(ctx.addEntity)

    logger.info(s"Found ${docs.size} documentation entries.")

    List.empty
  }

  private def getDocKind(srcText: String): Option[DocKind] = {
    if (srcText.startsWith(CommentOpen) && srcText.endsWith(CommentClose)) {
      Some(DocKind.Meta)
    } else if (LatexKeywords.exists(srcText.startsWith) && srcText
        .split(' ')
        .drop(1)
        .mkString
        .stripLeading()
        .startsWith(CartoucheClose) && srcText.endsWith(CartoucheClose)) {
      Some(DocKind.Latex)
    } else {
      None
    }
  }
}
