package de.qaware.findfacts.importer.steps.impl

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import com.typesafe.scalalogging.Logger

import de.qaware.findfacts.common.dt.CodeblockEt
import de.qaware.findfacts.importer.steps.impl.util.IdBuilder
import de.qaware.findfacts.importer.steps.{ImportStep, StepContext}
import de.qaware.findfacts.importer.{ImportError, TheoryView}

/**
 * Step to join code-blocks and theory entities.
 *
 * @param idBuilder to build ids
 */
class JoinBlockEntitiesStep(idBuilder: IdBuilder) extends ImportStep {
  private val logger = Logger[JoinBlockEntitiesStep]

  @SuppressWarnings(Array("TraversableHead"))
  override def execute(theory: TheoryView.Theory)(implicit ctx: StepContext): List[ImportError] = {
    logger.debug(s"Joining ${ctx.blocks.size} blocks with ${ctx.theoryEts.size}...")

    val errors = ListBuffer.empty[ImportError]

    // Create map from blocks by their id
    val blocksMap: mutable.Map[String, CodeblockEt] = mutable.Map(
      ctx.blocks.toList
        .groupBy(_.id)
        .toSeq
        .map {
          case (id, List(block)) => id -> block
          case (id, blocks) =>
            // Add error about duplicates, but keep block
            errors += ImportError(this, id, "Duplicate block id", s"For blocks: ${blocks.mkString(",")}")
            id -> blocks.head
        }: _*)

    // Go through entity positions
    val joinErrors = ctx.theoryEtsByPosition flatMap {
      case (pos, theoryEts) =>
        // Find block in source and get corresponding code-block
        val block = theory.source
          .get(pos)
          .flatMap(src => blocksMap.get(idBuilder.blockId(theory.name, src.startPos, src.endPos)))

        // Update block with the new entities
        block match {
          case Some(block) =>
            blocksMap.update(block.id, block.copy(entities = block.entities ++ theoryEts))
            None
          case None =>
            Some(ImportError(this, pos.toString, "Block not found for position", s"blocks: ${blocksMap.mkString(",")}"))
        }
    }

    // Store updated blocks in context
    ctx.blocks.clear()
    blocksMap.values.foreach(ctx.blocks.add)

    logger.debug(s"Finished joining blocks with ${joinErrors.size + errors.size} errors")
    joinErrors.toList ++ errors.toList
  }
}
