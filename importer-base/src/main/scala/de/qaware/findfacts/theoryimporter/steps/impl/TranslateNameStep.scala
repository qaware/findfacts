package de.qaware.findfacts.theoryimporter.steps.impl

import scala.collection.mutable

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.solr.Record.Id
import de.qaware.findfacts.common.solr.TheoryRecord
import de.qaware.findfacts.common.utils.ProgressLogger.withProgress
import de.qaware.findfacts.theoryimporter.pure.PureSyntax
import de.qaware.findfacts.theoryimporter.TheoryView
import de.qaware.findfacts.theoryimporter.TheoryView.Theory
import de.qaware.findfacts.theoryimporter.steps.{ImportError, ImportStep, StepContext}

/** Step to translate element names to unique ids. */
class TranslateNameStep extends ImportStep {
  private val logger = Logger[TranslateNameStep]

  override def apply(theories: Seq[Theory])(implicit ctx: StepContext): List[ImportError] = {
    logger.info("Translating names - building cache...")

    // Store names that cannot be found in order not to pollute the log
    val unresolvedNames = mutable.Set.empty[String]
    val missingNames = mutable.Set.empty[String]

    // Map names to IDs once
    val constIdByName = groupByName(ctx.consts, unresolvedNames)
    val typeIdByName = groupByName(ctx.types, unresolvedNames)
    val factIdByName = groupByName(ctx.facts, missingNames)

    logger.info(s"Translating names used by ${ctx.consts.size} constants...")
    // Update elements
    withProgress(ctx.consts) foreach { const =>
      val defUses = const.propositionUses.distinct
        .filterNot(unresolvedNames.contains)
        .flatMap(getOrLog("const", constIdByName, missingNames, _))
      val typeUses = const.typeUses.distinct
        .filterNot(unresolvedNames.contains)
        .flatMap(getOrLog("type", typeIdByName, missingNames, _))
      ctx.updateEntity(const, const.copy(typeUses = typeUses, propositionUses = defUses))
    }

    logger.info(s"Translating names used by ${ctx.types.size} types...")
    withProgress(ctx.types) foreach { typ =>
      val uses = typ.uses.distinct
        .filterNot(unresolvedNames.contains)
        .flatMap(getOrLog("type", constIdByName, missingNames, _))
      ctx.updateEntity(typ, typ.copy(uses = uses))
    }

    logger.info(s"Translating names used by ${ctx.facts.size} facts...")
    withProgress(ctx.facts) foreach { fact =>
      val uses = fact.uses.distinct
        .filterNot(unresolvedNames.contains)
        .flatMap(getOrLog("const", constIdByName, missingNames, _))
      val proofUses = fact.proofUses.flatMap(getOrLog("fact", factIdByName, missingNames, _))
      ctx.updateEntity(fact, fact.copy(uses = uses, proofUses = proofUses))
    }

    logger.info("Finished translating names.")

    unresolvedNames.toList.map(ImportError(this, _, "Non-unique name")) ++
      missingNames.toList.map(ImportError(this, _, "Could not find entity for name"))
  }

  @SuppressWarnings(Array("TraversableHead")) // Justification: groupBy produces only non-empty
  private def groupByName[T <: TheoryRecord](entities: Set[T], missing: mutable.Set[String]): Map[String, Id] = {
    entities.groupBy(_.name) filter {
      case (name, vs) =>
        if (vs.map(_.id).size == 1) {
          true
        } else {
          logger.whenDebugEnabled { logger.debug(s"Found non-unique $name in ${vs.map(_.sourceFile).mkString(", ")}") }
          missing += name
          false
        }
    } mapValues (_.toSeq.head.id)
  }

  /* Logs entities for which IDs cannot be found */
  private def getOrLog(typ: String, map: Map[String, String], missing: mutable.Set[String], name: String) = {
    if (missing.contains(name) || PureSyntax.values.exists(_.name == name)) {
      // Pure names are already resolved in representation, missing shouldn't be logged again
      None
    } else {
      val id = map.get(name)
      if (id.isEmpty) {
        logger.whenDebugEnabled { logger.debug(s"Could not find $typ entity for name: $name") }
        missing += name
      }
      id
    }
  }
}
