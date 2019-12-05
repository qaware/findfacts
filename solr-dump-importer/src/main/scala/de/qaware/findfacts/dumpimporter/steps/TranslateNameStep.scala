package de.qaware.findfacts.dumpimporter.steps

import scala.collection.mutable

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.solr.Record.Id
import de.qaware.findfacts.common.solr.TheoryRecord
import de.qaware.findfacts.common.utils.ProgressLogger.withProgress
import de.qaware.findfacts.dumpimporter.Config
import de.qaware.findfacts.dumpimporter.pure.PureSyntax

/** Step to translate element names to unique ids.
  *
  * @param config parameter
  */
class TranslateNameStep(override val config: Config) extends ImportStep {
  private val logger = Logger[TranslateNameStep]

  override def apply(ctx: StepContext): Unit = {
    logger.info("Translating names - building cache...")

    // Store names that cannot be found in order not to pollute the log
    val missingNames = mutable.Set.empty[String]

    // Map names to IDs once
    val constIdByName = groupByName(ctx.consts, missingNames)
    val typeIdByName = groupByName(ctx.types, missingNames)
    val factIdByName = groupByName(ctx.facts, missingNames)

    logger.info(s"Translating names used by ${ctx.consts.size} constants...")
    // Update elements
    withProgress(ctx.consts) foreach { const =>
      val defUses = const.propositionUses.distinct.flatMap(getOrLog("const", constIdByName, missingNames, _))
      val typeUses = const.typeUses.distinct.flatMap(getOrLog("type", typeIdByName, missingNames, _))
      ctx.updateEntity(const, const.copy(typeUses = typeUses, propositionUses = defUses))
    }

    logger.info(s"Translating names used by ${ctx.types.size} types...")
    withProgress(ctx.types) foreach { typ =>
      val uses = typ.uses.flatMap(getOrLog("type", constIdByName, missingNames, _))
      ctx.updateEntity(typ, typ.copy(uses = uses))
    }

    logger.info(s"Translating names used by ${ctx.facts.size} facts...")
    withProgress(ctx.facts) foreach { fact =>
      val uses = fact.uses.flatMap(getOrLog("const", constIdByName, missingNames, _))
      val proofUses = fact.proofUses.flatMap(getOrLog("fact", factIdByName, missingNames, _))
      ctx.updateEntity(fact, fact.copy(uses = uses, proofUses = proofUses))
    }

    logger.info("Finished translating names.")
  }

  private def groupByName[T <: TheoryRecord](entities: Set[T], missing: mutable.Set[String]): Map[String, Id] = {
    entities.groupBy(_.name) filter {
      case (name, vs) =>
        if (vs.map(_.id).size == 1) {
          true
        } else {
          logger.error(s"Found non-unique $name in ${vs.map(_.sourceFile).mkString(", ")}")
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
        logger.warn(s"Could not find $typ entity for name: $name")
        missing += name
      }
      id
    }
  }
}
