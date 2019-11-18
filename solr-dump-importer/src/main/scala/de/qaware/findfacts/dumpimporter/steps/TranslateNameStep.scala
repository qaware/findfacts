package de.qaware.findfacts.dumpimporter.steps

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.dumpimporter.Config
import de.qaware.findfacts.dumpimporter.pure.PureSyntax

class TranslateNameStep(override val config: Config) extends ImportStep {
  private val logger = Logger[TranslateNameStep]

  override def apply(ctx: StepContext): Unit = {
    logger.info("Translating names...")

    // Map names to IDs once
    val constIdByName = ctx.consts.groupBy(_.name).mapValues(_.toSeq).mapValues {
      case Seq(value) => value.id
      case elems: Any => throw new IllegalStateException(s"Constant names are not unique: $elems")
    }
    val typeIdByName = ctx.types.groupBy(_.name).mapValues(_.toSeq).mapValues {
      case Seq(value) => value.id
      case elems: Any => throw new IllegalStateException(s"Type names are not unique: $elems")
    }

    // Logs entities for which IDs cannot be found
    def getOrLog(typ: String, map: Map[String, String], name: String) = {
      if (PureSyntax.values.exists(_.name == name)) {
        // Pure names are already resolved in representation
        None
      } else {
        val id = map.get(name)
        if (id.isEmpty) {
          logger.warn(s"Could not find $typ entity for name: $name")
        }
        id
      }
    }

    logger.info("Translating names used by constants...")
    // Update elements
    ctx.consts foreach { const =>
      val defUses = const.defUses.distinct.flatMap(getOrLog("const", constIdByName, _))
      val typeUses = const.typeUses.distinct.flatMap(getOrLog("type", typeIdByName, _))
      ctx.updateEntity(const, const.copy(typeUses = typeUses, defUses = defUses))
    }

    logger.info("Translating names used by types...")
    ctx.types foreach { typ =>
      val uses = typ.uses.flatMap(getOrLog("type", constIdByName, _))
      ctx.updateEntity(typ, typ.copy(uses = uses))
    }

    logger.info("Translating names used by facts...")
    ctx.facts foreach { fact =>
      val uses = fact.uses.flatMap(getOrLog("const", constIdByName, _))
      ctx.updateEntity(fact, fact.copy(uses = uses))
    }

    logger.info("Finished translating names.")
  }
}
