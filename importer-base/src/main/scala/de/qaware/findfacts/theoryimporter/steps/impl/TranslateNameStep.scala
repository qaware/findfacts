package de.qaware.findfacts.theoryimporter.steps.impl

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.EtKind
import de.qaware.findfacts.theoryimporter.TheoryView.Theory
import de.qaware.findfacts.theoryimporter.pure.PureSyntax
import de.qaware.findfacts.theoryimporter.steps.{ImportError, ImportStep, StepContext}

/** Step to translate element names to unique ids. */
class TranslateNameStep extends ImportStep {
  private val logger = Logger[TranslateNameStep]

  override def apply(theories: Seq[Theory])(implicit ctx: StepContext): List[ImportError] = {
    logger.info("Translating names to ids")

    // Update elements
    ctx.consts foreach { const =>
      val defUses = const.propositionUses.distinct.filter(pureFilter).map(toUniqueKey(EtKind.Constant, _))
      val typeUses = const.typeUses.distinct.filter(pureFilter).map(toUniqueKey(EtKind.Type, _))
      ctx.updateEntity(const, const.copy(typeUses = typeUses, propositionUses = defUses))
    }

    ctx.types foreach { typ =>
      val uses = typ.uses.distinct.filter(pureFilter).map(toUniqueKey(EtKind.Type, _))
      ctx.updateEntity(typ, typ.copy(uses = uses))
    }

    logger.info(s"Translating names used by ${ctx.facts.size} facts...")
    ctx.facts foreach { fact =>
      val uses = fact.uses.distinct.filter(pureFilter).map(toUniqueKey(EtKind.Constant, _))
      val proofUses = fact.proofUses.distinct.filter(pureFilter).map(toUniqueKey(EtKind.Fact, _))
      ctx.updateEntity(fact, fact.copy(uses = uses, proofUses = proofUses))
    }

    logger.info("Finished translating names.")
    Nil
  }

  // Filter out pure stuff
  private def pureFilter(name: String): Boolean = !name.startsWith("Pure.") && !PureSyntax.values.exists(_.name == name)

  private def toUniqueKey(kind: EtKind, name: String): String = s"${kind.entryName}.$name"
}
