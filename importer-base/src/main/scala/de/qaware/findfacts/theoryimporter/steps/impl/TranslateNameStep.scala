package de.qaware.findfacts.theoryimporter.steps.impl

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.ThyEtKind
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
      val defUses = const.propositionUses.distinct.filterNot(pureFilter).map(toUniqueKey(ThyEtKind.Constant, _))
      val typeUses = const.typeUses.distinct.filterNot(pureFilter).map(toUniqueKey(ThyEtKind.Type, _))
      ctx.updateEntity(const, const.copy(typeUses = typeUses, propositionUses = defUses))
    }

    ctx.types foreach { typ =>
      val uses = typ.propositionUses.distinct.filterNot(pureFilter).map(toUniqueKey(ThyEtKind.Type, _))
      ctx.updateEntity(typ, typ.copy(propositionUses = uses))
    }

    logger.info(s"Translating names used by ${ctx.facts.size} facts...")
    ctx.facts foreach { fact =>
      val uses = fact.propositionUses.distinct.filterNot(pureFilter).map(toUniqueKey(ThyEtKind.Constant, _))
      val proofUses = fact.proofUses.distinct.filterNot(pureFilter).map(toUniqueKey(ThyEtKind.Fact, _))
      ctx.updateEntity(fact, fact.copy(propositionUses = uses, proofUses = proofUses))
    }

    logger.info("Finished translating names.")
    List.empty
  }

  /** Filter out pure stuff */
  private def pureFilter(name: String): Boolean = name.startsWith("Pure.") || PureSyntax.values.exists(_.name == name)

  /** Make unique key */
  private def toUniqueKey(kind: ThyEtKind, name: String): String = s"${kind.entryName}.$name"
}
