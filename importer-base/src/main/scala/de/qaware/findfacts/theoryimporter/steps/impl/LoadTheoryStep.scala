package de.qaware.findfacts.theoryimporter.steps.impl

import scala.language.postfixOps

import cats.implicits._
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.solr.{ConstRecord, FactRecord, TypeRecord}
import de.qaware.findfacts.common.utils.ProgressLogger.withProgress
import de.qaware.findfacts.theoryimporter.TheoryView.{Axiom, Const, Theory, Thm, Typedef}
import de.qaware.findfacts.theoryimporter.steps.impl.thy.{TermExtractor, TypExtractor}
import de.qaware.findfacts.theoryimporter.steps.{ImportError, ImportStep, StepContext}

/** Import step to load a stable theory export.
  *
  * @param termExtractor term extractor
  * @param typExtractor typ extractor
  */
class LoadTheoryStep(termExtractor: TermExtractor, typExtractor: TypExtractor) extends ImportStep {

  private val logger = Logger[LoadTheoryStep]

  override def apply(theories: Seq[Theory])(implicit ctx: StepContext): List[ImportError] = {
    logger.info(s"Found ${theories.size} theories. Resolving ...")

    val errors = withProgress(theories) flatMap { theory =>
      // Partition axioms in constant definition axioms and "normal" axioms
      val constAxNames = theory.constdefs.map(_.axiomName).toSet
      val typeAxNames = theory.typedefs.map(_.axiomName).toSet
      val (defAxioms, axioms) =
        theory.axioms.partition(ax => constAxNames.contains(ax.entity.name) || typeAxNames.contains(ax.entity.name))
      val (constAxioms, typeAxioms) = defAxioms.partition(ax => constAxNames.contains(ax.entity.name))

      extractFacts(theory.name, theory.thms, axioms, ctx)
      extractConsts(theory, constAxioms, ctx) ++
        extractTypes(theory, typeAxioms, ctx)
    }

    logger.info(s"Extracted ${ctx.consts.size} constants, ${ctx.types.size} types, ${ctx.facts.size} facts.")

    errors.toList
  }

  /** Extracts constants from theory.
    *
    * @param thy to extract constants from
    * @param constAxioms of constant definitions, pre-computed
    * @param context to write constants to
    */
  private def extractConsts(thy: Theory, constAxioms: Seq[Axiom], context: StepContext): List[ImportError] = {
    // Find related axioms, if any
    val axNameByConst = thy.constdefs.groupBy(_.name).mapValues(_.map(_.axiomName))
    val constAxiomsByName = constAxioms.groupBy(_.entity.name)

    thy.consts flatMap { const: Const =>
      val (errors, axioms) = (for {
        axName <- axNameByConst.getOrElse(const.entity.name, List.empty)
        ax <- constAxiomsByName.get(axName)
      } yield {
        ax match {
          case Seq(ax) => Right(ax)
          case axs =>
            logger.whenDebugEnabled {
              logger.debug(s"Expected exactly one axiom for name $axName, got: ${axs.mkString(",")}")
            }
            Left(ImportError(this, axName, "non-unique constdef axiom name"))
        }
      }).separate

      if (errors.isEmpty) {
        if (axioms.isEmpty) {
          logger.whenDebugEnabled { logger.debug(s"No constant definition for ${const.entity.name}") }
        }

        // Enrich const data with axiom def
        val entity = ConstRecord(
          thy.name,
          const.entity.pos.offset,
          const.entity.pos.endOffset,
          const.entity.name,
          const.typ.toString,
          typExtractor.referencedTypes(const.typ).toArray,
          axioms.map(_.prop.term).map(termExtractor.prettyPrint(_)).mkString(" | "),
          axioms.map(_.prop.term).flatMap(termExtractor.referencedConsts).toArray
        )

        // Register entity and serials
        context.addEntity(entity, Seq(const.entity.serial) ++ axioms.map(_.entity.serial))
      }

      errors
    }
  }

  /** Extracts facts from theory.
    *
    * @param name of the theory
    * @param thms to extract facts from
    * @param axioms that do not belong to const/thm definition
    * @param context to write fact entities into
    */
  private def extractFacts(name: String, thms: Seq[Thm], axioms: Seq[Axiom], context: StepContext): Unit = {
    thms foreach { thm =>
      context.addEntity(
        FactRecord(
          name,
          thm.entity.pos.offset,
          thm.entity.pos.endOffset,
          thm.entity.name,
          thm.prop.term.toString,
          termExtractor.referencedConsts(thm.prop.term).toArray,
          thm.deps.toArray
        ),
        Seq(thm.entity.serial)
      )
    }

    axioms foreach { ax =>
      context.addEntity(
        FactRecord(
          name,
          ax.entity.pos.offset,
          ax.entity.pos.endOffset,
          ax.entity.name,
          termExtractor.prettyPrint(ax.prop.term),
          termExtractor.referencedConsts(ax.prop.term).toArray,
          Array.empty
        ),
        Seq(ax.entity.serial)
      )
    }
  }

  /** Extracts types from theory.
    *
    * @param thy to extract from
    * @param typeAxioms of type definitions, pre-computed
    * @param context to write type entities into
    */
  private def extractTypes(thy: Theory, typeAxioms: Seq[Axiom], context: StepContext): List[ImportError] = {
    val typedefByTypeName = thy.typedefs.groupBy(_.name)
    val typeAxiomsByName = typeAxioms.groupBy(_.entity.name)

    thy.types flatMap { t =>
      val (errors, axioms) = (typedefByTypeName.getOrElse(t.entity.name, List.empty) map { typDef: Typedef =>
        typeAxiomsByName.getOrElse(typDef.axiomName, Seq.empty) match {
          case Seq(ax) => Right(ax)
          case axs: Any =>
            logger.error(s"Expected exactly one axiom for type ${typDef.axiomName}, got: ${axs.mkString(",")}")
            Left(ImportError(this, typDef.axiomName, "non-unique typedef axiom name"))
        }
      }).separate

      if (errors.isEmpty) {
        val typeEntity = TypeRecord(
          thy.name,
          t.entity.pos.offset,
          t.entity.pos.endOffset,
          t.entity.name,
          axioms.map(_.prop.term.toString).mkString(" | "),
          axioms.map(_.prop.term).flatMap(termExtractor.referencedConsts).toArray
        )

        // Register entity
        context.addEntity(typeEntity, Seq(t.entity.serial) ++ axioms.map(_.entity.serial))
      }

      errors
    }
  }
}
