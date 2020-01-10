package de.qaware.findfacts.theoryimporter.steps.impl

import scala.language.postfixOps

import cats.implicits._
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.solr.{ConstRecord, FactRecord, TypeRecord}
import de.qaware.findfacts.theoryimporter.TheoryView.{Axiom, Source, Theory, Thm, Typedef}
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

    val errors = theories flatMap { theory =>
      // Partition axioms in constant definition axioms and "normal" axioms
      val constAxNames: Set[String] = theory.constdefs.map(_.axiomName).toSet
      val typeAxNames: Set[String] = theory.typedefs.map(_.axiomName).toSet
      val (defAxioms, axioms) =
        theory.axioms.partition(ax => constAxNames.contains(ax.entity.name) || typeAxNames.contains(ax.entity.name))
      val (constAxioms, typeAxioms) = defAxioms.partition(ax => constAxNames.contains(ax.entity.name))

      // Extracting facts doesn't create errors
      extractFacts(theory.name, theory.thms.distinct, axioms, ctx)(theory.source) ++
        extractConsts(theory, constAxioms, ctx) ++
        extractTypes(theory, typeAxioms, ctx)
    } toList

    logger.info(s"Extracted ${ctx.consts.size} constants, ${ctx.types.size} types, ${ctx.facts.size} facts.")

    errors
  }

  /** Extracts constants from theory.
    *
    * @param thy to extract constants from
    * @param constAxioms of constant definitions, pre-computed
    * @param context to write constants to
    */
  private def extractConsts(thy: Theory, constAxioms: Seq[Axiom], context: StepContext): List[ImportError] = {
    import thy.source

    // Find related axioms, if any
    val axNameByConst = thy.constdefs.groupBy(_.name).mapValues(_.map(_.axiomName))
    val constAxiomsByName = constAxioms.groupBy(_.entity.name)

    val groupedConstants = thy.consts.groupBy(c => (c.entity.name, c.entity.pos, c.typargs, c.typ))
    groupedConstants.flatMap {
      case ((name, pos, _, typ), consts) =>
        val (errors, axioms) = (for {
          axName <- axNameByConst.getOrElse(name, List.empty)
          ax <- constAxiomsByName.get(axName)
        } yield {
          ax match {
            case Seq(ax) => Right(ax)
            case axs =>
              logger.whenDebugEnabled {
                logger.warn(s"Expected exactly one axiom for name $axName, got: ${axs.mkString(",")}")
              }
              Left(ImportError(this, axName, "non-unique constdef axiom name"))
          }
        }).separate

        if (errors.isEmpty) {
          if (axioms.isEmpty) {
            logger.whenDebugEnabled { logger.debug(s"No constant definition for $name") }
          }

          val defPositions = consts.map(_.entity.pos) ++ axioms.map(_.entity.pos)
          defPositions.map(p => source.get(p.offset, p.endOffset)).filter(_.isDefined).distinct match {
            case List(Some(src)) =>
              // Enrich const data with axiom def
              val entity = ConstRecord(
                thy.name,
                pos.offset,
                pos.endOffset,
                name,
                typExtractor.prettyPrint(typ),
                typExtractor.referencedTypes(typ).toArray,
                axioms.map(_.prop.term).map(termExtractor.prettyPrint).mkString(" | "),
                axioms.map(_.prop.term).flatMap(termExtractor.referencedConsts).toArray,
                Array.empty,
                src.text
              )

              // Register entity
              context.addEntity(entity)
              None
            case _ => Some(ImportError(this, name, "Not a single source for const"))
          }
        } else {
          errors
        }
    } toList
  }

  /** Extracts facts from theory.
    *
    * @param name of the theory
    * @param thms to extract facts from
    * @param axioms that do not belong to const/thm definition
    * @param context to write fact entities into
    */
  private def extractFacts(name: String, thms: Seq[Thm], axioms: Seq[Axiom], context: StepContext)(
      implicit source: Source): List[ImportError] = {
    (thms flatMap { thm =>
      source.get(thm.entity.pos.offset, thm.entity.pos.endOffset) match {
        case None => Some(ImportError(this, thm.entity.name, "No source for thm"))
        case Some(src) =>
          context.addEntity(
            FactRecord(
              name,
              thm.entity.pos.offset,
              thm.entity.pos.endOffset,
              thm.entity.name,
              termExtractor.prettyPrint(thm.prop.term),
              termExtractor.referencedConsts(thm.prop.term).toArray,
              thm.deps.toArray,
              Array.empty,
              src.text
            )
          )
          None
      }
    }).toList ++
      (axioms flatMap { ax =>
        source.get(ax.entity.pos.offset, ax.entity.pos.endOffset) match {
          case None => Some(ImportError(this, ax.entity.name, "No source for axiom"))
          case Some(src) =>
            context.addEntity(
              FactRecord(
                name,
                ax.entity.pos.offset,
                ax.entity.pos.endOffset,
                ax.entity.name,
                termExtractor.prettyPrint(ax.prop.term),
                termExtractor.referencedConsts(ax.prop.term).toArray,
                Array.empty,
                Array.empty,
                src.text
              )
            )
            None
        }
      })
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

    thy.types.distinct flatMap { t =>
      val (errors, axioms) = (typedefByTypeName.getOrElse(t.entity.name, List.empty) map { typDef: Typedef =>
        typeAxiomsByName.getOrElse(typDef.axiomName, Seq.empty) match {
          case Seq(ax) => Right(ax)
          case axs: Any =>
            logger.error(s"Expected exactly one axiom for type ${typDef.axiomName}, got: ${axs.mkString(",")}")
            Left(ImportError(this, typDef.axiomName, "non-unique typedef axiom name"))
        }
      }).separate

      if (errors.isEmpty) {
        val defPositions = t.entity.pos +: axioms.map(_.entity.pos)
        defPositions.map(p => thy.source.get(p.offset, p.endOffset)).filter(_.isDefined).distinct match {
          case List(Some(src)) =>
            val typeEntity = TypeRecord(
              thy.name,
              t.entity.pos.offset,
              t.entity.pos.endOffset,
              t.entity.name,
              axioms.map(_.prop.term).map(termExtractor.prettyPrint).mkString(" | "),
              axioms.map(_.prop.term).flatMap(termExtractor.referencedConsts).toArray,
              Array.empty,
              src.text
            )

            // Register entity
            context.addEntity(typeEntity)
            None
          case _ => Some(ImportError(this, t.entity.name, "Not a single source for type"))
        }
      } else {
        errors
      }
    }
  }
}
