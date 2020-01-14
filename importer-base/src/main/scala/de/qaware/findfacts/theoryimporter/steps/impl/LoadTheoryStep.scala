package de.qaware.findfacts.theoryimporter.steps.impl

import scala.language.postfixOps

import cats.implicits._
import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.{BlockEt, ConstantEt, FactEt, TypeEt}
import de.qaware.findfacts.common.utils.LoggingUtils.doDebug
import de.qaware.findfacts.theoryimporter.TheoryView.{Axiom, Block, Position, Source, Theory, Thm}
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

      extractFacts(theory.name, theory.source, theory.thms.distinct, axioms, ctx) ++
        extractConsts(theory, constAxioms, ctx) ++
        extractTypes(theory, typeAxioms, ctx)
    } toList

    logger.info(s"Extracted ${ctx.consts.size} constants, ${ctx.types.size} types, ${ctx.facts.size} facts.")

    errors
  }

  /** Extracts constants from theory.
    *
    * @param thy         to extract constants from
    * @param constAxioms of constant definitions, pre-computed
    * @param context     to write constants to
    */
  private def extractConsts(thy: Theory, constAxioms: Seq[Axiom], context: StepContext): List[ImportError] = {
    // Find related axioms, if any
    val axNameByConst: Map[String, List[String]] = thy.constdefs.groupBy(_.name).mapValues(_.map(_.axiomName))
    val constAxiomsByName: Map[String, Seq[Axiom]] = constAxioms.groupBy(_.entity.name)

    thy.consts
      .groupBy(c => (c.entity.name, c.entity.pos, c.typargs, c.typ))
      .flatMap {
        case ((name, pos, _, typ), consts) =>
          val res: Either[Seq[ImportError], Unit] = for {
            src <- findSrc(s"Const $pos:$name", consts.map(_.entity.pos), thy.source).left.map(Seq(_))
            axioms <- findAxioms(name, src, axNameByConst, constAxiomsByName)
          } yield
            context.addEntity(
              new ConstantEt(
                name,
                axioms.map(_.prop.term).map(termExtractor.prettyPrint).mkString(" | "),
                axioms.map(_.prop.term).flatMap(termExtractor.referencedConsts).toList,
                typExtractor.referencedTypes(typ).toList,
                typExtractor.prettyPrint(typ)
              ),
              new BlockEt(
                thy.name,
                src.start,
                src.stop,
                src.text,
              )
            )

          // Add entity or report errors
          res.fold(identity, _ => List.empty[ImportError])
    } toList
  }

  /** Extracts facts from theory.
    *
    * @param theoryName    of the theory
    * @param thms    to extract facts from
    * @param axioms  that do not belong to const/thm definition
    * @param context to write fact entities into
    */
  private def extractFacts(
      theoryName: String,
      source: Source,
      thms: Seq[Thm],
      axioms: Seq[Axiom],
      context: StepContext): List[ImportError] = {
    val thmErrors = thms map { thm =>
      findSrc(s"Thm ${thm.entity}", Seq(thm.entity.pos), source).right map { src =>
        context.addEntity(
          new FactEt(
            thm.entity.name,
            termExtractor.prettyPrint(thm.prop.term),
            termExtractor.referencedConsts(thm.prop.term).toList,
            thm.deps
          ),
          new BlockEt(theoryName, src.start, src.stop, src.text)
        )
      }
    }

    val axErrors = axioms map { ax =>
      findSrc(s"Axiom ${ax.entity}", Seq(ax.entity.pos), source).right map { src =>
        context.addEntity(
          new FactEt(
            ax.entity.name,
            termExtractor.prettyPrint(ax.prop.term),
            termExtractor.referencedConsts(ax.prop.term).toList,
            List.empty
          ),
          new BlockEt(theoryName, src.start, src.stop, src.text)
        )
      }
    }

    (thmErrors ++ axErrors).flatMap(_.swap.toOption).toList
  }

  /** Extracts types from theory.
    *
    * @param thy        to extract from
    * @param typeAxioms of type definitions, pre-computed
    * @param context    to write type entities into
    */
  private def extractTypes(thy: Theory, typeAxioms: Seq[Axiom], context: StepContext): List[ImportError] = {
    val typedefByTypeName = thy.typedefs.groupBy(_.name).mapValues(_.map(_.axiomName))
    val typeAxiomsByName = typeAxioms.groupBy(_.entity.name)

    thy.types.distinct flatMap { typ =>
      val res = for {
        src <- findSrc(s"Type ${typ.entity}", Seq(typ.entity.pos), thy.source).left.map(Seq(_))
        axioms <- findAxioms(typ.entity.name, src, typedefByTypeName, typeAxiomsByName)
      } yield
        context.addEntity(
          new TypeEt(
            typ.entity.name,
            axioms.map(_.prop.term).map(termExtractor.prettyPrint).mkString(" | "),
            axioms.map(_.prop.term).flatMap(termExtractor.referencedConsts).toList
          ),
          new BlockEt(thy.name, src.start, src.stop, src.text)
        )

      res.fold(identity, _ => List.empty[ImportError])
    }
  }

  private def findSrc(debugIdentifier: String, positions: Seq[Position], source: Source): Either[ImportError, Block] = {
    positions.flatMap(source.get).distinct match {
      case Nil => Left(doDebug(ImportError(this, debugIdentifier, "No source", s"at ${positions.mkString(",")}")))
      case Seq(src) => Right(src)
      case srcs =>
        Left(
          doDebug(
            ImportError(
              this,
              debugIdentifier,
              "Multiple sources",
              s"at ${positions.mkString(",")}: ${srcs.mkString(",")}")))
    }
  }

  private def findAxioms(
      name: String,
      sourceBlock: Block,
      axNameMapping: Map[String, List[String]],
      axiomsByName: Map[String, Seq[Axiom]]): Either[List[ImportError], Seq[Axiom]] = {
    val axNames = axNameMapping.getOrElse(name, Nil)
    val (errors, axioms) = axNames
      .map(axName =>
        axiomsByName.getOrElse(axName, Nil) match {
          case Nil => Left(doDebug(ImportError(this, axName, "No axiom for name", s"in $sourceBlock:$name")))
          case axioms => Right(axioms.filter(ax => sourceBlock.contains(ax.entity)))
      })
      .separate

    errors match {
      case Nil => Right(axioms.flatten)
      case _ => Left(errors)
    }
  }
}
