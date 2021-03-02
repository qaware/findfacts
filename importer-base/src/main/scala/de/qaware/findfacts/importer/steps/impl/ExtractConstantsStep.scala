package de.qaware.findfacts.importer.steps.impl

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.{ConstantEt, Kind}
import de.qaware.findfacts.importer.TheoryView.Const
import de.qaware.findfacts.importer.steps.impl.thy.{PropExtractor, TermExtractor, TypExtractor}
import de.qaware.findfacts.importer.steps.impl.util.IdBuilder
import de.qaware.findfacts.importer.steps.{ImportStep, StepContext}
import de.qaware.findfacts.importer.{ImportError, TheoryView}

/**
 * Step to extract constants from theory view.
 *
 * @param idBuilder to build entity ids
 * @param typExtractor to extract types
 * @param termExtractor to extract types from abbreviation terms
 * @param propExtractor to extract references from propositions
 */
class ExtractConstantsStep(
    idBuilder: IdBuilder,
    typExtractor: TypExtractor,
    termExtractor: TermExtractor,
    propExtractor: PropExtractor)
  extends ImportStep {
  private val logger = Logger[ExtractConstantsStep]

  @SuppressWarnings(Array("TraversableHead"))
  override def execute(theory: TheoryView.Theory)(implicit ctx: StepContext): List[ImportError] = {
    logger.debug(s"Importing ${theory.consts.size} constants with ${theory.constdefs.size} def axioms...")

    // Store names in set for fast lookup
    val axiomNames: Set[String] = theory.constdefs.map(_.axiomName).toSet

    val errors = ListBuffer.empty[ImportError]

    // Group relevant axioms by their names
    val axiomsByName = theory.axioms
      .filter(ax => axiomNames.contains(ax.entity.name))
      .groupBy(_.entity.name)
      .toList
      .map {
        case (name, List(axiom)) => name -> axiom
        case (name, axioms) =>
          errors += ImportError(this, name, "Multiple axioms for name", s"${axioms.mkString(",")}")
          name -> axioms.head
      }
      .toMap

    // Convert mapping to Map
    val axiomNamesByConst = theory.constdefs
      .groupBy(_.name)
      .view
      .mapValues(_.map(_.axiomName))
      .toMap

    // Build entities
    theory.consts foreach { const =>
      val fullName = const.entity.name
      val name = fullName.split('.').drop(1).mkString(".")

      // Axioms may be empty for a constant BUT if a constant is in constdefs then the axiom must be present
      val axioms = axiomNamesByConst
        .getOrElse(fullName, List.empty)
        .flatMap(axName => axiomsByName.get(axName).orElse(logNotFound(axName, const, errors)))

      val props = axioms.map(_.prop)

      // Collect uses
      val usedTypes = (typExtractor.referencedTypes(const.typ) ++ props.flatMap(propExtractor.referencedTypes) ++
        const.abbrev.toList.flatMap(termExtractor.referencedTypes)).toList
      val usedConsts = props.flatMap(propExtractor.referencedConsts).filterNot(fullName.equals) ++
        const.abbrev.toList.flatMap(termExtractor.referencedConsts)

      val uses = idBuilder.getIds(Kind.Type, usedTypes) ++ idBuilder.getIds(Kind.Constant, usedConsts)

      // Add entity to context
      val et =
        ConstantEt(idBuilder.theoryEtId(Kind.Constant, fullName), name, uses, typExtractor.prettyPrint(const.typ))
      ctx.consts.getOrElseUpdate(const.entity.pos, { mutable.Set.empty }).addOne(et)
    }

    logger.debug(s"Finished importing constants with ${errors.size} errors")
    errors.toList
  }

  private def logNotFound(axName: String, const: Const, errors: ListBuffer[ImportError]): Option[Nothing] = {
    errors += ImportError(this, axName, "Axiom for constdef not found", s"Const: $const")
    None
  }
}
