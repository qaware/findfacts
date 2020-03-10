package de.qaware.findfacts.theoryimporter.steps.impl

import scala.collection.mutable.ListBuffer

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.{Kind, TypeEt}
import de.qaware.findfacts.theoryimporter.TheoryView.Type
import de.qaware.findfacts.theoryimporter.steps.impl.thy.PropExtractor
import de.qaware.findfacts.theoryimporter.steps.impl.util.IdBuilder
import de.qaware.findfacts.theoryimporter.steps.{ImportStep, StepContext}
import de.qaware.findfacts.theoryimporter.{ImportError, TheoryView}

/** Step to extract types from a theory view.
  *
  * @param idBuilder to build entity ids
  * @param propExtractor to extract references from propositions
  */
class ExtractTypesStep(idBuilder: IdBuilder, propExtractor: PropExtractor) extends ImportStep {
  private val logger = Logger[ExtractTypesStep]

  override def apply(theory: TheoryView.Theory)(implicit ctx: StepContext): List[ImportError] = {
    logger.debug(s"Importing ${theory.types.size} types with ${theory.typedefs} def axioms...")

    // Store names in set for fast lookup
    val axiomNames: Set[String] = theory.typedefs.map(_.axiomName).toSet

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

    // Convert mapping to map
    val axiomNamesByTyp = theory.typedefs
      .groupBy(_.name)
      .mapValues(_.map(_.axiomName))

    theory.types foreach { typ =>
      // Build entities
      val name = typ.entity.name

      // Axioms may be empty for type BUT if there is a typedef then the axiom must be present
      val axioms = axiomNamesByTyp
        .getOrElse(name, List.empty)
        .flatMap(axName => axiomsByName.get(axName).orElse(logNotFound(axName, typ, errors)))

      val props = axioms.map(_.prop)

      // Usage in axioms
      val uses = idBuilder.getIds(Kind.Type, props.flatMap(propExtractor.referencedTypes).filterNot(name.equals)) ++
        idBuilder.getIds(Kind.Constant, props.flatMap(propExtractor.referencedConsts))

      ctx.types.addBinding(typ.entity.pos, new TypeEt(name, uses))
    }
    logger.debug(s"Finished importing types with ${errors.size} errors")
    errors.toList
  }

  private def logNotFound(axName: String, typ: Type, errors: ListBuffer[ImportError]): Option[Nothing] = {
    errors += ImportError(this, axName, "Axiom for typedef not found", s"Typ: $typ")
    None
  }
}
