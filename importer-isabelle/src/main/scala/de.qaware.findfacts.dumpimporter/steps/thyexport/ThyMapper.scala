package de.qaware.findfacts.dumpimporter.steps.thyexport

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.solr.{ConstRecord, FactRecord, TypeRecord}
import de.qaware.findfacts.dumpimporter.steps.StepContext
import isabelle.Export_Theory.{Axiom, Const, Theory, Thm, Typedef}
import isabelle.{Markup, Position, Properties}

class ThyMapper(termExtractor: TermExtractor, typExtractor: TypExtractor) {
  private val logger = Logger[ThyMapper]

  def mapTheory(thy: Theory)(implicit ctx: StepContext): Unit  = {
    // Partition axioms in constant definition axioms and "normal" axioms
    val constAxNames = thy.constdefs.map(_.axiom_name).toSet
    val typeAxNames = thy.typedefs.map(_.axiom_name).toSet
    val (defAxioms, axioms) =
      thy.axioms.partition(ax => constAxNames.contains(ax.entity.name) || typeAxNames.contains(ax.entity.name))
    val (constAxioms, typeAxioms) = defAxioms.partition(ax => constAxNames.contains(ax.entity.name))

    extractConsts(thy, constAxioms, ctx)
    extractTypes(thy, typeAxioms, ctx)
    extractFacts(thy.name, thy.thms, axioms, ctx)

    findRelated()
  }

  protected def findRelated()(implicit ctx: StepContext): Unit = {
    // Enrich data by grouping by position
    val entitiesByPos =
      (ctx.consts ++ ctx.facts ++ ctx.types).groupBy(e => (e.sourceFile, e.startPosition, e.endPosition))

    // Find related facts and constants, i.e. entities that spring from the same source positions
    entitiesByPos.keySet foreach { key =>
      val entities = entitiesByPos(key)
      val allIds = entities.map(_.id)

      // Add every ID at this position except self.id to entities
      entities foreach {
        case entity: ConstRecord => ctx.updateEntity(entity, entity.copy(related = (allIds - entity.id).toArray))
        case entity: FactRecord => ctx.updateEntity(entity, entity.copy(related = (allIds - entity.id).toArray))
        case entity: TypeRecord => ctx.updateEntity(entity, entity.copy(related = (allIds - entity.id).toArray))
      }
    }
  }

  /** Extracts constants from theory.
   *
   * @param thy to extract constants from
   * @param constAxioms of constant definitions, pre-computed
   * @param context to write constants to
   */
  def extractConsts(thy: Theory, constAxioms: Seq[Axiom], context: StepContext): Unit = {
    // Find related axioms, if any
    val axNameByConstName = thy.constdefs.groupBy(_.name).mapValues(_.map(_.axiom_name))
    val constAxiomsByName = constAxioms.groupBy(_.entity.name)

    thy.consts foreach { const: Const =>
      val axioms = axNameByConstName.getOrElse(const.entity.name, List.empty) flatMap { axName =>
        constAxiomsByName.getOrElse(axName, Seq.empty) match {
          case Seq(ax) => Some(ax)
          case axs: Any =>
            logger.error(s"Expected exactly one axiom for name $axName, got: ${axs.mkString(",")}")
            None
        }
      }

      logger.whenDebugEnabled(if (axioms.isEmpty) { logger.debug(s"No constant definition for ${const.entity.name}") })

      // Enrich const data with axiom def
      val entity = ConstRecord(
        thy.name,
        // TODO more positional information
        Properties.get(const.entity.pos, Markup.OFFSET).get.toInt,
        Properties.get(const.entity.pos, Markup.END_OFFSET).get.toInt,
        const.entity.name,
        const.typ.toString,
        typExtractor.referencedTypes(const.typ).toArray,
        axioms.map(_.prop.term).map(termExtractor.prettyPrint(_)).mkString(" | "),
        axioms.map(_.prop.term).flatMap(termExtractor.referencedConsts).toArray
      )

      // Register entity and serials
      context.addEntity(entity, Seq(const.entity.serial) ++ axioms.map(_.entity.serial))
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
          Properties.get(thm.entity.pos, Markup.OFFSET).get.toInt,
          Properties.get(thm.entity.pos, Markup.END_OFFSET).get.toInt,
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
          Properties.get(ax.entity.pos, Markup.OFFSET).get.toInt,
          Properties.get(ax.entity.pos, Markup.END_OFFSET).get.toInt,
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
  private def extractTypes(thy: Theory, typeAxioms: Seq[Axiom], context: StepContext): Unit = {
    val typedefByTypeName = thy.typedefs.groupBy(_.name)
    val typeAxiomsByName = typeAxioms.groupBy(_.entity.name)

    thy.types foreach { t =>
      val axioms = typedefByTypeName.getOrElse(t.entity.name, List.empty) flatMap { typDef: Typedef =>
        typeAxiomsByName.getOrElse(typDef.axiom_name, Seq.empty) match {
          case Seq(ax) => Some(ax)
          case axs: Any =>
            logger.error(s"Expected exactly one axiom for type ${typDef.axiom_name}, got: ${axs.mkString(",")}")
            None
        }
      }

      val typeEntity = TypeRecord(
        thy.name,
        Properties.get(t.entity.pos, Markup.OFFSET).get.toInt,
        Properties.get(t.entity.pos, Markup.END_OFFSET).get.toInt,
        t.entity.name,
        axioms.map(_.prop.term.toString).mkString(" | "),
        axioms.map(_.prop.term).flatMap(termExtractor.referencedConsts).toArray
      )

      // Register entity
      context.addEntity(typeEntity, Seq(t.entity.serial) ++ axioms.map(_.entity.serial))
    }
  }
}
