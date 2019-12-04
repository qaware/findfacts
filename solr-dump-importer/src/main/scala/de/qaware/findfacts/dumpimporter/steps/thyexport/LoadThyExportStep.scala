package de.qaware.findfacts.dumpimporter.steps.thyexport

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.solr.{ConstRecord, FactRecord, TypeRecord}
import de.qaware.findfacts.dumpimporter.Config
import de.qaware.findfacts.dumpimporter.dataaccess.RepositoryReader
import de.qaware.findfacts.dumpimporter.steps.thyexport.IsabelleEntities.{Axiom, Theory, Thm}
import de.qaware.findfacts.dumpimporter.steps.{ImportStep, StepContext}
import de.qaware.findfacts.scalautils.ProgressLogger.withProgress

import scala.language.postfixOps
import scala.util.matching.Regex
import scala.util.{Failure, Success}

/** Import step to load a stable theory export.
  *
  * @param config of the importer
  */
class LoadThyExportStep(override val config: Config) extends ImportStep {

  /** File pattern of serialized theories. */
  final val SERIALIZED_FILE: Regex = ".*/theory/serialized_thy".r

  private val logger = Logger[LoadThyExportStep]

  override def apply(ctx: StepContext): Unit = {
    val thyFiles = RepositoryReader(config.dump).readAll(SERIALIZED_FILE)
    logger.info(s"Found ${thyFiles.size} serialized theories. Deserializing...")

    val theories = withProgress(thyFiles) flatMap { repoEntry =>
      Deserializer.deserialize(repoEntry.file.byteArray) match {
        case Failure(ex) =>
          logger.error(s"Could not deserialize $repoEntry:", ex)
          None
        case Success(value) =>
          Some(value)
      }
    } toIterable

    logger.info(s"Successfully deserialized ${theories.size} theories")

    logger.info("Extracting theory content...")
    withProgress(theories) foreach { thy =>
      // Partition axioms in constant definition axioms and "normal" axioms
      val constAxNames = thy.constdefs.map(_.axiomName).toSet
      val typeAxNames = thy.typedefs.map(_.axiomName).toSet
      val (defAxioms, axioms) =
        thy.axioms.partition(ax => constAxNames.contains(ax.entity.name) || typeAxNames.contains(ax.entity.name))
      val (constAxioms, typeAxioms) = defAxioms.partition(ax => constAxNames.contains(ax.entity.name))

      extractConsts(thy, constAxioms, ctx)
      extractTypes(thy, typeAxioms, ctx)
      extractFacts(thy.name, thy.thms, axioms, ctx)
    }
    logger.info(s"Found ${ctx.consts.size} constants, ${ctx.types.size} types, ${ctx.facts.size} facts.")

    logger.info("Enriching theories...")
    // Enrich data by grouping by position
    val entitiesByPos =
      (ctx.consts ++ ctx.facts ++ ctx.types).groupBy(e => (e.sourceFile, e.startPosition, e.endPosition))

    // Find related facts and constants, i.e. entities that spring from the same source positions
    (withProgress(entitiesByPos.keySet)) foreach { key =>
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
  private def extractConsts(thy: Theory, constAxioms: Seq[Axiom], context: StepContext): Unit = {
    // Find related axioms, if any
    val axNameByConstName = thy.constdefs.groupBy(_.name).mapValues(_.map(_.axiomName))
    val constAxiomsByName = constAxioms.groupBy(_.entity.name)

    thy.consts foreach { const =>
      val axioms = axNameByConstName.getOrElse(const.entity.name, Array.empty) flatMap { axName =>
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
        const.entity.startPos,
        const.entity.endPos,
        const.entity.name,
        const.typ.toString,
        const.typ.referencedTypes.toArray,
        axioms.map(_.prop.term.toString).mkString(" | "),
        axioms.flatMap(_.prop.term.referencedConsts)
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
  private def extractFacts(name: String, thms: Array[Thm], axioms: Seq[Axiom], context: StepContext): Unit = {
    thms foreach { thm =>
      context.addEntity(
        FactRecord(
          name,
          thm.entity.startPos,
          thm.entity.endPos,
          thm.entity.name,
          thm.prop.term.toString,
          thm.prop.term.referencedConsts.toArray,
          thm.deps
        ),
        Seq(thm.entity.serial)
      )
    }

    axioms foreach { ax =>
      context.addEntity(
        FactRecord(
          name,
          ax.entity.startPos,
          ax.entity.endPos,
          ax.entity.name,
          ax.prop.term.toString,
          ax.prop.term.referencedConsts.toArray,
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
      val axioms = typedefByTypeName.getOrElse(t.entity.name, Array.empty) flatMap { td =>
        typeAxiomsByName.getOrElse(td.axiomName, Seq.empty) match {
          case Seq(ax) => Some(ax)
          case axs: Any =>
            logger.error(s"Expected exactly one axiom for type ${td.axiomName}, got: ${axs.mkString(",")}")
            None
        }
      }

      val typeEntity = TypeRecord(
        thy.name,
        t.entity.startPos,
        t.entity.endPos,
        t.entity.name,
        axioms.map(_.prop.term.toString).mkString(" | "),
        axioms.flatMap(_.prop.term.referencedConsts)
      )

      // Register entity
      context.addEntity(typeEntity, Seq(t.entity.serial) ++ axioms.map(_.entity.serial))
    }
  }
}
