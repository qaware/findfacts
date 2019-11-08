package de.qaware.dumpimporter.steps.thyexport

import scala.language.postfixOps
import scala.util.{Failure, Success}

import com.typesafe.scalalogging.Logger
import de.qaware.common.solr.dt.{ConstEntity, FactEntity, TypeEntity}
import de.qaware.dumpimporter.Config
import de.qaware.dumpimporter.dataaccess.RepositoryReader
import de.qaware.dumpimporter.steps.thyexport.IsabelleEntities.{Axiom, Theory, Thm}
import de.qaware.dumpimporter.steps.{ImportStep, StepContext}

/** Import step to load a stable theory export.
  *
  * @param config of the importer
  */
class LoadThyExportStep(override val config: Config) extends ImportStep {
  private val logger = Logger[LoadThyExportStep]

  override def apply(context: StepContext): Unit = {
    val thyFiles = RepositoryReader(config.dump).readAll("(.*)theory/serialized_thy".r)
    logger.info(s"Found ${thyFiles.size} serialized theories. Deserializing...")

    val theories = thyFiles flatMap { repoEntry =>
      Deserializer.deserialize(repoEntry.file.byteArray) match {
        case Failure(ex) =>
          logger.error(s"Could not deserialize $repoEntry:", ex)
          None
        case Success(value) =>
          Some(value)
      }
    }
    logger.info(s"Successfully deserialized ${theories.size} theories")

    theories foreach { thy =>
      // Partition axioms in constant definition axioms and "normal" axioms
      val constAxNames = thy.constdefs.map(_.axiomName).toSet
      val typeAxNames = thy.typedefs.map(_.axiomName).toSet
      val (defAxioms, axioms) =
        thy.axioms.partition(ax => constAxNames.contains(ax.entity.name) || typeAxNames.contains(ax.entity.name))
      val (constAxioms, typeAxioms) = defAxioms.partition(ax => constAxNames.contains(ax.entity.name))

      context.consts ++= extractConsts(thy, constAxioms)
      context.types ++= extractTypes(thy, typeAxioms)
      context.facts ++= extractFacts(thy.name, thy.thms, axioms)
    }
    logger.info(s"Found ${context.consts.size} constants, ${context.types.size} types, ${context.facts.size} facts.")
  }

  private def extractConsts(thy: Theory, constAxioms: Seq[Axiom]): Seq[ConstEntity] = {
    // Find related axioms, if any
    val axNameByConstName = thy.constdefs.groupBy(_.name).mapValues(_.map(_.axiomName))
    val constAxiomsByName = constAxioms.groupBy(_.entity.name)

    thy.consts map { const =>
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
      ConstEntity(
        thy.name,
        const.entity.startPos,
        const.entity.endPos,
        const.entity.name,
        const.typ.toString,
        axioms.map(_.prop.term.toString),
        Array.empty)
    } toSeq
  }

  private def extractFacts(name: String, thms: Array[Thm], axioms: Seq[Axiom]): Seq[FactEntity] = {
    (thms map { thm =>
      FactEntity(name, thm.entity.startPos, thm.entity.endPos, thm.entity.name, thm.prop.term.toString, Array.empty)
    } toSeq) ++ (axioms map { ax =>
      FactEntity(name, ax.entity.startPos, ax.entity.endPos, ax.entity.name, ax.prop.term.toString, Array.empty)
    })
  }

  private def extractTypes(thy: Theory, typeAxioms: Seq[Axiom]): Seq[TypeEntity] = {
    val typedefByTypeName = thy.typedefs.groupBy(_.name)
    val typeAxiomsByName = typeAxioms.groupBy(_.entity.name)

    thy.types map { t =>
      val axioms = typedefByTypeName.getOrElse(t.entity.name, Array.empty) flatMap { td =>
        typeAxiomsByName.getOrElse(td.axiomName, Seq.empty) match {
          case Seq(ax) => Some(ax)
          case axs: Any =>
            logger.error(s"Expected exactly one axiom for type ${td.axiomName}, got: ${axs.mkString(",")}")
            None
        }
      }

      TypeEntity(
        thy.name,
        t.entity.startPos,
        t.entity.endPos,
        t.entity.name,
        axioms.map(_.prop.term.toString),
        Array.empty)
    } toSeq
  }
}
