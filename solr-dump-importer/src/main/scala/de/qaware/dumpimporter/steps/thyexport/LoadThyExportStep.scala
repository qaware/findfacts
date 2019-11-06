package de.qaware.dumpimporter.steps.thyexport

import com.typesafe.scalalogging.Logger
import de.qaware.common.solr.dt.{ConstEntity, FactEntity}
import de.qaware.dumpimporter.Config
import de.qaware.dumpimporter.dataaccess.RepositoryReader
import de.qaware.dumpimporter.steps.{ImportStep, StepContext}

import scala.language.postfixOps
import scala.util.{Failure, Success}

class LoadThyExportStep(override val config: Config) extends ImportStep {
  private val logger = Logger[LoadThyExportStep]

  override def apply(context: StepContext): Unit = {
    val thyFiles = RepositoryReader(config.dump).readAll("(.*)theory/serialized_thy".r)
    logger.info("Found {} serialized theories", thyFiles.size)

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
      val (constAxioms, axioms) = thy.axioms.partition(ax => constAxNames.contains(ax.entity.name))

      context.consts ++= extractConsts(thy, constAxioms.toSeq)
      context.facts ++= extractFacts(thy, axioms.toSeq)
    }
  }

  protected def extractConsts(thy: Theory, constAxioms: Seq[Axiom]): Seq[ConstEntity] = {
    // Find related axioms, if any
    val axNameByConstName = thy.constdefs.groupBy(_.name).mapValues(_.map(_.axiomName))
    val constAxiomsByName = constAxioms.groupBy(_.entity.name)

    thy.consts flatMap { const =>
      val axioms = axNameByConstName.getOrElse(const.entity.name, Array.empty) flatMap { axName =>
        constAxiomsByName.getOrElse(axName, Seq.empty) match {
          case Seq(ax) => Some(ax)
          case axs =>
            logger.error(s"Expected exactly one axiom for name $axName, got: ${axs.mkString(",")}")
            None
        }
      }

      if (axioms.isEmpty) {
        logger.whenDebugEnabled(logger.debug(s"No constant definition for ${const.entity.name}"))
      }

      // Enrich const data with axiom def
      Some(
        ConstEntity(
          const.entity.serial.toString,
          thy.name,
          const.entity.startPos,
          const.entity.endPos,
          const.entity.name,
          const.typ,
          axioms.map(_.prop.term),
          Array()
        ))
    }
  } toSeq

  protected def extractFacts(thy: Theory, axioms: Seq[Axiom]): Seq[FactEntity] = {
    (thy.thms map { thm =>
      FactEntity(thm.entity.serial.toString, thy.name, thm.entity.startPos, thm.entity.endPos, thm.prop.term, Array())
    } toSeq) ++ (axioms map { ax =>
      FactEntity(ax.entity.serial.toString, thy.name, ax.entity.startPos, ax.entity.endPos, ax.prop.term, Array())
    })
  }
}
