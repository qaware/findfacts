package de.qaware.dumpimporter.steps.thyexport

import com.typesafe.scalalogging.Logger
import de.qaware.common.solr.dt.ConstEntity
import de.qaware.dumpimporter.Config
import de.qaware.dumpimporter.dataaccess.RepositoryReader
import de.qaware.dumpimporter.steps.thyexport.EntityWrapper.Theorem
import de.qaware.dumpimporter.steps.{ImportStep, StepContext}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

class LoadThyExportStep(override val config: Config) extends ImportStep {
  private val logger = Logger[LoadThyExportStep]

  override def apply(context: StepContext): Unit = {
    val thyFiles = RepositoryReader(config.dump).readAll("(.*)Example/theory/serialized_thy".r)
    logger.info("Found {} serialized theories", thyFiles.size)

    val theories = thyFiles flatMap { repoEntry =>
      Deserializer.deserialize(repoEntry.file.byteArray) match {
        case Failure(ex) =>
          logger.error(s"Could not deserialize $repoEntry: $ex")
          None
        case Success(value) =>
          Some(value)
      }
    }
    logger.info(s"Successfully deserialized ${theories.size} theories")

    theories foreach { thy =>
      val constByPos = thy.consts.groupBy(c => (c.entity.startPos, c.entity.endPos))
      val thyByPos = thy.thms.groupBy(t => (t.entity.startPos, t.entity.endPos))
      constByPos.keys foreach {pos =>
        val consts = constByPos(pos)
        val thys = thyByPos.getOrElse(pos, Seq())
        println(s"Pos $pos consts: $consts.size thys: $thys.size")
      }
      (thyByPos.keySet -- constByPos.keySet) foreach {pos =>
        val thys = thyByPos(pos)
        println(s"Pos $pos thys: $thys.size")
      }

      val thms: mutable.Set[Theorem] = mutable.Set()
      thms ++= thy.thms
      val consts = thy.consts flatMap {c =>
        thms.filter(_.entity.name == (c.entity.name + "_def")).toSeq match {
          case Seq(defn) =>
            thms.remove(defn)
            Some(ConstEntity(c.entity.id.toString, thy.name, c.entity.startPos, defn.entity.endPos, c.entity.name, c.typ, defn.term, Array()))
          case arr => logger.error("Did not found exactly one definition!")
            None
        }
      }
    }
  }
}
