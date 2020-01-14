package de.qaware.findfacts.theoryimporter.steps.solrimpl

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.BaseEt
import de.qaware.findfacts.common.solr.{SolrRepository, ToSolrDoc}
import de.qaware.findfacts.theoryimporter.TheoryView.Theory
import de.qaware.findfacts.theoryimporter.steps.{ImportError, ImportStep, StepContext}
import org.apache.solr.client.solrj.SolrServerException

/** Step to write entities to solr.
  *
  * @param solrRepository connection of the importer
  */
class WriteSolrStep(solrRepository: SolrRepository) extends ImportStep {

  /** HTTP status ok code */
  final val STATUS_OK = 200

  private val logger = Logger[WriteSolrStep]

  override def apply(theories: Seq[Theory])(implicit ctx: StepContext): List[ImportError] = {
    val entities = ctx.blocks ++ ctx.docs

    if (entities.isEmpty) {
      return List.empty
    }

    logger.info(s"Writing ${entities.size} entities to solr...")

    // Add all entities
    Try {
      val solr = solrRepository.solrConnection()

      val mapper = ToSolrDoc[BaseEt]

      solr.add(entities.map(mapper.toSolrDoc).asJava)

      // Commit, wait for response and check if it is ok
      val res = solr.commit()
      if (res.getStatus != 0 && res.getStatus != STATUS_OK) {
        throw new SolrServerException(s"Error ${res.getStatus} while writing to solr")
      }
    } match {
      case Failure(ex) =>
        logger.error("Exception occurred while writing to solr: ", ex)
        List(ImportError(this, "*", ex.getMessage, ex.getStackTrace.mkString("\n")))
      case Success(res) =>
        logger.info("Finished writing to solr")
        List.empty
    }
  }
}
