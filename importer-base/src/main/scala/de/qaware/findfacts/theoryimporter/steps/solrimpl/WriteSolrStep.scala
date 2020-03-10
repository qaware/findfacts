package de.qaware.findfacts.theoryimporter.steps.solrimpl

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.CodeblockEt
import de.qaware.findfacts.common.solr.mapper.ToSolrDoc
import de.qaware.findfacts.theoryimporter.ImportError
import de.qaware.findfacts.theoryimporter.TheoryView.Theory
import de.qaware.findfacts.theoryimporter.steps.{ImportStep, StepContext}
import org.apache.solr.client.solrj.{SolrClient, SolrServerException}

/** Step to write entities to solr.
  *
  * @param solrClient connection of the importer
  */
class WriteSolrStep(solrClient: SolrClient) extends ImportStep {

  /** HTTP status ok code */
  final val STATUS_OK = 200

  private val logger = Logger[WriteSolrStep]

  override def apply(theory: Theory)(implicit ctx: StepContext): List[ImportError] = {
    val entities = ctx.blocks

    if (entities.isEmpty) {
      logger.debug(s"Nothing to import")
      return List.empty
    }

    logger.debug(s"Importing ${entities.size} entities to solr...")

    // Add all entities
    Try {
      val mapper = ToSolrDoc[CodeblockEt]

      solrClient.add(entities.map(mapper.toSolrDoc).asJava)

      // Commit, wait for response and check if it is ok
      val res = solrClient.commit()
      if (res.getStatus != 0 && res.getStatus != STATUS_OK) {
        throw new SolrServerException(s"Error ${res.getStatus} while writing to solr")
      }
    } match {
      case Failure(ex) =>
        logger.debug("Exception occurred while writing to solr: ", ex)
        List(ImportError(this, "*", ex.getMessage, ex.getStackTrace.mkString("\n")))
      case Success(_) =>
        logger.debug("Finished importing to solr")
        List.empty
    }
  }
}
