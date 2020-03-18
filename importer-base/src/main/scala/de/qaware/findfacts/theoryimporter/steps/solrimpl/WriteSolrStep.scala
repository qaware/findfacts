package de.qaware.findfacts.theoryimporter.steps.solrimpl

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.BaseEt
import de.qaware.findfacts.common.solr.SolrRepository
import de.qaware.findfacts.common.solr.mapper.ToSolrDoc
import de.qaware.findfacts.theoryimporter.ImportError
import de.qaware.findfacts.theoryimporter.TheoryView.Theory
import de.qaware.findfacts.theoryimporter.steps.{ImportStep, StepContext}

import scala.collection.JavaConverters._

/** Step to write entities to solr.
  * @param index to write to
  * @param solr connection of the importer
  */
class WriteSolrStep(index: String, solr: SolrRepository) extends ImportStep {

  /** HTTP status ok code */
  final val StatusOk = 200

  private val logger = Logger[WriteSolrStep]

  override def apply(theory: Theory)(implicit ctx: StepContext): List[ImportError] = {
    solr.createIndex(index)
    val entities = ctx.blocks

    if (entities.isEmpty) {
      logger.debug(s"Nothing to import")
      List.empty
    } else {
      logger.debug(s"Importing ${entities.size} entities to solr...")

      // Add all entities
      val mapper = ToSolrDoc[BaseEt]

      solr.solrConnection.add(index, entities.map(mapper.toSolrDoc).asJava)

      // Commit, wait for response and check if it is ok
      val res = solr.solrConnection.commit(index)
      if (res.getStatus != 0 && res.getStatus != StatusOk) {
        logger.error(s"Error occurred while writing to solr: $res")
        throw new IllegalStateException(s"Error ${res.getStatus} while writing to solr")
      }

      logger.debug("Finished importing to solr")
      List.empty
    }
  }
}
