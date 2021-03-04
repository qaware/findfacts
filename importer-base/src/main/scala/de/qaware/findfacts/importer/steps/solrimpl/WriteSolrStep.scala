package de.qaware.findfacts.importer.steps.solrimpl

import scala.jdk.CollectionConverters._

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.dt.BaseEt
import de.qaware.findfacts.common.solr.SolrRepository
import de.qaware.findfacts.common.solr.mapper.ToSolrDoc
import de.qaware.findfacts.importer.ImportError
import de.qaware.findfacts.importer.TheoryView.Theory
import de.qaware.findfacts.importer.steps.solrimpl.WriteSolrStep.STATUS_OK
import de.qaware.findfacts.importer.steps.{ImportStep, StepContext}

/**
 * Step to write entities to solr.
 *
 * @param index to write to
 * @param solr connection of the importer
 */
class WriteSolrStep(index: String, solr: SolrRepository) extends ImportStep {

  private val logger = Logger[WriteSolrStep]

  override def execute(theory: Theory)(implicit ctx: StepContext): List[ImportError] = {
    solr.createIndex(index)
    val entities = ctx.getBlocks

    if (entities.isEmpty) {
      logger.debug(s"Nothing to import")
      List.empty
    } else {
      logger.debug(s"Importing ${entities.size} entities to solr...")

      // Add all entities
      val mapper = ToSolrDoc[BaseEt]

      solr.add(index, entities.map(mapper.toSolrDoc).asJava)

      // Commit, wait for response and check if it is ok
      val res = solr.commit(index)
      if (res.getStatus != 0 && res.getStatus != STATUS_OK) {
        logger.error(s"Error occurred while writing to solr: $res")
        throw new IllegalStateException(s"Error ${res.getStatus} while writing to solr")
      }

      logger.debug("Finished importing to solr")
      List.empty
    }
  }
}

/** Companion object. */
object WriteSolrStep {

  /** HTTP status ok code */
  final val STATUS_OK = 200
}
