package de.qaware.findfacts.dumpimporter.steps

import scala.collection.JavaConverters._

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.solr.{LocalSolr, SolrRepository}

/** Step to write entities to solr.
  *
  * @param solrRepository connection of the importer
  */
class WriteSolrStep(solrRepository: SolrRepository) extends ImportStep {

  /** HTTP status ok code */
  final val STATUS_OK = 200

  private val logger = Logger[WriteSolrStep]

  if (solrRepository.getClass != classOf[LocalSolr]) {
    // Check if non-embedded solr instance is available to fail early if it is not
    logger.info(s"Connection to solr at ${solrRepository} successful")
  }

  override def apply(implicit ctx: StepContext): Unit = {
    val entities = ctx.allEntities

    logger.info(s"Writing ${entities.size} entities to solr...")
    // Add all entities
    val solr = solrRepository.solrConnection()
    solr.addBeans(entities.iterator.asJava)
    // Commit, wait for response and check if it is ok
    val res = solr.commit()

    if (res.getStatus != 0 && res.getStatus != STATUS_OK) {
      logger.error(s"Update request failed: $res")
      throw new IllegalStateException("Could not write to solr")
    } else {
      logger.info("Finished writing to solr")
    }

    // Finally close solr connection
    solr.close()
  }
}
