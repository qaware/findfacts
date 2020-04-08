package de.qaware.findfacts.importer.solrimpl

import com.typesafe.scalalogging.Logger
import de.qaware.findfacts.common.solr.SolrRepository
import de.qaware.findfacts.importer.steps.StepContext
import de.qaware.findfacts.importer.steps.solrimpl.WriteSolrStep
import de.qaware.findfacts.importer.{ImportError, ImporterModule, TheoryView}

/** Solr impl of the importer module.
  *
  * @param solr repository has to be provided
  */
class SolrImporterModule(solr: SolrRepository) extends ImporterModule {
  private val logger = Logger[SolrImporterModule]

  override def importSession(index: String, theories: Seq[TheoryView.Theory]): List[ImportError] = {
    logger.info("Starting import...")

    val writeSolrStep = new WriteSolrStep(index, solr)

    if (solr.createIndex(index)) {
      logger.info(s"Created index $index")
    }

    val errors = theories flatMap { theory =>
      logger.info(s"Processing theory ${theory.name}")

      implicit val ctx: StepContext = StepContext()
      val errors = (steps :+ writeSolrStep).flatMap(_(theory))

      errors foreach { error =>
        logger.warn(s"Error during import: $error")
        logger.debug(s"Details: ${error.getDebugInfo}")
      }
      errors
    }

    logger.info(s"Finished import with ${errors.size} errors.")
    errors.toList
  }
}
