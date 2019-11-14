package de.qaware.dumpimporter.steps

import scala.collection.JavaConverters._

import com.typesafe.scalalogging.Logger
import de.qaware.dumpimporter.Config
import de.qaware.scalautils.Using

/** Step to write entities to solr.
  *
  * @param config of the importer
  */
class WriteSolrStep(override val config: Config) extends ImportStep {

  /** HTTP status ok code */
  final val STATUS_OK = 200

  private val logger = Logger[WriteSolrStep]

  override def apply(ctx: StepContext): Unit = {
    logger.info(s"Writing ${ctx.facts.size + ctx.types.size + ctx.consts.size + ctx.doc.size} entities to solr...")
    Using.resource(config.solr.solrConnection()) { solr =>
      // Add all entities
      solr.addBeans(ctx.consts.iterator.asJava)
      solr.addBeans(ctx.facts.iterator.asJava)
      solr.addBeans(ctx.types.iterator.asJava)
      solr.addBeans(ctx.doc.iterator.asJava)
      // Commit, wait for response and check if it is ok
      val res = solr.commit()

      if (res.getStatus != 0 && res.getStatus != STATUS_OK) {
        logger.error(s"Update request failed: $res")
        throw new IllegalStateException("Could not write to solr")
      } else {
        logger.info("Finished writing to solr")
      }
    }
  }
}
