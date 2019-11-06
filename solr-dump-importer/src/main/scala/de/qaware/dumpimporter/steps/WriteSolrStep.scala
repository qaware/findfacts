package de.qaware.dumpimporter.steps

import com.typesafe.scalalogging.Logger
import de.qaware.dumpimporter.Config

import scala.collection.JavaConverters._

/** Step to write entities to solr. */
class WriteSolrStep(override val config: Config) extends ImportStep {

  /** HTTP status ok code */
  final val STATUS_OK = 200

  private val logger = Logger[WriteSolrStep]

  override def apply(context: StepContext): Unit = {
    val res = try {
      val solr = config.solr.get.solrConnection()
      solr.addBeans(context.consts.iterator.asJava)
      solr.addBeans(context.facts.iterator.asJava)
      solr.commit()
    } catch {
      case e: Exception =>
        logger.error("Exception occurred while writing to solr: ", e)
        throw new IllegalStateException("Could not write to solr")
    }
    if (res.getStatus != 0 && res.getStatus != STATUS_OK) {
      logger.error(s"Update request failed: $res")
      throw new IllegalStateException("Could not write to solr")
    }
  }
}
