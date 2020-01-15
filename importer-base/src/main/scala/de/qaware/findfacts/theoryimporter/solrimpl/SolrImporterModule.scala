package de.qaware.findfacts.theoryimporter.solrimpl

import com.softwaremill.macwire.wire
import de.qaware.findfacts.theoryimporter.ImporterModule
import de.qaware.findfacts.theoryimporter.steps.ImportStep
import de.qaware.findfacts.theoryimporter.steps.solrimpl.WriteSolrStep
import org.apache.solr.client.solrj.SolrClient

/** Solr impl of the importer module. */
trait SolrImporterModule extends ImporterModule {

  /** Solr client has to be provided. */
  def solrClient: SolrClient

  override val indexWriterStep: ImportStep = wire[WriteSolrStep]
}
