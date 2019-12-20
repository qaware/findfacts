package de.qaware.findfacts.theoryimporter.solrimpl

import com.softwaremill.macwire.wire
import de.qaware.findfacts.common.solr.SolrRepository
import de.qaware.findfacts.theoryimporter.ImporterModule
import de.qaware.findfacts.theoryimporter.steps.ImportStep
import de.qaware.findfacts.theoryimporter.steps.solrimpl.WriteSolrStep

/** Solr impl of the importer module. */
trait SolrImporterModule extends ImporterModule {

  /** Solr repository has to be provided. */
  def repository: SolrRepository

  override val indexWriterStep: ImportStep = wire[WriteSolrStep]
}
