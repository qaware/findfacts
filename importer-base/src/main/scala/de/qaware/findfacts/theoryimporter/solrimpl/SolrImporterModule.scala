package de.qaware.findfacts.theoryimporter.solrimpl

import com.softwaremill.macwire.wire
import de.qaware.findfacts.common.solr.SolrRepository
import de.qaware.findfacts.theoryimporter.{ImportError, ImporterModule, TheoryView}
import de.qaware.findfacts.theoryimporter.steps.solrimpl.WriteSolrStep

/** Solr impl of the importer module. */
abstract class SolrImporterModule(index: String) extends ImporterModule {
  solr.createIndex(index)

  /** Solr client has to be provided.
    *
    * @return the provided solr client
    */
  def solr: SolrRepository

  override lazy val indexWriterStep: WriteSolrStep = wire[WriteSolrStep]

  override def importSession(theories: Seq[TheoryView.Theory]): List[ImportError] = {
    super.importSession(theories)
  }
}
