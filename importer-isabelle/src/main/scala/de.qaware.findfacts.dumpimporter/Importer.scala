package de.qaware.findfacts.dumpimporter

import isabelle.Isabelle_Tool

object Importer
{
  val isabelle_tool =
    Isabelle_Tool("dump_importer", "Import dump into solr", args => {
    throw new NotImplementedError("hi!")
  })
}
