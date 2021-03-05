/*  Title:      findfacts/build_importer.scala
    Author:     Fabian Huch, TU Munich/QAware GmbH

Isabelle dump importer.
 */

package de.tum.in.isabelle.search.importer

import de.qaware.findfacts.common.solr.{LocalSolr, RemoteSolr}
import de.qaware.findfacts.importer.Importer
import de.qaware.findfacts.importer.solrimpl.SolrImporterModule
import isabelle.Build.build
import isabelle.Export.Provider
import isabelle._

object Build_Importer {

  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool(
    "build_importer",
    "Import build db into solr",
    Scala_Project.here,
    args => {
      /* arguments */

      var index_name = "theorydata"
      var configset = Isabelle_System.getenv("SOLR_CONFIGSET")
      var local_solr = ""
      var remote_solr: List[String] = Nil

      val getopts = Getopts(
        """
Usage: isabelle build_importer [OPTIONS] SESSIONS...

  Options are:
    -i NAME         index NAME to import into
    -C NAME         Solr configset NAME
    -l SOLRDIR      local Solr repository at SOLRDIR
    -r HOST:PORT    remote Solr connection at HOST:PORT

  Import Isabelle dump from DUMPDIR into Solr db. Only one Solr connection
  may be used. For remote connections, a configset must be set (either via
  argument or environment variable 'SOLR_CONFIGSET').
  Index name usually has form '${NAME}_${ISABELLE_VERSION}_${AFP_VERSION}'.
""",
        "i:" -> (arg => index_name = arg),
        "C:" -> (arg => configset = arg),
        "l:" -> (arg => local_solr = arg),
        "r:" -> (arg => remote_solr = Library.distinct(space_explode(':', arg)))
      )

      val sessions = getopts(args)

      val solr_repository = (local_solr, remote_solr) match {
        case (dir, _) if !dir.isBlank => LocalSolr(Path.explode(dir).absolute_file)
        case (_, host :: port :: Nil) if !host.isBlank && !configset.isBlank =>
          RemoteSolr(host, Value.Int.parse(port), configset)
        case _ => getopts.usage()
      }

      using(solr_repository) { solr_repository =>
        val importer_module = new SolrImporterModule(solr_repository)

        val progress = new Console_Progress()
        val options = Options.init()

        // Build
        build(
          options,
          progress = progress,
          selection = Sessions.Selection(sessions = sessions),
          export_files = true
        )

        // Import
        sessions foreach { session_name =>
          val store = Sessions.store(options)

          using(store.open_database(session_name, output = false)) { db =>
            val provider = Provider.database(db, XML.Cache.make(), session_name, "dummy")
            val theories = store.read_theories(db, session_name)
            Importer.import_session(index_name, provider, session_name, theories, importer_module, progress)
          }
        }
      }
    }
  )
}
