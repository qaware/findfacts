/*  Title:      findfacts/importer.scala
    Author:     Fabian Huch, TU Munich/QAware GmbH

Isabelle dump importer.
 */

package de.qaware.findfacts.importer

import de.qaware.findfacts.common.solr.{LocalSolr, RemoteSolr, SolrRepository}
import de.qaware.findfacts.importer.Theory._
import de.qaware.findfacts.importer.solrimpl.SolrImporterModule
import isabelle._

object Importer {

  /* import a session to solr */

  def solr_import(
      index_name: String,
      provider: Export.Provider,
      session_name: String,
      theory_names: List[String],
      solr_repository: SolrRepository,
      progress: Progress = new Progress): Unit = {
    val importer = new SolrImporterModule(solr_repository)
    import_session(index_name, provider, session_name, theory_names, importer, progress)
  }

  /* import a session with a generic importer */

  def import_session(
      index_name: String,
      provider: Export.Provider,
      session_name: String,
      theory_names: List[String],
      importer: ImporterModule,
      progress: Progress = new Progress): Unit = {
    progress.echo("importing " + session_name + " with " + theory_names.size + " theories...")

    val theories = theory_names map { theory_name =>
      val theory_provider = provider.focus(theory_name)

      val isabelle_theory = Export_Theory.read_theory(theory_provider, session_name, theory_name)

      val markup_xml = theory_provider.uncompressed_yxml("markup.yxml")
      val markup_blocks = Markup_Blocks.from_XML(markup_xml)

      // Create accessor for importer

      map_theory(session_name, isabelle_theory, markup_blocks)
    }

    val errors = importer.importSession(index_name, theories)

    errors foreach { error =>
      val message = session_name + ": " + error.step.getClass + ": " + error.causeEntity + ": " + error.errorMsg
      progress.echo_error_message(message)
    }

    if (errors.isEmpty) {
      progress.echo("finished importing " + session_name)
    } else {
      progress.echo("finished importing " + session_name + " with " + errors.size + " errors.")
    }
  }

  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool(
    "dump_importer",
    "Import dump into solr",
    Scala_Project.here,
    args => {
      /* arguments */

      var index_name = "theorydata"
      var all_sessions = false
      var configset = Isabelle_System.getenv("SOLR_CONFIGSET")
      var local_solr = ""
      var remote_solr: List[String] = Nil

      val getopts = Getopts(
        """
Usage: isabelle dump_importer [OPTIONS] [SESSIONS ...] DUMPDIR

  Options are:
    -a              select all dumped sessions
    -i NAME         index NAME to import into
    -C NAME         Solr configset NAME
    -l SOLRDIR      local Solr repository at SOLRDIR
    -r HOST:PORT    remote Solr connection at HOST:PORT

  Import Isabelle dump from DUMPDIR into Solr db. Only one Solr connection
  may be used. For remote connections, a configset must be set (either via
  argument or environment variable 'SOLR_CONFIGSET').
  Index name usually has form '${NAME}_${ISABELLE_VERSION}_${AFP_VERSION}'.
""",
        "a" -> (_ => all_sessions = true),
        "i:" -> (arg => index_name = arg),
        "C:" -> (arg => configset = arg),
        "l:" -> (arg => local_solr = arg),
        "r:" -> (arg => remote_solr = Library.distinct(space_explode(':', arg)))
      )

      val more_args = getopts(args)

      val dump_dir = more_args match {
        case Nil => getopts.usage()
        case opts => Path.explode(opts.last)
      }
      val dump_theory_dirs = File.read_dir(dump_dir)

      val sessions = (more_args.dropRight(1), all_sessions) match {
        case (Nil, true) => dump_theory_dirs.map(_.split('.').head).distinct
        case (session_list, false) => session_list
        case _ => getopts.usage()
      }

      val solr_repository = (local_solr, remote_solr) match {
        case (dir, _) if !dir.isBlank => LocalSolr(Path.explode(dir).absolute_file)
        case (_, host :: port :: Nil) if !host.isBlank && !configset.isBlank =>
          RemoteSolr(host, Value.Int.parse(port), configset)
        case _ => getopts.usage()
      }

      using(solr_repository) { solr_repository =>
        val importer_module = new SolrImporterModule(solr_repository)

        val progress = new Console_Progress()

        // run import

        sessions map { session =>
          Future.fork {
            val theory_dirs = dump_theory_dirs.filter(dir => dir == session || dir.startsWith(session + "."))
            val theory_names = theory_dirs map { theory_dir =>
              if (theory_dir.length > session.length) theory_dir else session
            }
            val provider = Export.Provider.directory(dump_dir, XML.Cache.make(), session, "dummy")

            import_session(index_name, provider, session, theory_names, importer_module, progress)
          }
        } foreach (_.join)
      }
    }
  )
}
