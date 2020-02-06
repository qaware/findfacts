/*  Title:      findfacts/importer.scala
    Author:     Fabian Huch, TU Munich/QAware GmbH

Isabelle dump importer.
*/

package de.qaware.findfacts


import de.qaware.findfacts.Theory._
import de.qaware.findfacts.common.solr.RemoteSolr
import de.qaware.findfacts.theoryimporter.ImporterModule
import de.qaware.findfacts.theoryimporter.solrimpl.SolrImporterModule
import org.apache.solr.client.solrj.SolrClient
import isabelle._


object Importer
{

  /* import a session to solr */

  def solr_import(
    provider: Export.Provider,
    session_name: String,
    theory_names: List[String],
    solr: SolrClient,
    progress: Progress = No_Progress)
  {
    val importer = new SolrImporterModule {
      override def solrClient: SolrClient = solr
    }
    import_session(provider, session_name, theory_names, importer, progress)
  }

  /* import a session with a generic importer */

  def import_session(
    provider: Export.Provider,
    session_name: String,
    theory_names: List[String],
    importer: ImporterModule,
    progress: Progress = No_Progress)
  {
    progress.echo("importing " + session_name + " with " + theory_names.size + " theories...")

    val theories = theory_names map { theory_name =>
      val theory_provider = provider.focus(theory_name)

      val isabelle_theory = Export_Theory.read_theory(theory_provider, session_name, theory_name)

      val markup_xml = theory_provider.uncompressed_yxml("markup.yxml")
      val markup_blocks = Markup_Blocks.from_XML(markup_xml)

      // Create accessor for importer

      map_theory(session_name, isabelle_theory, markup_blocks)
    }

    val errors = importer.importSession(theories)

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

  val isabelle_tool = Isabelle_Tool("dump_importer", "Import dump into solr", args =>
  {
    /* arguments */

    var solr_core = "theorydata"
    var sessions: List[String] = Nil

    val getopts = Getopts("""
Usage: isabelle dump_importer [OPTIONS] DUMPDIR HOST PORT

  Options are:
    -B NAME       import session NAME
    -C NAME       sorl core NAME

  Import isabelle dump from DUMPDIR into solr db.
  Only one solr connection may be specified.
""",
      "B:" -> (arg => sessions = sessions ::: List(arg)),
      "C:" -> (arg => solr_core = arg))

    val more_args = getopts(args)

    val (dump_dir, solr_repository) = more_args match {
      case dump :: host :: port :: Nil => (Path.explode(dump), RemoteSolr(host,  Value.Int.parse(port), solr_core))
      case _ => getopts.usage
    }

    using(solr_repository.solrConnection()) { solr =>
      val importer_module = new SolrImporterModule { override def solrClient: SolrClient = solr }

      val progress = new Console_Progress()

      // find all sessions written by 'isabelle dump'

      val dump_theory_dirs = File.read_dir(dump_dir)

      if (sessions.isEmpty) {
        sessions = dump_theory_dirs.map(_.split('.').head).distinct
      }

      // run import

      sessions map { session =>
        Future.fork {
          val theory_dirs = dump_theory_dirs.filter(dir => dir == session || dir.startsWith(session + "."))
          val theory_names = theory_dirs map { theory_dir =>
            if (theory_dir.length > session.length) theory_dir else session
          }
          val provider = Export.Provider.directory(dump_dir, session, "dummy")

          import_session(provider, session, theory_names, importer_module, progress)
        }
      } foreach (_.join)
    }
  })
}
