package de.qaware.findfacts

import de.qaware.findfacts.Theory._
import de.qaware.findfacts.common.solr.{LocalSolr, RemoteSolr, SolrRepository}
import de.qaware.findfacts.theoryimporter.solrimpl.SolrImporterModule
import de.qaware.findfacts.theoryimporter.{ImporterModule, TheoryView}
import isabelle.{Console_Progress, Export, Export_Theory, File, Getopts, Isabelle_Tool, No_Progress, Path, Progress, Url}

object Importer
{

  /* import a session to solr */

  def solr_import_session(
    provider: Export.Provider,
    session_name: String,
    theory_names: List[String],
    solr: SolrRepository,
    progress: Progress = No_Progress)
  {
    val importer = new SolrImporterModule {
      override def repository: SolrRepository = solr
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
    val theories = theory_names map { theory_name =>
      provider.focus(theory_name)

      val isabelle_theory = Export_Theory.read_theory(provider, session_name, theory_name)

      // Create accessor for importer

      new TheoryView.Theory
      {
        override val name: String = theory_name
        override val session: String = session_name
        override def types: List[TheoryView.Type] = isabelle_theory.types.map(Type_Wrapper)
        override def consts: List[TheoryView.Const] = isabelle_theory.consts.map(Const_Wrapper)
        override def axioms: List[TheoryView.Axiom] = isabelle_theory.axioms.map(Axiom_Wrapper)
        override def thms: List[TheoryView.Thm] = isabelle_theory.thms.map(Thm_Wrapper)
        override def constdefs: List[TheoryView.Constdef] = isabelle_theory.constdefs.map(Constdef_Wrapper)
        override def typedefs: List[TheoryView.Typedef] = isabelle_theory.typedefs.map(Typedef_Wrapper)
      }
    }

    importer.importSession(theories)

    progress.echo("imported " + provider)
  }

  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("dump_importer", "Import dump into solr", args =>
  {
    /* arguments */

    var export_db: List[SolrRepository] = Nil
    var sessions: List[String] = Nil

    val getopts = Getopts("""
Usage: isabelle dump_importer [OPTIONS] DUMPDIR

  Options are:
    -l DIR       directory for local solr to write in
    -e URL       http url for external solr
    -B NAME      import session NAME

  Import isabelle dump from DUMPDIR into solr db.
  Only one solr connection may be specified.
""",
      "l:" -> (arg => export_db = export_db ::: List(LocalSolr(better.files.File(arg)))),
      "e:" -> (arg => export_db = export_db ::: List(RemoteSolr(Url(arg)))),
      "B:" -> (arg => sessions = sessions ::: List(arg)))

    val dump_dir = getopts(args) match {
      case List(dump) => Path.explode(dump)
      case _ => getopts.usage()
    }

    val importer_module = export_db match {
      case List(solr) => new SolrImporterModule { override def repository: SolrRepository = solr }
      case _ => getopts.usage()
    }

    val progress = new Console_Progress()

    // find all sessions written by 'isabelle dump'

    val dump_theory_dirs = File.read_dir(dump_dir)

    if (sessions.isEmpty) {
      sessions = dump_theory_dirs.map(_.split('.').head).distinct
    }

    // run import

    sessions foreach { session =>
      val theory_dirs = dump_theory_dirs.filter(dir => dir == session || dir.startsWith(session + "."))
      val theory_names = theory_dirs map { theory_dir =>
        if (theory_dir.length > session.length) theory_dir else session + "." + session
      }
      val provider = Export.Provider.directory(dump_dir, session, theory_names.head)

      import_session(provider, session, theory_names, importer_module, progress)
    }
  })
}
