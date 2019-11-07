import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

import isabelle.{Getopts, Path, Export_Theory, Sessions, Options, Console_Progress, Library, Dump, Bytes, Properties}
import isabelle.Export.{Entry, Provider}
import isabelle.Library.space_explode
import isabelle.Dump.Aspect

val tb = currentMirror.mkToolBox()
val entities_src = io.Source.fromFile("./isabelle-extensions/stable_entities.scala") // TODO
val serializer_src = io.Source.fromFile("./isabelle-extensions/dump_serializer.scala")
val entity_wrapper = tb.define(tb.parse(entities_src.mkString).asInstanceOf[tb.u.ImplDef])
val serialize_wrapper = tb.parse(serializer_src.mkString)
entities_src.close()
serializer_src.close()

val serializer = tb.define(q"""
object SerializerWrapper {
  import $entity_wrapper._
  $serialize_wrapper
}
""")
val serialize = tb.eval(q"""{ theory => $serializer.MappingSerializer.serialize_theory(theory) }""")
  .asInstanceOf[Export_Theory.Theory => Bytes]

/* Aspect wrapper for stable interface */

val serialize_theory_aspect = Aspect("theory", "foundational theory content (stable entities)",
  { case args =>
    val name = args.snapshot.node_name.toString
    val snapshot_provider = Provider.snapshot(args.snapshot)
    val theory = Export_Theory.read_theory(snapshot_provider, name, name)
    args.write(Path.explode("theory/serialized_thy"), serialize(theory))
  }, options = List("export_theory"))

/* CLI for the stable interface aspect. (Mostly) copied from src/Pure/Tools/dump.scala. */

var aspects: List[Aspect] = Nil
var base_sessions: List[String] = Nil
var select_dirs: List[Path] = Nil
var output_dir = Dump.default_output_dir
var requirements = false
var exclude_session_groups: List[String] = Nil
var all_sessions = false
var logic = Dump.default_logic
var dirs: List[Path] = Nil
var session_groups: List[String] = Nil
var options = Options.init()
var verbose = false
var exclude_sessions: List[String] = Nil

val getopts = Getopts("" + Library.prefix_lines("    ", Dump.show_aspects) + "\n",
  "A:" -> (arg => aspects = Library.distinct(Library.space_explode(',', arg)).map(Dump.the_aspect(_))),
  "B:" -> (arg => base_sessions = base_sessions ::: List(arg)),
  "D:" -> (arg => select_dirs = select_dirs ::: List(Path.explode(arg))),
  "O:" -> (arg => output_dir = Path.explode(arg)),
  "R" -> (_ => requirements = true),
  "X:" -> (arg => exclude_session_groups = exclude_session_groups ::: List(arg)),
  "a" -> (_ => all_sessions = true),
  "b:" -> (arg => logic = arg),
  "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
  "g:" -> (arg => session_groups = session_groups ::: List(arg)),
  "o:" -> (arg => options = options + arg),
  "v" -> (_ => verbose = true),
  "x:" -> (arg => exclude_sessions = exclude_sessions ::: List(arg)))

val sessions = getopts(args)

val progress = new Console_Progress(verbose = verbose)

progress.interrupt_handler {
  Dump.dump(options, logic,
    aspects = aspects :+ serialize_theory_aspect,
    progress = progress,
    dirs = dirs,
    select_dirs = select_dirs,
    output_dir = output_dir,
    selection = Sessions.Selection(
      requirements = requirements,
      all_sessions = all_sessions,
      base_sessions = base_sessions,
      exclude_session_groups = exclude_session_groups,
      exclude_sessions = exclude_sessions,
      session_groups = session_groups,
      sessions = sessions))
}
