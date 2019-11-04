import java.io.{ByteArrayOutputStream, ObjectOutputStream}

import scala.collection.immutable

import isabelle.{Getopts, Path, Export_Theory, Sessions, Options, Console_Progress, Library, Dump, Bytes, Properties}
import isabelle.Export.{Entry, Provider}
import isabelle.Library.space_explode
import isabelle.Dump.Aspect

/* Theory content interface needs to be wrapped in order to be serializable by JIT compiler. */

object SerializableWrapper extends Serializable
{
  /* Simplification of:
  sealed abstract class Typ
  case class Type(name: String, args: List[Typ] = Nil) extends Typ
  case class TFree(name: String, sort: Sort = Nil) extends Typ
  case class TVar(name: Indexname, sort: Sort = Nil) extends Typ */

  type Typ = String

  /* Simplification of:
  sealed abstract class Term
  case class Const(name: String, typargs: List[Typ] = Nil) extends Term
  case class Free(name: String, typ: Typ) extends Term
  case class Var(name: Indexname, typ: Typ) extends Term
  case class Bound(index: Int) extends Term
  case class Abs(name: String, typ: Typ, body: Term) extends Term
  case class App(fun: Term, arg: Term) extends Term */

  type Term = String

  /* Simplification of:
  sealed abstract class Proof
  case object MinProof extends Proof
  case class PBound(index: Int) extends Proof
  case class Abst(name: String, typ: Typ, body: Proof) extends Proof
  case class AbsP(name: String, hyp: Term, body: Proof) extends Proof
  case class Appt(fun: Proof, arg: Term) extends Proof
  case class AppP(fun: Proof, arg: Proof) extends Proof
  case class Hyp(hyp: Term) extends Proof
  case class PAxm(name: String, types: List[Typ]) extends Proof
  case class OfClass(typ: Typ, cls: Class) extends Proof
  case class Oracle(name: String, prop: Term, types: List[Typ]) extends Proof
  case class PThm(serial: Long, theory_name: String, name: String, types: List[Typ]) extends Proof */

  type Proof = String

  @SerialVersionUID(-6600248326866346277L)
  sealed case class Entity(name: String, id: Long, startPos: Int, endPos: Int) extends Serializable

  @SerialVersionUID(8214256252769140008L)
  sealed case class Constant(entity: Entity, typargs: immutable.Vector[String], typ: Typ) extends Serializable

  @SerialVersionUID(4245087039579397970L)
  sealed case class Theorem(
     entity: Entity,
     typargs: immutable.Vector[String],
     args: immutable.Vector[(String, Typ)],
     term: Term,
     deps: immutable.Vector[String],
     proof: Proof) extends Serializable

  @SerialVersionUID(-7187211222156107352L)
  sealed case class Theory(
    name: String,
    consts: immutable.Vector[Constant],
    thms: immutable.Vector[Theorem]) extends Serializable

}

/* Import wrapper content to top level. Wrapper is only necessary because of jit compiler bug/missing feature. */

import SerializableWrapper._

/* Serializes theory entries into stable interface elements */

object MappingSerializer
{
  private def mapEntity(e: Export_Theory.Entity) =
  {
    if (!Properties.defined(e.pos, "offset") || !Properties.defined(e.pos, "end_offset")) {
      throw new IllegalArgumentException("Offsets need to be defined for entity " + e)
    }

    val start = Properties.get(e.pos, "offset").get.toInt
    val end = Properties.get(e.pos, "end_offset").get.toInt

    Entity(e.xname, e.serial, start, end)
  }

  def serialize_theory(theory: Export_Theory.Theory) : Bytes = {
    // Map to stable elements
    val consts = theory.consts map {const =>
      Constant(mapEntity(const.entity), const.typargs.toVector, const.typ.toString)
    } toVector
    val thms = theory.thms map {thm =>
      Theorem(mapEntity(thm.entity),
        thm.prop.typargs.map(_._1).toVector,
        thm.prop.args map { t => (t._1, t._2.toString) } toVector,
        thm.prop.term.toString,
        thm.deps.toVector,
        thm.proof.toString)
    } toVector
    val thy = Theory(theory.name, consts, thms)

    // Serialize entities
    val byteStream = new ByteArrayOutputStream()
    val objectStream = new ObjectOutputStream(byteStream)

    objectStream.writeObject(thy)
    objectStream.close()
    byteStream.close()

    Bytes(byteStream.toByteArray)
  }
}

/* Aspect wrapper for stable interface */

val serialize_theory_aspect = Aspect("theory", "foundational theory content (stable entities)",
  { case args =>
    val name = args.snapshot.node_name.toString
    val snapshot_provider = Provider.snapshot(args.snapshot)
    val theory = Export_Theory.read_theory(snapshot_provider, name, name)
    args.write(Path.explode("theory/serialized_thy"), MappingSerializer.serialize_theory(theory))
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

val getopts = Getopts("""
Usage: isabelle scala dump_stable.scala [OPTIONS] [SESSIONS ...]

  Options are:
    -A NAMES     dump named aspects, in addition to stable theory serialization (default: none)
    -B NAME      include session NAME and all descendants
    -D DIR       include session directory and select its sessions
    -O DIR       output directory for dumped files (default: """ + Dump.default_output_dir + """)
    -R           operate on requirements of selected sessions
    -X NAME      exclude sessions from group NAME and all descendants
    -a           select all sessions
    -b NAME      base logic image (default """ + isabelle.quote(Dump.default_logic) + """)
    -d DIR       include session directory
    -g NAME      select session group NAME
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)
    -v           verbose
    -x NAME      exclude session NAME and all descendants

  Dump cumulative PIDE session database, with the following aspects:

""" + Library.prefix_lines("    ", Dump.show_aspects) + "\n",
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
