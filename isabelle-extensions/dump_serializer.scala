object MappingSerializer
{
  /* Imports need to be inlined for reflective compilation */
  import java.io.{ByteArrayOutputStream, ObjectOutputStream}

  import isabelle.{Export_Theory, Bytes, Properties, Term}

  def map_entity(e: Export_Theory.Entity) = {
    if (!Properties.defined(e.pos, "offset") || !Properties.defined(e.pos, "end_offset")) {
      throw new IllegalArgumentException("Offsets need to be defined for entity " + e)
    }

    val start = Properties.get(e.pos, "offset").get.toInt
    val end = Properties.get(e.pos, "end_offset").get.toInt

    Entity(e.name, e.xname, e.serial, start, end)
  }

  def map_indexname(in: Term.Indexname) = Indexname(in.name, in.index)

  def map_typ(t: Term.Typ): Typ = {
    t match {
      case Term.Type(name, args) => TProd(name, args.map(map_typ).toArray)
      case Term.TFree(name, sort) => TFree(name, sort.toArray)
      case Term.TVar(name, sort) => TVar(map_indexname(name), sort.toArray)
    }
  }

  def map_prop(p: Export_Theory.Prop) = Prop(
    p.typargs map { case (name, sort) => (name, sort.toArray) } toArray,
    p.args map { case (name, typ) => (name, map_typ(typ)) } toArray,
    map_term(p.term)
  )

  def map_const(c: Export_Theory.Const) = Const(map_entity(c.entity), c.typargs.toArray, map_typ(c.typ))

  def map_axiom(a: Export_Theory.Axiom) = Axiom(map_entity(a.entity), map_prop(a.prop))

  def map_thm_id(ti: Export_Theory.Thm_Id) = ThmId(ti.serial, ti.theory_name)

  def map_term(t: Term.Term): Term = {
    t match {
      case Term.Const(name, typargs) => Constref(name, typargs.map(map_typ).toArray)
      case Term.Free(name, typ) => Free(name, map_typ(typ))
      case Term.Var(name, typ) => Var(map_indexname(name), map_typ(typ))
      case Term.Bound(index) => Bound(index)
      case Term.Abs(name, typ, body) => Abs(name, map_typ(typ), map_term(body))
      case Term.App(fun, arg) => App(map_term(fun), map_term(arg))
    }
  }

  def map_proof(p: Term.Proof): Proof = {
    p match {
      case Term.MinProof => MinProof
      case Term.PBound(index) => PBound(index)
      case Term.Abst(name, typ, body) => Abst(name, map_typ(typ), map_proof(body))
      case Term.AbsP(name, hyp, body) => AbsP(name, map_term(hyp), map_proof(body))
      case Term.Appt(fun, arg) => Appt(map_proof(fun), map_term(arg))
      case Term.Hyp(hyp) => Hyp(map_term(hyp))
      case Term.PAxm(name, types) => PAxm(name, types.map(map_typ).toArray)
      case Term.OfClass(typ, cls) => OfClass(map_typ(typ), cls)
      case Term.Oracle(name, prop, types) => Oracle(name, map_term(prop), types.map(map_typ).toArray)
      case Term.PThm(serial, theory_name, name, types) => PThm(serial, theory_name, name, types.map(map_typ).toArray)
    }
  }

  def map_thm(t: Export_Theory.Thm) = Thm(
    map_entity(t.entity),
    map_prop(t.prop),
    t.deps.toArray,
    t.proof_boxes.map(map_thm_id).toArray,
    map_proof(t.proof)
  )

  def map_constdef(cd: Export_Theory.Constdef) = Constdef(cd.name, cd.axiom_name)

  def map_type(t: Export_Theory.Type) = Type(map_entity(t.entity), t.args.toArray)

  def map_typedef(td: Export_Theory.Typedef) = Typedef(
    td.name,
    map_typ(td.rep_type),
    map_typ(td.abs_type),
    td.rep_name,
    td.abs_name,
    td.axiom_name
  )

  def serialize_theory(theory: Export_Theory.Theory): Bytes = {
    // Map to stable elements
    val thy = Theory(
      theory.name,
      theory.consts.map(map_const).toArray,
      theory.axioms.map(map_axiom).toArray,
      theory.thms.map(map_thm).toArray,
      theory.types.map(map_type).toArray,
      theory.constdefs.map(map_constdef).toArray,
      theory.typedefs.map(map_typedef).toArray
    )

    // Serialize entities
    val byteStream = new ByteArrayOutputStream()
    val objectStream = new ObjectOutputStream(byteStream)

    objectStream.writeObject(thy)
    objectStream.close()

    Bytes(byteStream.toByteArray)
  }
}
