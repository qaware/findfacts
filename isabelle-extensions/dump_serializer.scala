object MappingSerializer
{
  /* Imports need to be inlined for reflective compilation */
  import java.io.{ByteArrayOutputStream, ObjectOutputStream}

  import isabelle.{Export_Theory, Bytes, Properties}

  def map_entity(e: Export_Theory.Entity) = {
    if (!Properties.defined(e.pos, "offset") || !Properties.defined(e.pos, "end_offset")) {
      throw new IllegalArgumentException("Offsets need to be defined for entity " + e)
    }

    val start = Properties.get(e.pos, "offset").get.toInt
    val end = Properties.get(e.pos, "end_offset").get.toInt

    Entity(e.name, e.xname, e.serial, start, end)
  }

  def map_prop(p: Export_Theory.Prop) = Prop(
    p.typargs.map(ta => (ta._1, ta._2.toString)).toArray,
    p.args.map(a => (a._1, a._2.toString)).toArray,
    p.term.toString
  )

  def map_const(c: Export_Theory.Const) = Const(map_entity(c.entity), c.typargs.toArray, c.typ.toString)

  def map_axiom(a: Export_Theory.Axiom) = Axiom(map_entity(a.entity), map_prop(a.prop))

  def map_thm_id(ti: Export_Theory.Thm_Id) = ThmId(ti.serial, ti.theory_name)

  def map_thm(t: Export_Theory.Thm) = Thm(
    map_entity(t.entity),
    map_prop(t.prop),
    t.deps.toArray,
    t.proof_boxes.map(map_thm_id).toArray,
    t.proof.toString
  )

  def map_constdef(cd: Export_Theory.Constdef) = Constdef(cd.name, cd.axiom_name)

  def serialize_theory(theory: Export_Theory.Theory): Bytes = {
    // Map to stable elements
    val thy = Theory(
      theory.name,
      theory.consts.map(map_const).toArray,
      theory.axioms.map(map_axiom).toArray,
      theory.thms.map(map_thm).toArray,
      theory.constdefs.map(map_constdef).toArray
    )

    // Serialize entities
    val byteStream = new ByteArrayOutputStream()
    val objectStream = new ObjectOutputStream(byteStream)

    objectStream.writeObject(thy)
    objectStream.close()

    Bytes(byteStream.toByteArray)
  }
}
