object MappingSerializer {
  /* Imports need to be inlined for reflective compilation */
  import java.io.{ByteArrayOutputStream, ObjectOutputStream}

  import isabelle.{Export_Theory, Bytes, Properties}

  private def mapEntity(e: Export_Theory.Entity) = {
    if (!Properties.defined(e.pos, "offset") || !Properties.defined(e.pos, "end_offset")) {
      throw new IllegalArgumentException("Offsets need to be defined for entity " + e)
    }

    val start = Properties.get(e.pos, "offset").get.toInt
    val end = Properties.get(e.pos, "end_offset").get.toInt

    new Entity(e.xname, e.serial, start, end)
  }

  def serialize_theory(theory: Export_Theory.Theory): Bytes = {
    // Map to stable elements
    val consts = theory.consts map { const =>
      new Constant(mapEntity(const.entity), const.typargs.toArray, const.typ.toString)
    } toArray
    val thms = theory.thms map { thm =>
      new Theorem(mapEntity(thm.entity),
        thm.prop.typargs.map(_._1).toArray,
        thm.prop.args map { t => (t._1, t._2.toString) } toArray,
        thm.prop.term.toString,
        thm.deps.toArray,
        thm.proof.toString)
    } toArray
    val thy = new Theory(theory.name, consts, thms)

    // Serialize entities
    val byteStream = new ByteArrayOutputStream()
    val objectStream = new ObjectOutputStream(byteStream)

    objectStream.writeObject(thy)
    objectStream.close()

    Bytes(byteStream.toByteArray)
  }
}