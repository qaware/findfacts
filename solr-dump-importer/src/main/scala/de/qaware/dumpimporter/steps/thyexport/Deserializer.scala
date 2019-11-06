package de.qaware.dumpimporter.steps.thyexport

import java.io.{ByteArrayInputStream, InputStream, ObjectInputStream, ObjectStreamClass}

import scala.util.Try
import scala.util.matching.Regex

@SerialVersionUID(-4468468967588042461L)
sealed case class Entity(name: String, localName: String, serial: Long, startPos: Int, endPos: Int)

@SerialVersionUID(-6479946311825844471L)
sealed case class Const(entity: Entity, typargs: Array[String], typ: String)

@SerialVersionUID(645511391258153528L)
sealed case class Constdef(name: String, axiomName: String)

@SerialVersionUID(-6072692905282064541L)
sealed case class Axiom(entity: Entity, prop: Prop)

@SerialVersionUID(-5240318743403678874L)
sealed case class Prop(typargs: Array[(String, String)], args: Array[(String, String)], term: String)

@SerialVersionUID(3258375583870971926L)
sealed case class ThmId(serial: Long, theory_name: String)

@SerialVersionUID(-5164559171920544926L)
sealed case class Thm(entity: Entity, prop: Prop, deps: Array[String], proofBoxes: Array[ThmId], proof: String)

@SerialVersionUID(-8070090537415643108L)
sealed case class Theory(
    name: String,
    consts: Array[Const],
    axioms: Array[Axiom],
    thms: Array[Thm],
    constdefs: Array[Constdef])

/* Deserializer for dump_stable data. */
object Deserializer {

  /** Deserializes theory.
    *
    * @param bytes to deserialize
    * @return deserialized theory
    */
  def deserialize(bytes: Array[Byte]): Try[Theory] = {
    val ois = new NameMappingObjectInputStream(
      "__wrapper(.*).EntityWrapper\\$".r,
      "de.qaware.dumpimporter.steps.thyexport.",
      new ByteArrayInputStream(bytes))
    Try {
      val theory = ois.readObject
      ois.close()
      theory.asInstanceOf[Theory]
    }
  }
}

/** Object input stream that can map class names.
  *
  * @param from regex to match parts to rename
  * @param to replacement string
  * @param in input stream
  */
class NameMappingObjectInputStream(from: Regex, to: String, in: InputStream) extends ObjectInputStream(in) {
  override def resolveClass(desc: ObjectStreamClass): Class[_] = {
    Class.forName(desc.getName.replaceAll(from.regex, to))
  }
  override def readClassDescriptor(): ObjectStreamClass = {
    ObjectStreamClass.lookup(resolveClass(super.readClassDescriptor()))
  }
}
