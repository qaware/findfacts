package de.qaware.dumpimporter.steps.thyexport

import java.io.{ByteArrayInputStream, InputStream, ObjectInputStream, ObjectOutputStream, ObjectStreamClass}

import scala.util.Try
import scala.util.matching.Regex

object EntityWrapper {
  type Typ = String
  type Term = String
  type Proof = String

  @SerialVersionUID(-6600248326866346277L)
  sealed case class Entity(name: String, id: Long, startPos: Int, endPos: Int) extends Serializable

  @SerialVersionUID(8214256252769140008L)
  sealed case class Constant(entity: Entity, typargs: Array[String], typ: Typ) extends Serializable

  @SerialVersionUID(4245087039579397970L)
  sealed case class Theorem(
      entity: Entity,
      typargs: Array[String],
      args: Array[(String, Typ)],
      term: Term,
      deps: Array[String],
      proof: Proof)
      extends Serializable

  @SerialVersionUID(-7187211222156107352L)
  sealed case class Theory(name: String, consts: Array[Constant], thms: Array[Theorem]) extends Serializable
}

/* Deserializer for dump_stable data. */
object Deserializer {

  /** Deserializes theory.
    *
    * @param bytes to deserialize
    * @return deserialized theory
    */
  def deserialize(bytes: Array[Byte]): Try[EntityWrapper.Theory] = {
    val ois = new NameMappingObjectInputStream(
      "__wrapper(.*).EntityWrapper".r,
      "de.qaware.dumpimporter.steps.thyexport.EntityWrapper",
      new ByteArrayInputStream(bytes))
    Try {
      val theory = ois.readObject
      ois.close()
      theory.asInstanceOf[EntityWrapper.Theory]
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
  override def resolveClass(desc: ObjectStreamClass): Class[_] = Class.forName(desc.getName.replaceAll(from.regex, to))
}
