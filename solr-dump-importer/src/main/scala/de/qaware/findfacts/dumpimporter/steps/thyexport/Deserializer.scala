package de.qaware.findfacts.dumpimporter.steps.thyexport

import java.io.{ByteArrayInputStream, InputStream, ObjectInputStream, ObjectStreamClass}

import scala.util.Try
import scala.util.matching.Regex

import de.qaware.findfacts.dumpimporter.steps.thyexport.IsabelleEntities.Theory

/** Deserializer for dump_stable data. */
object Deserializer {

  /** Deserializes theory.
    *
    * @param bytes to deserialize
    * @return deserialized theory
    */
  @SuppressWarnings(Array("AsInstanceOf")) // Justification: Deserialization
  def deserialize(bytes: Array[Byte]): Try[Theory] = {
    val ois = new NameMappingObjectInputStream(
      "__wrapper(.*).EntityWrapper".r,
      "de.qaware.findfacts.dumpimporter.steps.thyexport.IsabelleEntities",
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
