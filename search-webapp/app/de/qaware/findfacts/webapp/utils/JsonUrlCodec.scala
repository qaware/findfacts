package de.qaware.findfacts.webapp.utils

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.Base64
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import io.circe.Json
// scalastyle:off
import io.circe.parser._
// scalastyle:on

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try

/** Codec to write json into urls. */
class JsonUrlCodec {

  /** Compresses and encodes json.
    *
    * @param json to encode
    * @return compressed and url-encoded json
    */
  def encodeCompressed(json: Json): String = {
    // Zip json
    val output = new ByteArrayOutputStream
    val zipper = new GZIPOutputStream(output)
    zipper.write(json.toString.getBytes)
    zipper.close()
    // Encode as url-base64
    Base64.getUrlEncoder.encodeToString(output.toByteArray)
  }

  /** Decompresses and decodes json.
    *
    * @param json to decode
    * @return decompressed and decoded json
    */
  def decodeCompressed(json: String): Try[Json] = {
    Try {
      val input = new GZIPInputStream(new ByteArrayInputStream(Base64.getUrlDecoder.decode(json)))
      val unzipped = Source.fromInputStream(input).mkString
      parse(unzipped).toTry
    } flatten
  }
}
