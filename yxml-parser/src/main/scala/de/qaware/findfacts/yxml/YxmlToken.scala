package de.qaware.findfacts.yxml

/** Base token trait. */
sealed trait YxmlToken extends Any

/** Token containing readable text. */
sealed trait TextToken extends Any with YxmlToken {

  /** Contained text
    *
    * @return text as string
    */
  def str: String

  override def toString: String = str
}

/** Arbitrary text, without control characters, tokens, and whitespace.
  *
  * @param str text string
  */
final class TS(override val str: String) extends AnyVal with TextToken
/** Companion object for value class to allow for pattern matching. */
object TS {
  def unapply(arg: TS): Option[String] = Some(arg.str) // scalastyle:ignore
}

/** Equal sign. */
case object EQ extends TextToken {
  override val str: String = "="
}

/** 'X' char, i.e. ascii char 5 */
case object X extends YxmlToken

/** 'Y' char, i.e. ascii char 6 */
case object Y extends YxmlToken
