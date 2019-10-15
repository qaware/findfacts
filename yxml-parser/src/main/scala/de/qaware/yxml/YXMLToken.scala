package de.qaware.yxml

import scala.util.parsing.input.Positional

/** Tokens in yxml. */
sealed trait YXMLToken extends Positional

/** 'X' char, i.e. ascii char 5 */
case class X() extends YXMLToken

/** 'Y' char, i.e. ascii char 6 */
case class Y() extends YXMLToken

/** Trait for token that can be text. */
sealed trait TextToken {
  /** String representation. */
  val str: String
}

/** Equal sign. */
case class EQUAL() extends YXMLToken with TextToken {
  override val str: String = "="
}

/** Whitespace for manual handling.
  *
  * @param str the whitespace as string representation
  */
case class WS(str: String) extends YXMLToken with TextToken

/** Arbitrary text, without control characters, tokens, and whitespace.
  *
  * @param str text string
  */
case class TEXT(str: String) extends YXMLToken with TextToken
