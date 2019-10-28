package de.qaware.yxml

import scala.util.parsing.input.Positional

sealed trait YxmlToken extends Positional

sealed trait TextToken extends YxmlToken {
  def str: String
}
/** Arbitrary text, without control characters, tokens, and whitespace.
  *
  * @param str text string
  */
case class TS(override val str: String) extends TextToken

/** Equal sign. */
case class EQ() extends TextToken {
  override val str: String = "="
}

/** 'X' char, i.e. ascii char 5 */
case class X() extends YxmlToken

/** 'Y' char, i.e. ascii char 6 */
case class Y() extends YxmlToken
