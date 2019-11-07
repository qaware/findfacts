package de.qaware.yxml

import scala.util.parsing.input.Positional

/** Base token trait. */
sealed trait YxmlToken extends Positional

/** Token containing readable text. */
sealed trait TextToken extends YxmlToken {

  /** Contained text
    *
    * @return text as string
    */
  def str: String
}

/** Arbitrary text, without control characters, tokens, and whitespace.
  *
  * @param str text string
  */
final case class TS(override val str: String) extends TextToken

/** Equal sign. */
@SuppressWarnings(Array("EmptyCaseClass")) // Justification: Positional data stored in superclass
final case class EQ() extends TextToken {
  override val str: String = "="
}

/** 'X' char, i.e. ascii char 5 */
@SuppressWarnings(Array("EmptyCaseClass")) // Justification: Positional data stored in superclass
final case class X() extends YxmlToken

/** 'Y' char, i.e. ascii char 6 */
@SuppressWarnings(Array("EmptyCaseClass")) // Justification: Positional data stored in superclass
final case class Y() extends YxmlToken
