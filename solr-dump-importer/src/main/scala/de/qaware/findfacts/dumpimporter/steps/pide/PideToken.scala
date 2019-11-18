package de.qaware.findfacts.dumpimporter.steps.pide

import de.qaware.findfacts.common.solr.dt.DocumentationType

/** Trait for Pide tokens. */
sealed trait PideToken {

  /** String data.
    *
    * @return data string
    */
  def data: String
}

/** Delimiter for type annotations. */
case object TypeDelimToken extends PideToken {
  override val data: String = PideField.TypeDelimiter.toString
}

/** Delimiter for lines of definition, for the 'fun' construct.  */
case object DefDelimToken extends PideToken {
  override val data: String = PideField.DefDelimiter.toString
}

/** Delimiter for name tags. */
case object NameDelimToken extends PideToken {
  override def data: String = PideField.NameDelimiter.toString
}

/** Pide 'where' token. */
case object WhereToken extends PideToken {
  override val data: String = PideField.Where.toString
}

/** Pide 'for' token. */
case object ForToken extends PideToken {
  override val data: String = PideField.For.toString
}

/** Pide string token.
  *
  * @param data string
  */
final case class StringToken(override val data: String) extends PideToken

/** Pide whitespace token.
  *
  * @param data string
  */
final case class WhitespaceToken(override val data: String) extends PideToken

/** Token for unknown constructs. As this parser only extracts a subset of PIDE,
  * many tokens could not be parsed otherwise.
  *
  * @param data string
  */
final case class UnknownToken(override val data: String) extends PideToken

/** Pide definition token.
  *
  * @param data string
  * @param serial internal id of the definition, consistent within a dump
  */
final case class DefToken(override val data: String, serial: Long) extends PideToken

/** Comment token.
  *
  * @param data string
  * @param docType type of the comment, i.e. inline or meta here
  */
final case class CommentToken(override val data: String, docType: DocumentationType.Value) extends PideToken

/** Generic token for code, used in the parser. This token is NOT lexed!
  *
  * @param data string
  */
final case class Code(override val data: String) extends PideToken
