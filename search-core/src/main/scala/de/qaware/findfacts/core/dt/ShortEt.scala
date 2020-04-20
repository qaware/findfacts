package de.qaware.findfacts.core.dt

import io.circe.generic.auto._

import de.qaware.findfacts.common.dt.EtField._

/** Children field for short theory entities. */
case object ShortChildren extends Children[ShortThyEt] {
  override implicit val implicits: FieldImplicits[ShortThyEt] = FieldImplicits()
}

/**
 * Short source code block.
 *
 * @param id unique identifier
 * @param theory source theory
 * @param startLine line at which block starts
 * @param srcBefore source code before this block
 * @param src source text
 * @param srcAfter source text after this block
 * @param entities child entities
 * @param command type of the command
 */
final case class ShortBlock(
    id: Id.T,
    theory: SourceTheory.T,
    startLine: StartLine.T,
    srcBefore: SourceTextBefore.T,
    src: SourceText.T,
    srcAfter: SourceTextAfter.T,
    entities: ShortChildren.T,
    command: Command.T
)

/**
 * Short theory entity.
 *
 * @param kind of theory entity
 * @param name of theory entity
 */
final case class ShortThyEt(id: Id.T, kind: Kind.T, name: Name.T)
