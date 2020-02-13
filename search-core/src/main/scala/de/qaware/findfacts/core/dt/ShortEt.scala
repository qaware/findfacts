package de.qaware.findfacts.core.dt

import de.qaware.findfacts.common.da.api.Variant.Discriminator
import de.qaware.findfacts.common.dt
import de.qaware.findfacts.common.dt.CmdKind
// scalastyle:off
import de.qaware.findfacts.common.dt.EtField._
import io.circe.generic.auto._
// scalastyle:on

/** Children field for short theory entities. */
case object ShortChildren extends Children[ShortThyEt] {
  override implicit val implicits: FieldImplicits[ShortThyEt] = FieldImplicits()
}

/** Union type for commands. */
sealed trait ShortCmd {

  /** Unique identifier. */
  val id: Id.T

  /** Source theory. */
  val theory: SourceTheory.T

  /** Source text. */
  val src: SourceText.T

  /** Child entities. */
  val entities: ShortChildren.T

  /** Type of the command. */
  val cmdKind: CommandKind.T
}

/** Short source code block. */
final case class ShortBlock(
    override val id: Id.T,
    override val theory: SourceTheory.T,
    override val src: SourceText.T,
    override val entities: ShortChildren.T)
    extends ShortCmd
    with Discriminator[CmdKind, CommandKind.type, CmdKind.Codeblock.type] {
  override val cmdKind: CommandKind.T = CmdKind.Codeblock
}

/** Short documentation entity.
  *
  * @param docKind kind of documentation
  */
final case class ShortDocumentation(
    override val id: Id.T,
    override val theory: SourceTheory.T,
    override val src: SourceText.T,
    docKind: DocumentationKind.T)
    extends ShortCmd
    with Discriminator[CmdKind, CommandKind.type, dt.CmdKind.Documentation.type] {
  override val cmdKind: CommandKind.T = CmdKind.Documentation
  override val entities: ShortChildren.T = List.empty
}

/** Short theory entity.
  *
  * @param id unique identifier
  * @param kind of theory entity
  * @param name of theory entity
  */
case class ShortThyEt(id: Id.T, kind: Kind.T, name: Name.T)
