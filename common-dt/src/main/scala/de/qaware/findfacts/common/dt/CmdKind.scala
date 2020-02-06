package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.utils.DefaultEnum
import enumeratum.EnumEntry

/** Union type for enum. */
sealed trait CmdKind extends EnumEntry

/** Kind of command spans. */
object CmdKind extends DefaultEnum[CmdKind] {
  final val values = findValues

  /** Command spans that are code blocks. */
  case object Codeblock extends Value

  /** Comamnd spans that are purely documentation. */
  case object Documentation extends Value
}
