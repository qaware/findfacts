package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.utils.DefaultEnum
import enumeratum.EnumEntry

/** Union type for entity kinds. */
sealed trait EtKind extends EnumEntry

/** Kinds of entities. */
object EtKind extends DefaultEnum[EtKind] {
  override final val values = findValues

  /** Type definition. */
  case object Type extends Value

  /** Constants, including constructors. */
  case object Constant extends Value

  /** Some propositions. */
  case object Fact extends Value

  /** Source text. */
  case object Block extends Value

  /** Comments, sections, titles etc. */
  case object Documentation extends Value
}
