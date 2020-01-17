package de.qaware.findfacts.common.dt

import de.qaware.findfacts.common.utils.DefaultEnum
import enumeratum.EnumEntry

/** Union type for [[DocKind]]s. */
sealed trait DocKind extends EnumEntry

/** Types of documentation. */
object DocKind extends DefaultEnum[DocKind] {
  override final val values = findValues

  /** Comments in the meta-language, (* ... *) */
  case object Meta extends Value

  /** Latex documentation: sections, etc. */
  case object Latex extends Value

  /** Inline comments, usually in cartouches. */
  case object Inline extends Value
}
