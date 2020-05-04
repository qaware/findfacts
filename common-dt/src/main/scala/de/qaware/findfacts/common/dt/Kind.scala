package de.qaware.findfacts.common.dt

import enumeratum.EnumEntry
import io.swagger.annotations.ApiModel

import de.qaware.findfacts.common.utils.DefaultEnum

/** Union type for theory entity kinds. */
@ApiModel(description = "variants: " + Kind.names)
sealed trait Kind extends EnumEntry

/** Kinds of theory entities. */
object Kind extends DefaultEnum[Kind] {
  override final val names = findNames
  override final val values = findValues

  /** Type definitions. */
  case object Type extends Kind

  /** Pure constants. */
  case object Constant extends Kind

  /** Pure axioms and theorems. */
  case object Fact extends Kind
}
