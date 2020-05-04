package de.qaware.findfacts.common.da.api

import enumeratum.EnumEntry

import de.qaware.findfacts.common.utils.DefaultEnum

/** Union type for doc kinds. */
sealed trait DocumentKind extends EnumEntry

/** Kind of documents */
object DocumentKind extends DefaultEnum[DocumentKind] {
  override final val values = findValues
  override final val names = findNames

  /** Parent docs. */
  case object Parent extends DocumentKind

  /** Child docs. */
  case object Child extends DocumentKind
}
