package de.qaware.findfacts.common.da.api

import enumeratum.EnumEntry
import shapeless.tag.Tagged

/**
 * Trait for variant fields.
 *
 * @tparam A type of the variant enum
 * @tparam F field to store variant information in
 * @tparam B concrete variant type
 */
trait Variant[A <: EnumEntry, F <: SingleValuedField[A], B <: A]

/** Companion object. */
object Variant {

  /**
   * Extending this type tags the class with [[Variant]].
   *
   * @tparam A type of the variant enum
   * @tparam F field to store variant information in
   * @tparam B concrete variant type
   */
  type Discriminator[A <: EnumEntry, F <: SingleValuedField[A], B <: A] = Tagged[Variant[A, F, B]]
}
