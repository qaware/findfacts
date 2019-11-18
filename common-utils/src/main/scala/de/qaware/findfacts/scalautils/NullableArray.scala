package de.qaware.findfacts.scalautils

/** Handle java arrays that can be null */
object NullableArray {

  /** Deep scala object representation of nullable java array.
    *
    * @param arr to represent
    * @tparam A type of array
    * @return deep representation that can be null
    */
  @SuppressWarnings(Array("NullParameter"))
  def deep[A](arr: Array[A]): IndexedSeq[Any] = if (arr == null) null else arr.deep // scalastyle:ignore
}
