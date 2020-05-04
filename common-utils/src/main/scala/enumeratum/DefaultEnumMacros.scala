package enumeratum

import scala.reflect.macros.whitebox

/** Extension of enumeratum-private macros. */
object DefaultEnumMacros {

  /** Generates a compile-time string of enum names.
   *
   * @param c whitebox context (required for literal type)
   * @tparam E type of enum
   * @return constant string expression for name list
   */
  def names[E: c.WeakTypeTag](c: whitebox.Context): c.Expr[String] = {
    import c.universe._

    val subclassSymbols = EnumMacros.enclosedSubClasses(c)(weakTypeOf[E].typeSymbol)

    reify {
      c.Expr[String] {
        Literal(Constant(subclassSymbols.map(_.name).mkString(", ")))
      }.splice
    }
  }
}
