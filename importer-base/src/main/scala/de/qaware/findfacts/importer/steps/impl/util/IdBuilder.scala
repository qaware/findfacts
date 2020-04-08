package de.qaware.findfacts.importer.steps.impl.util

import de.qaware.findfacts.common.dt.{Kind, TheoryEt}
import de.qaware.findfacts.importer.steps.impl.pure.PureSyntax

/** Util to build ids. */
class IdBuilder {

  /** Creates ids from names
    *
    * @param kind of theory entity
    * @param names to get ids for
    * @return list of ids
    */
  def getIds(kind: Kind, names: List[String]): List[String] = {
    names.filterNot(PureSyntax.isPure).map(TheoryEt.makeId(kind, _))
  }
}
