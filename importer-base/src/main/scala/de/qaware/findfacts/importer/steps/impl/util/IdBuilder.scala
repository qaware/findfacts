package de.qaware.findfacts.importer.steps.impl.util

import de.qaware.findfacts.common.dt.EtField.Id
import de.qaware.findfacts.common.dt.Kind
import de.qaware.findfacts.importer.steps.impl.pure.PureSyntax

/** Util to build ids. */
final class IdBuilder {

  /**
   * Creates an unique id for a code-block.
   *
   * @param theory the block is in
   * @param startPos the position of the first character in the block
   * @param endPos the position after the last character in the block
   * @return unique id
   */
  def blockId(theory: String, startPos: Int, endPos: Int): String = s"$theory.$startPos.$endPos"

  /**
   * Creates an unique id for a given theory entity.
   *
   * @param kind of the entity
   * @param fullName of the entity. Must be unique within the kind of entities.
   * @return unique id
   */
  def theoryEtId(kind: Kind, fullName: String): Id.T = s"$kind.$fullName"

  /**
   * Creates ids from names.
   *
   * @param kind of theory entity
   * @param names to get ids for
   * @return list of ids
   */
  def getIds(kind: Kind, names: List[String]): List[String] = {
    names.filterNot(PureSyntax.isPure).map(theoryEtId(kind, _))
  }
}
