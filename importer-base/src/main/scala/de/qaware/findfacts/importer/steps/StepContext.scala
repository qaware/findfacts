package de.qaware.findfacts.importer.steps

import scala.collection.mutable

import de.qaware.findfacts.common.dt.{BaseEt, CodeblockEt, ConstantEt, FactEt, TheoryEt, TypeEt}
import de.qaware.findfacts.importer.TheoryView.Position

/**
 * Holds mutable context shared throughout the steps.
 *
 * @param blocks block entities, without children
 * @param consts pairs of constants and the block it is in
 * @param types pairs of types and the block it is in
 * @param facts pairs of facts and the block it is in
 */
final class StepContext(
    blocks: mutable.Set[CodeblockEt] = mutable.Set.empty,
    consts: mutable.Map[Position, mutable.Set[ConstantEt]] = mutable.Map.empty,
    types: mutable.Map[Position, mutable.Set[TypeEt]] = mutable.Map.empty,
    facts: mutable.Map[Position, mutable.Set[FactEt]] = mutable.Map.empty) {

  /**
   * Adds a constant.
   *
   * @param pos on which const is defined
   * @param const to add
   */
  def putConst(pos: Position, const: ConstantEt): Unit = {
    consts.getOrElseUpdate(pos, { mutable.Set.empty }).add(const)
  }

  /**
   * Adds a type.
   *
   * @param pos on which type is defined
   * @param typ to add
   */
  def putType(pos: Position, typ: TypeEt): Unit = {
    types.getOrElseUpdate(pos, { mutable.Set.empty }).add(typ)
  }

  /**
   * Adds a fact.
   *
   * @param pos on which fact is defined
   * @param fact to add
   */
  def putFact(pos: Position, fact: FactEt): Unit = {
    facts.getOrElseUpdate(pos, { mutable.Set.empty }).add(fact)
  }

  /**
   * Adds a code block.
   *
   * @param block to add
   */
  def putBlock(block: CodeblockEt): Unit = blocks.add(block)

  /**
   * Immutable view on all blocks
   *
   * @return immutable view on all blocks
   */
  def getBlocks: Set[CodeblockEt] = blocks.toSet

  /**
   * Clear all blocks.
   */
  def clearBlocks(): Unit = blocks.clear()

  /**
   * Immutable view on all theory entities.
   *
   * @return immutable view on all theory entities
   */
  def theoryEts: Set[TheoryEt] = {
    consts.values.toSet.flatten ++ types.values.toSet.flatten ++ facts.values.toSet.flatten
  }

  /**
   * Immutable view on all theory entities by position.
   *
   * @return immutabel view on theory entiteis by position
   */
  def theoryEtsByPosition: Map[Position, List[TheoryEt]] = {
    (consts.view.mapValues(_.toList) ++
      types.view.mapValues(_.toList) ++
      facts.view.mapValues(_.toList))
      .toList
      .groupBy(_._1)
      .view
      .mapValues(_.flatMap(_._2))
      .toMap
  }

  /**
   * Immutable view on all entities.
   *
   * @return immutable view on all entities
   */
  def allEts: Set[BaseEt] = blocks.toSet ++ theoryEts
}

/** Companion object. */
object StepContext {

  /**
   * C-tor.
   *
   * @return empty context
   */
  def apply(): StepContext = new StepContext()
}
