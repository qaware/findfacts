package de.qaware.findfacts.importer.steps

import scala.collection.mutable

import de.qaware.findfacts.common.dt.{BaseEt, CodeblockEt, ConstantEt, FactEt, TheoryEt, TypeEt}
import de.qaware.findfacts.common.utils.DefaultMultiMap
import de.qaware.findfacts.importer.TheoryView.Position

/**
 * Holds mutable context shared throughout the steps.
 *
 * @param blocks block entities, without children
 * @param consts pairs of constants and the block it is in
 * @param types pairs of types and the block it is in
 * @param facts pairs of facts and the block it is in
 */
final case class StepContext(
    blocks: mutable.Set[CodeblockEt] = mutable.Set.empty,
    consts: mutable.MultiMap[Position, ConstantEt] = DefaultMultiMap.empty,
    types: mutable.MultiMap[Position, TypeEt] = DefaultMultiMap.empty,
    facts: mutable.MultiMap[Position, FactEt] = DefaultMultiMap.empty) {

  /**
   * Immutable view on all theory entities.
   *
   * @return immutable view on all theory entities
   */
  def theoryEts: Set[TheoryEt] = {
    consts.values.to[Set].flatten ++ types.values.to[Set].flatten ++ facts.values.to[Set].flatten
  }

  /**
   * Immutable view on all theory entities by position.
   *
   * @return immutabel view on theory entiteis by position
   */
  def theoryEtsByPosition: Map[Position, List[TheoryEt]] = {
    (consts.toMap.mapValues(_.toList).toList ++
      types.toMap.mapValues(_.toList).toList ++
      facts.toMap.mapValues(_.toList).toList)
      .groupBy(_._1)
      .mapValues(_.flatMap(_._2))
  }

  /**
   * Immutable view on all entities.
   *
   * @return immutable view on all entities
   */
  def allEts: Set[BaseEt] = blocks.to[Set] ++ theoryEts
}
