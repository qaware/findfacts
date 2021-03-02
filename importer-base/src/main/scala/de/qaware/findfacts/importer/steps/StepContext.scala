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
final case class StepContext(
    blocks: mutable.Set[CodeblockEt] = mutable.Set.empty,
    consts: mutable.Map[Position, mutable.Set[ConstantEt]] = mutable.Map.empty,
    types: mutable.Map[Position, mutable.Set[TypeEt]] = mutable.Map.empty,
    facts: mutable.Map[Position, mutable.Set[FactEt]] = mutable.Map.empty) {

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
    (consts.toMap.view.mapValues(_.toList) ++
      types.toMap.view.mapValues(_.toList) ++
      facts.toMap.view.mapValues(_.toList))
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
