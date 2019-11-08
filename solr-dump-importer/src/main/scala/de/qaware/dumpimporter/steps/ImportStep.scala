package de.qaware.dumpimporter.steps

import scala.collection.mutable

import de.qaware.common.solr.dt.{ConstEntity, FactEntity, TypeEntity}
import de.qaware.dumpimporter.Config

/** Step of the import process. */
trait ImportStep {

  /** Configuration parameter */
  val config: Config

  /** Applies the step, mutating the context.
    *
    * @param context that is mutated in the step
    */
  def apply(context: StepContext): Unit
}

/** Context shared throughout the steps.
  *
  * @param consts intermediate constant entities
  * @param types intermediate type entities
  * @param facts intermediate fact entities
  */
final case class StepContext(
    consts: mutable.Set[ConstEntity] = mutable.Set.empty,
    types: mutable.Set[TypeEntity] = mutable.Set.empty,
    facts: mutable.Set[FactEntity] = mutable.Set.empty)
object StepContext {

  /** Builds an empty context.
    *
    * @return a new context
    */
  def empty: StepContext = StepContext()
}
