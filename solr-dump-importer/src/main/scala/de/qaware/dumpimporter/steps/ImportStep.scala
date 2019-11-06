package de.qaware.dumpimporter.steps

import de.qaware.common.solr.dt.{ConstEntity, FactEntity}
import de.qaware.dumpimporter.Config

import scala.collection.mutable

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

/** Context shared throughout the steps. */
final case class StepContext(
    consts: mutable.Set[ConstEntity] = mutable.Set.empty,
    facts: mutable.Set[FactEntity] = mutable.Set.empty)
object StepContext {

  /** Builds an empty context.
    *
    * @return a new context
    */
  def empty: StepContext = StepContext()
}
