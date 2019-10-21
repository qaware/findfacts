package de.qaware.dumpimporter.steps

import scala.collection.mutable

import de.qaware.common.solr.dt.DocumentationEntity
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

/** Context shared throughout the steps. */
final case class StepContext(doc: mutable.Set[DocumentationEntity] = mutable.Set.empty)
object StepContext {

  /** Builds an empty context.
    *
    * @return a new context
    */
  def empty: StepContext = StepContext()
}
