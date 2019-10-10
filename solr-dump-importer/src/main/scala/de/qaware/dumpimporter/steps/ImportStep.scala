package de.qaware.dumpimporter.steps

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
case class StepContext()
object StepContext {

  /** Builds an empty context.
    *
    * @return a new context
    */
  def empty: StepContext = StepContext()
}
