package de.qaware.dumpimporter.steps

import de.qaware.dumpimporter.Config

/** Step of the import process. */
trait ImportStep {

  /**  */
  def config: Config

  /**  */
  def apply(ctx: StepContext): Unit
}

case class StepContext()
