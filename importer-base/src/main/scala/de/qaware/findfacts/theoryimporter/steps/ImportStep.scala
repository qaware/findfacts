package de.qaware.findfacts.theoryimporter.steps

import de.qaware.findfacts.theoryimporter.ImportError
import de.qaware.findfacts.theoryimporter.TheoryView.Theory

/** Step of the import process. */
trait ImportStep {

  /** Applies the step, mutating the context.
    *
    * @param theory to import
    * @param ctx context that is mutated in the step
    * @return import errors, if any
    */
  def apply(theory: Theory)(implicit ctx: StepContext): List[ImportError]
}
