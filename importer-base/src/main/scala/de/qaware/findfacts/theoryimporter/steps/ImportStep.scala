package de.qaware.findfacts.theoryimporter.steps

import de.qaware.findfacts.theoryimporter.TheoryView.Theory

/** Non-technical error in import, i.e. an issue preventing the business logic from being executed correctly.
  *
  * @param step the error occurred in
  * @param causeEntity name of the entity associated with the error
  * @param errorMsg description of the issue
  */
case class ImportError(step: ImportStep, causeEntity: String, errorMsg: String)

/** Step of the import process. */
trait ImportStep {

  /** Applies the step, mutating the context.
    *
    * @param theories to import
    * @param ctx context that is mutated in the step
    * @return import errors, if any
    */
  def apply(theories: Seq[Theory])(implicit ctx: StepContext): List[ImportError]
}
