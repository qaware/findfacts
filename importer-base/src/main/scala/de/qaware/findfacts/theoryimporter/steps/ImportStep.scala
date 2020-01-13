package de.qaware.findfacts.theoryimporter.steps

import de.qaware.findfacts.theoryimporter.TheoryView.Theory

/** Non-technical error in import, i.e. an issue preventing the business logic from being executed correctly.
  *
  * @param step the error occurred in
  * @param causeEntity name of the entity associated with the error
  * @param errorMsg description of the issue
  * @param debugInfo lazy string for more involved debug information
  */
class ImportError(val step: ImportStep, val causeEntity: String, val errorMsg: String, debugInfo: => String) {
  val getDebugInfo: String = debugInfo

  override def toString: String = s"Error at $step, $causeEntity: $errorMsg. Additional info: $debugInfo"
}
object ImportError {
  // scalastyle:ignore
  def apply(step: ImportStep, causeEntity: String, errorMsg: String, debugInfo: => String): ImportError =
    new ImportError(step, causeEntity, errorMsg, debugInfo)
}

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
