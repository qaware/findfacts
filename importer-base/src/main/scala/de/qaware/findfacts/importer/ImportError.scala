package de.qaware.findfacts.importer

import de.qaware.findfacts.importer.steps.ImportStep

/**
 * Error in import, i.e. an issue preventing the business logic from being executed correctly.
 *
 * @param step the error occurred in
 * @param causeEntity name of the entity associated with the error
 * @param errorMsg description of the issue
 * @param debugInfo lazy string for more involved debug information
 */
class ImportError(val step: ImportStep, val causeEntity: String, val errorMsg: String, debugInfo: => String) {
  override def toString: String = s"Error at $step, $causeEntity: $errorMsg"
  def getDebugInfo: String = debugInfo
}

/** Companion object, needed since case classes can't have by-name params. */
object ImportError {

  /**
   * C-tor.
   *
   * @param step the error occurred in
   * @param causeEntity name of the entity associated with the error
   * @param errorMsg description of the issue
   * @param debugInfo lazy string for more involved debug information
   * @return new import error
   */
  def apply(step: ImportStep, causeEntity: String, errorMsg: String, debugInfo: => String): ImportError =
    new ImportError(step, causeEntity, errorMsg, debugInfo)
}
