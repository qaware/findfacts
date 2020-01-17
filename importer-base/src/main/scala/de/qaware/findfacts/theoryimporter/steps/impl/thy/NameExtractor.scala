package de.qaware.findfacts.theoryimporter.steps.impl.thy

import de.qaware.findfacts.theoryimporter.TheoryView.Indexname

/** Extractor to pretty-print names. */
class NameExtractor {

  /** Pretty-prints index names.
    *
    * @param idxName to pretty-print
    * @return pretty string
    */
  def prettyPrint(idxName: Indexname): String = {
    if (!idxName.name.endsWith("[0-9]"))
      if (idxName.index == 0) idxName.name else s"${idxName.name}${idxName.index}"
    else
      s"${idxName.name}.${idxName.index}"
  }
}
