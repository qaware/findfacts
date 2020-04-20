package de.qaware.findfacts.importer.steps.impl.thy

import de.qaware.findfacts.importer.TheoryView.Prop

/**
 * Extractor to get references from propositions.
 *
 * @param typExtractor  to extract types
 * @param termExtractor to extract terms.
 */
final class PropExtractor(typExtractor: TypExtractor, termExtractor: TermExtractor) {

  /**
   * Find referenced constants for a proposition.
   *
   * @param prop to find constants in
   * @return set of constant names
   */
  def referencedConsts(prop: Prop): Set[String] = {
    termExtractor.referencedConsts(prop.term)
  }

  /**
   * Find referenced types for a proposition.
   *
   * @param prop to find types in
   * @return set of type names
   */
  def referencedTypes(prop: Prop): Set[String] = {
    prop.args.map(_._2).flatMap(typExtractor.referencedTypes).toSet ++ termExtractor.referencedTypes(prop.term)
  }
}
