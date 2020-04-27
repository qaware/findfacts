package de.qaware.findfacts.importer.steps.impl.thy

import de.qaware.findfacts.importer.TheoryView.{Abs, App, ConstTerm, Free, Term, Var}

/**
 * Helper to extract term content.
 *
 * @param typExtractor to extract types
 */
class TermExtractor(typExtractor: TypExtractor) {

  /**
   * Finds consts referenced in the term.
   *
   * @param term to find consts in
   * @return set of const names referenced in the term
   */
  def referencedConsts(term: Term): Set[String] =
    term match {
      case ConstTerm(name, _) => Set(name)
      case Abs(_, _, body) => referencedConsts(body)
      case App(fun, arg) => referencedConsts(fun) ++ referencedConsts(arg)
      case _ => Set.empty
    }

  /**
   * Finds types referenced in the term.
   *
   * @param term to find types in
   * @return set of type names referenced in the term
   */
  def referencedTypes(term: Term): Set[String] =
    term match {
      case ConstTerm(_, types) => types.flatMap(typExtractor.referencedTypes).toSet
      case Free(_, typ) => typExtractor.referencedTypes(typ)
      case Var(_, typ) => typExtractor.referencedTypes(typ)
      case Abs(_, typ, body) => typExtractor.referencedTypes(typ) ++ referencedTypes(body)
      case App(fun, arg) => referencedTypes(fun) ++ referencedTypes(arg)
      case _ => Set.empty
    }
}
