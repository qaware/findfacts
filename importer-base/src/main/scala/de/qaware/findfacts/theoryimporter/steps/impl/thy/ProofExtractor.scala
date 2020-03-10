package de.qaware.findfacts.theoryimporter.steps.impl.thy

import de.qaware.findfacts.theoryimporter.TheoryView._

/** Extractor to extract information from proofs.
  *
  * @param typExtractor to extract types
  * @param termExtractor to extract terms
  */
class ProofExtractor(typExtractor: TypExtractor, termExtractor: TermExtractor) {

  /** Find facts referenced in a proof.
    *
    * @param proof to find facts in
    * @return set of fact names
    */
  def referencedFacts(proof: Proof): Set[String] = proof match {
    case Abst(_, _, body) => referencedFacts(body)
    case AbsP(_, _, body) => referencedFacts(body)
    case Appt(fun, _) => referencedFacts(fun)
    case AppP(fun, arg) => referencedFacts(fun) ++ referencedFacts(arg)
    case PAxm(name, _) => Set(name)
    case Oracle(name, _, _) => Set.empty // TODO correct?
    case PThm(_, name, _) => Set(name)
    case _ => Set.empty
  }

  /** Find consts referenced in a proof.
    *
    * @param proof to find consts in
    * @return set of constant names
    */
  def referencedConsts(proof: Proof): Set[String] = proof match {
    case Abst(_, _, body) => referencedConsts(body)
    case AbsP(_, hyp, proof) => termExtractor.referencedConsts(hyp) ++ referencedConsts(proof)
    case Appt(fun, arg) => referencedConsts(fun) ++ termExtractor.referencedConsts(arg)
    case AppP(fun, arg) => referencedConsts(fun) ++ referencedConsts(arg)
    case Hyp(hyp) => termExtractor.referencedConsts(hyp)
    case Oracle(_, prop, _) => termExtractor.referencedConsts(prop)
    case _ => Set.empty
  }

  /** Find types referenced in a proof.
    *
    * @param proof to find types in
    * @return set of type names
    */
  def referencedTypes(proof: Proof): Set[String] = proof match {
    case Abst(_, typ, body) => typExtractor.referencedTypes(typ) ++ referencedTypes(body)
    case AbsP(_, hyp, body) => termExtractor.referencedTypes(hyp) ++ referencedTypes(body)
    case Appt(fun, arg) => referencedTypes(fun) ++ termExtractor.referencedTypes(arg)
    case AppP(fun, arg) => referencedTypes(fun) ++ referencedTypes(arg)
    case Hyp(hyp) => termExtractor.referencedTypes(hyp)
    case PAxm(_, types) => types.flatMap(typExtractor.referencedTypes).toSet
    case OfClass(typ, _) => typExtractor.referencedTypes(typ)
    case Oracle(_, prop, types) =>
      termExtractor.referencedTypes(prop) ++ types.flatMap(typExtractor.referencedTypes).toSet
    case PThm(_, _, types) => types.flatMap(typExtractor.referencedTypes).toSet
    case _ => Set.empty
  }
}
