package de.qaware.findfacts.theoryimporter.steps.impl.thy

import de.qaware.findfacts.theoryimporter.TheoryView.{Abs, App, Bound, ConstTerm, Free, Term, Var}
import de.qaware.findfacts.theoryimporter.pure.PureSyntax

class TermExtractor {
  def referencedConsts(term: Term): Set[String] = term match {
    case ConstTerm(name, _) => Set(name)
    case Abs(_, _, body) => referencedConsts(body)
    case App(fun, arg) => referencedConsts(fun) ++ referencedConsts(arg)
    case _ => Set.empty
  }

  def prettyPrint(term: Term, vars: IndexedSeq[String] = Vector.empty): String = term match {
    case ConstTerm(PureSyntax.Type.name, _) => "Γ"
    case ConstTerm(name, _) => s"<$name>"
    case Free(name, _) => s"'$name'"
    case Var(name, _) => s"?${name.toString}"
    case Bound(index) => vars(index)
    case abs: Abs => s"(λ ${chainAbs(abs, vars)})"
    case App(ConstTerm(PureSyntax.Eq.name, _), arg) => s"${prettyPrint(arg, vars)} ≡"
    case App(ConstTerm(PureSyntax.Imp.name, _), arg) => s"${prettyPrint(arg, vars)} ⟹"
    case App(ConstTerm(PureSyntax.All.name, _), arg) => s"(⋀ ${prettyPrint(arg, vars)})"
    case App(ConstTerm(PureSyntax.Conj.name, _), arg) => s"${prettyPrint(arg, vars)} ∧"
    case App(fun, arg) => s"(${prettyPrint(fun, vars)} ${prettyPrint(arg, vars)})"
  }

  private def chainAbs(abs: Abs, vars: IndexedSeq[String]): String = abs.body match {
    case b: Abs => s"${abs.name} ${chainAbs(b, b.name +: vars)}"
    case _ => s"${abs.name}. ${prettyPrint(abs.body, abs.name +: vars)}"
  }
}
