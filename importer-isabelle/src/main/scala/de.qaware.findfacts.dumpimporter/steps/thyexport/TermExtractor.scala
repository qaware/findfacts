package de.qaware.findfacts.dumpimporter.steps.thyexport

import de.qaware.findfacts.dumpimporter.pure.PureSyntax
import isabelle.Term
import isabelle.Term.{Abs, App, Bound, Const, Free, Term, Var}

class TermExtractor {
  def referencedConsts(term: Term): Set[String] = term match {
    case Const(name, _) => Set(name)
    case Abs(_, _, body) => referencedConsts(body)
    case App(fun, arg) => referencedConsts(fun) ++ referencedConsts(arg)
    case _ => Set.empty
  }

  def prettyPrint(term: Term, vars: IndexedSeq[String] = Vector.empty): String = term match {
    case Const(PureSyntax.Type.name, _) => "Γ"
    case Const(name, _) => s"<$name>"
    case Free(name, _) => s"'$name'"
    case Var(name, _) => s"?${name.toString}"
    case Bound(index) => vars(index)
    case abs: Abs => s"(λ ${chainAbs(abs, vars)})"
    case App(Const(PureSyntax.Eq.name, _), arg) => s"${prettyPrint(arg, vars)} ≡"
    case App(Const(PureSyntax.Imp.name, _), arg) => s"${prettyPrint(arg, vars)} ⟹"
    case App(Const(PureSyntax.All.name, _), arg) => s"(⋀ ${prettyPrint(arg, vars)})"
    case App(Const(PureSyntax.Conj.name, _), arg) => s"${prettyPrint(arg, vars)} ∧"
    case App(fun, arg) => s"(${prettyPrint(fun, vars)} ${prettyPrint(arg, vars)})"
  }

  private def chainAbs(abs: Term.Abs, vars: IndexedSeq[String]): String = abs.body match {
    case b: Abs => s"${abs.name} ${chainAbs(b, b.name +: vars)}"
    case _ => s"${abs.name}. ${prettyPrint(abs.body, abs.name +: vars)}"
  }
}
