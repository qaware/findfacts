package de.qaware.findfacts.theoryimporter.steps.impl.thy

import de.qaware.findfacts.theoryimporter.TheoryView
import de.qaware.findfacts.theoryimporter.TheoryView.{Abs, App, Bound, ConstTerm, Free, Term, Typ, Var}
import de.qaware.findfacts.theoryimporter.pure.PureSyntax

class TermExtractor {

  private def replace(body: Term, _name: String, _typ: Typ, targetDepth: Int): Term = body match {
    case Bound(depth) if depth == targetDepth =>
      new Free {
        override def name: String = _name
        override def typ: Typ = _typ
      }
    case Abs(__name, __typ, _body) =>
      new Abs {
        override def name: String = __name
        override def typ: Typ = __typ
        override def body: Term = replace(_body, _name, _typ, targetDepth + 1)
      }
    case App(_fun, _arg) =>
      new App {
        override def fun: Term = replace(_fun, _name, _typ, targetDepth)
        override def arg: Term = replace(_arg, _name, _typ, targetDepth)
      }
    case _ => body
  }

  private def minimize(term: Term): Term = term match {
    case App(ConstTerm(PureSyntax.All.name, allTyp), Abs(_name, absTyp, _body)) if allTyp == List(absTyp) =>
      minimize(replace(_body, _name, absTyp, 0))
    case Abs(_name, _typ, _body) =>
      new Abs {
        override def name: String = _name
        override def typ: TheoryView.Typ = _typ
        override def body: Term = minimize(_body)
      }
    case App(_fun, _arg) =>
      new App {
        override def fun: Term = minimize(_fun)
        override def arg: Term = minimize(_arg)
      }
    case _ => term
  }

  def referencedConsts(term: Term): Set[String] = term match {
    case ConstTerm(name, _) => Set(name)
    case Abs(_, _, body) => referencedConsts(body)
    case App(fun, arg) => referencedConsts(fun) ++ referencedConsts(arg)
    case _ => Set.empty
  }

  def prettyPrint(term: Term): String = prettyPrintRec(minimize(term), Vector.empty)

  private def prettyPrintRec(term: Term, vars: IndexedSeq[String]): String = term match {
    case ConstTerm(PureSyntax.Type.name, _) => "Γ"
    case ConstTerm(name, _) => s"<$name>"
    case Free(name, _) => s"'$name'"
    case Var(name, _) => s"?${name.toString}"
    case Bound(index) => vars(index)
    case abs: Abs => s"(λ ${chainAbs(abs, vars)})"
    case App(ConstTerm(PureSyntax.Eq.name, _), arg) => s"${prettyPrintRec(arg, vars)} ≡"
    case App(ConstTerm(PureSyntax.Imp.name, _), arg) => s"${prettyPrintRec(arg, vars)} ⟹"
    case App(ConstTerm(PureSyntax.All.name, _), arg) => s"(⋀ ${prettyPrintRec(arg, vars)})"
    case App(ConstTerm(PureSyntax.Conj.name, _), arg) => s"${prettyPrintRec(arg, vars)} ∧"
    case App(fun, arg) => s"(${prettyPrintRec(fun, vars)} ${prettyPrintRec(arg, vars)})"
  }

  private def chainAbs(abs: Abs, vars: IndexedSeq[String]): String = abs.body match {
    case b: Abs => s"${abs.name} ${chainAbs(b, b.name +: vars)}"
    case _ => s"${abs.name}. ${prettyPrintRec(abs.body, abs.name +: vars)}"
  }
}
