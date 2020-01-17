package de.qaware.findfacts.theoryimporter.steps.impl.thy

import de.qaware.findfacts.theoryimporter.TheoryView
import de.qaware.findfacts.theoryimporter.TheoryView.{Abs, App, Bound, ConstTerm, Free, Term, Typ, Var}
import de.qaware.findfacts.theoryimporter.pure.PureSyntax

/** Helper to extract term content and pretty-print terms.
  *
  * @param nameExtractor to pretty-print names
  */
class TermExtractor(nameExtractor: NameExtractor) {

  private def replace(body: Term, _name: String, _typ: Typ, absDepth: Int): Term = body match {
    // Correct bounds that reference abstractions before the replaced one, since now there is one less lambda in between
    case Bound(depth) if depth > absDepth =>
      new Bound {
        override def index: Int = depth - 1
      }
    // Replace bound by free variable if we hit
    case Bound(depth) if depth == absDepth =>
      new Free {
        override def name: String = _name
        override def typ: Typ = _typ
      }
    case Abs(__name, __typ, _body) =>
      new Abs {
        override def name: String = __name
        override def typ: Typ = __typ
        override def body: Term = replace(_body, _name, _typ, absDepth + 1)
      }
    case App(_fun, _arg) =>
      new App {
        override def fun: Term = replace(_fun, _name, _typ, absDepth)
        override def arg: Term = replace(_arg, _name, _typ, absDepth)
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

  /** Finds consts referenced in the term.
    *
    * @param term to find consts in
    * @return set of const names referenced in the term
    */
  def referencedConsts(term: Term): Set[String] = term match {
    case ConstTerm(name, _) => Set(name)
    case Abs(_, _, body) => referencedConsts(body)
    case App(fun, arg) => referencedConsts(fun) ++ referencedConsts(arg)
    case _ => Set.empty
  }

  /** Pretty-prints a term.
    *
    * @param term to pretty-print
    * @return pretty term string
    */
  def prettyPrint(term: Term): String = prettyPrintRec(minimize(term), Vector.empty)

  private def prettyPrintRec(term: Term, vars: IndexedSeq[String]): String = term match {
    case ConstTerm(PureSyntax.Type.name, _) => "Γ"
    case ConstTerm(name, _) => s"<$name>"
    case Free(name, _) => s"'$name'"
    case Var(indexname, _) => s"?${nameExtractor.prettyPrint(indexname)}"
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

  /** Loosely checks if two terms are semantically equal.
    *
    * @param t1 first term
    * @param t2 second term
    * @return true if they are semantically equal, and false if they are not or too complex to unify
    */
  def semanticEqual(t1: Term, t2: Term): Boolean = {
    (minimize(t1), minimize(t2)) match {
      case (Abs(_, typ1, term1), Abs(_, typ2, term2)) => typ1 == typ2 && semanticEqual(term1, term2)
      case (App(fun1, arg1), App(fun2, arg2)) => semanticEqual(fun1, fun2) && semanticEqual(arg1, arg2)
      case (t1, t2) => t1 == t2
    }
  }
}
