package de.qaware.findfacts.theoryimporter.steps.impl.thy

import de.qaware.findfacts.theoryimporter.TheoryView.{TFree, TVar, Typ, TypeTyp}
import de.qaware.findfacts.theoryimporter.pure.PureSyntax

/** Extracts and pretty-print types.
  *
  * @param nameExtractor to pretty-print names
  */
class TypExtractor(nameExtractor: NameExtractor) {

  /** Pretty-prints types.
    *
    * @param typ to pretty-print
    * @return pretty string
    */
  def prettyPrint(typ: Typ): String = typ match {
    case TypeTyp(PureSyntax.Fun.name, args) => s"(${args.map(prettyPrint).mkString(" ⇒ ")})"
    case TypeTyp(name, List()) => name
    case TypeTyp(name, List(single)) => s"${prettyPrint(single)} $name"
    case TypeTyp(name, args) => s"(${args.map(prettyPrint).mkString(" ⇒ ")}) $name"

    case TFree(name, List()) => name
    case TFree(name, List(single)) => s"($name::$single)"
    case TFree(name, args) => s"($name::{${args.mkString(",")}})"

    case TVar(name, List()) => nameExtractor.prettyPrint(name)
    case TVar(name, List(single)) => s"(${nameExtractor.prettyPrint(name)}::$single)"
    case TVar(name, sort) => s"(${nameExtractor.prettyPrint(name)}::{${sort.mkString(",")}})"
  }

  /** Finds types referenced by this type.
    *
    * @param typ to find referenced types for
    * @return set of referenced type names
    */
  def referencedTypes(typ: Typ): Set[String] = typ match {
    case TypeTyp(name, args) => args.toSet.flatMap(referencedTypes) + name
    case TFree(_, sort) => sort.toSet
    case TVar(_, sort) => sort.toSet
  }
}
