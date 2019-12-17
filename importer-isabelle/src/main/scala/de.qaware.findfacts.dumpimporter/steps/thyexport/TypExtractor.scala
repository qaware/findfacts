package de.qaware.findfacts.dumpimporter.steps.thyexport

import de.qaware.findfacts.dumpimporter.pure.PureSyntax
import isabelle.Term
import isabelle.Term.{Indexname, Typ}

class TypExtractor {
  def prettyPrint(idxName: Indexname): String = {
    if (!idxName.name.endsWith("[0-9]"))
      if (idxName.index == 0) idxName.name else s"${idxName.name}${idxName.index}"
    else
      s"${idxName.name}.${idxName.index}"
  }

  def prettyPrint(typ: Typ): String = typ match {
    case Term.Type(PureSyntax.Fun.name, args) => s"(${args.mkString(" ⇒ ")})"
    case Term.Type(name, List()) => name
    case Term.Type(name, List(single)) => s"$single $name"
    case Term.Type(name, args) => s"(${args.mkString(" ⇒ ")}) $name"

    case Term.TFree(name, List()) => name
    case Term.TFree(name, List(single)) => s"($name::$single)"
    case Term.TFree(name, args) => s"($name::{${args.mkString(",")}})"

    case Term.TVar(name, List()) => prettyPrint(name)
    case Term.TVar(name, List(single)) => s"(${prettyPrint(name)}::$single)"
    case Term.TVar(name, sort) =>s"(${prettyPrint(name)}::{${sort.mkString(",")}})"
  }

  def referencedTypes(typ: Typ): Set[String] = typ match {
    case Term.Type(name, args) => args.toSet.flatMap(referencedTypes) + name
    case Term.TFree(_, sort) => sort.toSet
    case Term.TVar(_, sort) => sort.toSet
  }
}
