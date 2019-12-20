package de.qaware.findfacts.theoryimporter.steps.impl.thy

import de.qaware.findfacts.theoryimporter.TheoryView.{Indexname, TFree, TVar, Typ, TypeTyp}
import de.qaware.findfacts.theoryimporter.pure.PureSyntax

class TypExtractor {
  def prettyPrint(idxName: Indexname): String = {
    if (!idxName.name.endsWith("[0-9]"))
      if (idxName.index == 0) idxName.name else s"${idxName.name}${idxName.index}"
    else
      s"${idxName.name}.${idxName.index}"
  }

  def prettyPrint(typ: Typ): String = typ match {
    case TypeTyp(PureSyntax.Fun.name, args) => s"(${args.mkString(" ⇒ ")})"
    case TypeTyp(name, List()) => name
    case TypeTyp(name, List(single)) => s"$single $name"
    case TypeTyp(name, args) => s"(${args.mkString(" ⇒ ")}) $name"

    case TFree(name, List()) => name
    case TFree(name, List(single)) => s"($name::$single)"
    case TFree(name, args) => s"($name::{${args.mkString(",")}})"

    case TVar(name, List()) => prettyPrint(name)
    case TVar(name, List(single)) => s"(${prettyPrint(name)}::$single)"
    case TVar(name, sort) => s"(${prettyPrint(name)}::{${sort.mkString(",")}})"
  }

  def referencedTypes(typ: Typ): Set[String] = typ match {
    case TypeTyp(name, args) => args.toSet.flatMap(referencedTypes) + name
    case TFree(_, sort) => sort.toSet
    case TVar(_, sort) => sort.toSet
  }
}
