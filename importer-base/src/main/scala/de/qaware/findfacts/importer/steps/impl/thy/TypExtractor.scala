package de.qaware.findfacts.importer.steps.impl.thy

import de.qaware.findfacts.importer.TheoryView.{TFree, TVar, Typ, TypeTyp}
import de.qaware.findfacts.importer.steps.impl.pure.PureSyntax

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
    // Functions
    case TypeTyp(PureSyntax.Fun.name, List()) => "()"
    case TypeTyp(PureSyntax.Fun.name, List(single)) => s"() ⇒ ${prettyPrint(single)}"
    case TypeTyp(PureSyntax.Fun.name, args) =>
      (args.dropRight(1).map(prettyPrintFnArg) ++ args.takeRight(1).map(prettyPrint)).mkString(" ⇒ ")

    // Arbitrary c-tors
    case TypeTyp(name, List()) => name
    case TypeTyp(name, List(single)) => s"${prettyPrintCtorArg(single)} $name"
    case TypeTyp(name, args) => s"(${args.map(prettyPrint).mkString(", ")}) $name"

    case TFree(name, sorts) => s"$name${prettyPrintSorts(sorts)}"
    case TVar(indexname, sorts) => s"${nameExtractor.prettyPrint(indexname)}${prettyPrintSorts(sorts)}"
  }

  private def prettyPrintFnArg(typ: Typ): String = typ match {
    case TypeTyp(PureSyntax.Fun.name, _ :: _) => s"(${prettyPrint(typ)})"
    case _ => prettyPrint(typ)
  }

  private def prettyPrintCtorArg(typ: Typ): String = typ match {
    case TypeTyp(PureSyntax.Fun.name, List()) | TypeTyp(_, List()) => prettyPrint(typ)
    case TypeTyp(PureSyntax.Fun.name, _) | TypeTyp(_, _) | TFree(_, List(_)) | TVar(_, List(_)) =>
      s"(${prettyPrint(typ)})"
    case _ => prettyPrint(typ)
  }

  private def prettyPrintSorts(sorts: List[String]): String = sorts match {
    case List() => ""
    case List(single) => s" :: $single"
    case List(args) => s" :: {${args.mkString(", ")}}"
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
