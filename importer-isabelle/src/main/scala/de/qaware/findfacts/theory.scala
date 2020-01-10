/*  Title:      findfacts/theory.scala
    Author:     Fabian Huch, TU Munich/QAware GmbH

Isabelle Export_Theory -> findfacts TheoryView mapping.
*/

package de.qaware.findfacts


import _root_.scala.language.implicitConversions
import de.qaware.findfacts.theoryimporter.TheoryView
import de.qaware.findfacts.theoryimporter.TheoryView.Source
import isabelle._


object Theory
{

  /* mappers for sealed traits */

  implicit def map_term(term: isabelle.Term.Term): TheoryView.Term = term match
  {
    case t: Term.Const => t
    case t: Term.Free => t
    case t: Term.Var => t
    case t: Term.Bound => t
    case t: Term.Abs => t
    case t: Term.App => t
  }

  implicit def map_typ(typ: isabelle.Term.Typ): TheoryView.Typ = typ match
  {
    case t: Term.Type => t
    case t: Term.TFree => t
    case t: Term.TVar => t
  }

  implicit def map_proof(proof: isabelle.Term.Proof): TheoryView.Proof = proof match
  {
    case p: Term.MinProof.type => p
    case p: Term.PBound => p
    case p: Term.Abst => p
    case p: Term.AbsP => p
    case p: Term.Appt => p
    case p: Term.AppP => p
    case p: Term.Hyp => p
    case p: Term.PAxm => p
    case p: Term.OfClass => p
    case p: Term.Oracle => p
    case p: Term.PThm => p
  }

  /* implicit value classes as mappers for concrete types */

  implicit class Prop_Wrapper(val inner: Export_Theory.Prop) extends AnyVal with TheoryView.Prop
  {
    override def typargs: List[(String, List[String])] = inner.typargs
    override def args: List[(String, TheoryView.Typ)] = inner.args map
    {
      case (s, t) => (s, map_typ(t))
    }
    override def term: TheoryView.Term = inner.term
  }

  implicit class Indexname_Wrapper(val inner: Term.Indexname) extends AnyVal with TheoryView.Indexname
  {
    override def name: String = inner.name
    override def index: Int = inner.index
  }

  implicit class TypeTyp_Wrapper(val inner: Term.Type) extends AnyVal with TheoryView.TypeTyp
  {
    override def name: String = inner.name
    override def args: List[TheoryView.Typ] = inner.args.map(map_typ)
  }

  implicit class TFree_Wrapper(val inner: Term.TFree) extends AnyVal with TheoryView.TFree
  {
    override def name: String = inner.name
    override def sort: List[String] = inner.sort
  }

  implicit class TVar_Wrapper(val inner: Term.TVar) extends AnyVal with TheoryView.TVar
  {
    override def name: TheoryView.Indexname = inner.name
    override def sort: List[String] = inner.sort
  }

  implicit class ConstTerm_Wrapper(val inner: Term.Const) extends AnyVal with TheoryView.ConstTerm
  {
    override def name: String = inner.name
    override def typargs: List[TheoryView.Typ] = inner.typargs.map(map_typ)
  }

  implicit class Free_Wrapper(val inner: Term.Free) extends AnyVal with TheoryView.Free
  {
    override def name: String = inner.name
    override def typ: TheoryView.Typ = inner.typ
  }

  implicit class Var_Wrapper(val inner: Term.Var) extends AnyVal with TheoryView.Var
  {
    override def name: TheoryView.Indexname = inner.name
    override def typ: TheoryView.Typ = inner.typ
  }

  implicit class Bound_Wrapper(val inner: Term.Bound) extends AnyVal with TheoryView.Bound
  {
    override def index: Int = inner.index
  }

  implicit class Abs_Wrapper(val inner: Term.Abs) extends AnyVal with TheoryView.Abs
  {
    override def name: String = inner.name
    override def typ: TheoryView.Typ = inner.typ
    override def body: TheoryView.Term = inner.body
  }

  implicit class App_Wrapper(val inner: Term.App) extends AnyVal with TheoryView.App
  {
    override def fun: TheoryView.Term = inner.fun
    override def arg: TheoryView.Term = inner.arg
  }

  implicit class MinProof_Wrapper(val inner: Term.MinProof.type) extends AnyVal with TheoryView.MinProof

  implicit class PBound_Wrapper(val inner: Term.PBound) extends AnyVal with TheoryView.PBound
  {
    override def index: Int = inner.index
  }

  implicit class Abst_Wrapper(val inner: Term.Abst) extends AnyVal with TheoryView.Abst
  {
    override def name: String = inner.name
    override def typ: TheoryView.Typ = inner.typ
    override def body: TheoryView.Proof = inner.body
  }

  implicit class AbsP_Wrapper(val inner: Term.AbsP) extends AnyVal with TheoryView.AbsP
  {
    override def name: String = inner.name
    override def hyp: TheoryView.Term = inner.hyp
    override def body: TheoryView.Proof = inner.body
  }

  implicit class Appt_Wrapper(val inner: Term.Appt) extends AnyVal with TheoryView.Appt
  {
    override def fun: TheoryView.Proof = inner.fun
    override def arg: TheoryView.Term = inner.arg
  }

  implicit class AppP_Wrapper(val inner: Term.AppP) extends AnyVal with TheoryView.AppP
  {
    override def fun: TheoryView.Proof = inner.fun
    override def arg: TheoryView.Proof = inner.arg
  }

  implicit class Hyp_Wrapper(val inner: Term.Hyp) extends AnyVal with TheoryView.Hyp
  {
    override def hyp: TheoryView.Term = inner.hyp
  }

  implicit class PAxm_Wrapper(val inner: Term.PAxm) extends AnyVal with TheoryView.PAxm
  {
    override def name: String = inner.name
    override def types: List[TheoryView.Typ] = inner.types.map(map_typ)
  }

  implicit class OfClass_Wrapper(val inner: Term.OfClass) extends AnyVal with TheoryView.OfClass
  {
    override def typ: TheoryView.Typ = inner.typ
    override def cls: String = inner.cls
  }

  implicit class Oracle_Wrapper(val inner: Term.Oracle) extends AnyVal with TheoryView.Oracle
  {
    override def name: String = inner.name
    override def prop: TheoryView.Term = inner.prop
    override def types: List[TheoryView.Typ] = inner.types.map(map_typ)
  }

  implicit class PThm_Wrapper(val inner: Term.PThm) extends AnyVal with TheoryView.PThm
  {
    override def theoryName: String = inner.theory_name
    override def name: String = inner.name
    override def types: List[TheoryView.Typ] = inner.types.map(map_typ)
  }

  implicit class Position_Wrapper(val inner: isabelle.Position.T) extends AnyVal with TheoryView.Position
  {
    override def offset: Int = Properties.get(inner, Markup.OFFSET).get.toInt
    override def endOffset: Int = Properties.get(inner, Markup.END_OFFSET).get.toInt
  }

  implicit class Entity_Wrapper(val inner: Export_Theory.Entity) extends AnyVal with TheoryView.Entity
  {
    override def name: String = inner.name
    override def pos: TheoryView.Position = inner.pos
  }

  implicit class Type_Wrapper(val inner: Export_Theory.Type) extends AnyVal with TheoryView.Type
  {
    override def entity: TheoryView.Entity = inner.entity
    override def args: List[String] = inner.args
  }

  implicit class Const_Wrapper(val inner: Export_Theory.Const) extends AnyVal with TheoryView.Const
  {
    override def entity: TheoryView.Entity = inner.entity
    override def typargs: List[String] = inner.typargs
    override def typ: TheoryView.Typ = inner.typ
  }

  implicit class Axiom_Wrapper(val inner: Export_Theory.Axiom) extends AnyVal with TheoryView.Axiom
  {
    override def entity: TheoryView.Entity = inner.entity
    override def prop: TheoryView.Prop = inner.prop
  }

  implicit class Thm_Wrapper(val inner: Export_Theory.Thm) extends AnyVal with TheoryView.Thm
  {
    override def entity: TheoryView.Entity = inner.entity
    override def prop: TheoryView.Prop = inner.prop
    override def deps: List[String] = inner.deps
    override def proof: TheoryView.Proof = inner.proof
  }

  implicit class Constdef_Wrapper(val inner: Export_Theory.Constdef) extends AnyVal with TheoryView.Constdef
  {
    override def name: String = inner.name
    override def axiomName: String = inner.axiom_name
  }

  implicit class Typedef_Wrapper(val inner: Export_Theory.Typedef) extends AnyVal with TheoryView.Typedef
  {
    override def name: String = inner.name
    override def axiomName: String = inner.axiom_name
  }

  implicit class Block_wrapper(val inner: Markup_Blocks.Block) extends AnyVal with TheoryView.Block
  {
    override def offset: Int = inner.range.start
    override def endOffset: Int = inner.range.stop
    override def text: String = inner.body
  }

  implicit class Source_Wrapper(val inner: Markup_Blocks) extends AnyVal with TheoryView.Source
  {
    override def get(offset: Int, endOffset: Int): Option[TheoryView.Block] =
      inner.get_containing(Text.Range(offset, endOffset)).map(Block_wrapper)
  }

  def map_theory(
    session_name: String,
    isabelle_theory: Export_Theory.Theory,
    markup_Blocks: Markup_Blocks): TheoryView.Theory =
  {

    new TheoryView.Theory
    {
      override val name: String = isabelle_theory.name
      override val session: String = session_name
      override def source: Source = markup_Blocks
      override def types: List[TheoryView.Type] = isabelle_theory.types.map(Type_Wrapper)
      override def consts: List[TheoryView.Const] = isabelle_theory.consts.map(Const_Wrapper)
      override def axioms: List[TheoryView.Axiom] = isabelle_theory.axioms.map(Axiom_Wrapper)
      override def thms: List[TheoryView.Thm] = isabelle_theory.thms.map(Thm_Wrapper)
      override def constdefs: List[TheoryView.Constdef] = isabelle_theory.constdefs.map(Constdef_Wrapper)
      override def typedefs: List[TheoryView.Typedef] = isabelle_theory.typedefs.map(Typedef_Wrapper)
    }
  }
}
