package de.qaware.dumpimporter.steps.thyexport

// scalastyle:off
object IsabelleEntities {
  @SerialVersionUID(5381987416567122528L)
  final case class Indexname(name: String, index: Int)

  type Class = String
  type Sort = Array[Class]

  sealed abstract class Typ
  @SerialVersionUID(1288274563903406362L)
  final case class TProd(name: String, args: Array[Typ]) extends Typ {
    override def toString: String = s"$name(${args.mkString(",")})"
  }
  @SerialVersionUID(4632215889499084620L)
  final case class TFree(name: String, sort: Sort) extends Typ {
    override def toString: String = {
      if (sort.isEmpty) {
        name
      } else {
        s"($name<:${sort.mkString("+")})"
      }
    }
  }
  @SerialVersionUID(-7154596930660649505L)
  final case class TVar(name: Indexname, sort: Sort) extends Typ {
    override def toString: String = {
      if (sort.isEmpty) {
        s"${name.name}_${name.index}"
      } else {
        s"(${name.name}_${name.index}<:${sort.mkString("+")})"
      }
    }
  }

  sealed abstract class Term
  @SerialVersionUID(-4831700261800560597L)
  final case class Constref(name: String, typargs: Array[Typ]) extends Term {
    override def toString: String = name
  }
  @SerialVersionUID(3537669541329937639L)
  final case class Free(name: String, typ: Typ) extends Term {
    override def toString: String = name
  }
  @SerialVersionUID(1681230165060449254L)
  final case class Var(name: Indexname, typ: Typ) extends Term {
    override def toString: String = name.toString
  }
  @SerialVersionUID(-3254303285314576246L)
  final case class Bound(index: Int) extends Term {
    override def toString: String = s"B($index)"
  }
  @SerialVersionUID(2356134117982949924L)
  final case class Abs(name: String, typ: Typ, body: Term) extends Term {
    override def toString: String = s"λ$name.$body"
  }
  @SerialVersionUID(-9129660572822677999L)
  final case class App(fun: Term, arg: Term) extends Term {
    override def toString: String = s"$fun ($arg)"
  }

  sealed abstract class Proof
  @SerialVersionUID(1079683092354666971L)
  case object MinProof extends Proof
  @SerialVersionUID(-3328238238320029042L)
  final case class PBound(index: Int) extends Proof
  @SerialVersionUID(7965969198978870226L)
  final case class Abst(name: String, typ: Typ, body: Proof) extends Proof
  @SerialVersionUID(-8854245495727580719L)
  final case class AbsP(name: String, hyp: Term, body: Proof) extends Proof
  @SerialVersionUID(5652081705570356466L)
  final case class Appt(fun: Proof, arg: Term) extends Proof
  @SerialVersionUID(4810161010890760247L)
  final case class AppP(fun: Proof, arg: Proof) extends Proof
  @SerialVersionUID(-1748681562027671903L)
  final case class Hyp(hyp: Term) extends Proof
  @SerialVersionUID(4640219576157011277L)
  final case class PAxm(name: String, types: Array[Typ]) extends Proof
  @SerialVersionUID(-6025628329710835720L)
  final case class OfClass(typ: Typ, cls: Class) extends Proof
  @SerialVersionUID(27731834100826277L)
  final case class Oracle(name: String, prop: Term, types: Array[Typ]) extends Proof
  @SerialVersionUID(-2270750669329013837L)
  final case class PThm(serial: Long, theoryName: String, name: String, types: Array[Typ]) extends Proof

  @SerialVersionUID(-4468468967588042461L)
  final case class Entity(name: String, localName: String, serial: Long, startPos: Int, endPos: Int)

  @SerialVersionUID(-6479946311825844471L)
  final case class Const(entity: Entity, typargs: Array[String], typ: Typ)

  @SerialVersionUID(645511391258153528L)
  final case class Constdef(name: String, axiomName: String)

  @SerialVersionUID(-5240318743403678874L)
  final case class Prop(typargs: Array[(String, Sort)], args: Array[(String, Typ)], term: Term)

  @SerialVersionUID(-6072692905282064541L)
  final case class Axiom(entity: Entity, prop: Prop)

  @SerialVersionUID(1288665339737666662L)
  final case class ThmId(serial: Long, theoryName: String)

  @SerialVersionUID(-5164559171920544926L)
  final case class Thm(entity: Entity, prop: Prop, deps: Array[String], proofBoxes: Array[ThmId], proof: Proof)

  @SerialVersionUID(-3018807716739527586L)
  final case class Type(entity: Entity, args: Array[String])

  @SerialVersionUID(-4034689847239365935L)
  final case class Typedef(
      name: String,
      repType: Typ,
      absType: Typ,
      repName: String,
      absName: String,
      axiomName: String)

  @SerialVersionUID(-8070090537415643108L)
  final case class Theory(
      name: String,
      consts: Array[Const],
      axioms: Array[Axiom],
      thms: Array[Thm],
      types: Array[Type],
      constdefs: Array[Constdef],
      typedefs: Array[Typedef])
}
