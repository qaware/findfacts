object EntityWrapper
{
  /* Simplification of:
  sealed abstract class Typ
  case class Type(name: String, args: List[Typ] = Nil) extends Typ
  case class TFree(name: String, sort: Sort = Nil) extends Typ
  case class TVar(name: Indexname, sort: Sort = Nil) extends Typ */
  type Typ = String
  /* Simplification of:
  type Class = String
  type Sort = List[Class] */
  type Sort = String
  /* Simplification of:
   sealed abstract class Term
   case class Const(name: String, typargs: List[Typ] = Nil) extends Term
   case class Free(name: String, typ: Typ) extends Term
   case class Var(name: Indexname, typ: Typ) extends Term
   case class Bound(index: Int) extends Term
   case class Abs(name: String, typ: Typ, body: Term) extends Term
   case class App(fun: Term, arg: Term) extends Term */
  type Term = String
  /* Simplification of:
  sealed abstract class Proof
  case object MinProof extends Proof
  case class PBound(index: Int) extends Proof
  case class Abst(name: String, typ: Typ, body: Proof) extends Proof
  case class AbsP(name: String, hyp: Term, body: Proof) extends Proof
  case class Appt(fun: Proof, arg: Term) extends Proof
  case class AppP(fun: Proof, arg: Proof) extends Proof
  case class Hyp(hyp: Term) extends Proof
  case class PAxm(name: String, types: List[Typ]) extends Proof
  case class OfClass(typ: Typ, cls: Class) extends Proof
  case class Oracle(name: String, prop: Term, types: List[Typ]) extends Proof
  case class PThm(serial: Long, theory_name: String, name: String, types: List[Typ]) extends Proof */
  type Proof = String

  @SerialVersionUID(- 4468468967588042461L)
  sealed case class Entity(name: String, localName: String, serial: Long, startPos: Int, endPos: Int)

  @SerialVersionUID(-6479946311825844471L)
  sealed case class Const(entity: Entity, typargs: Array[String], typ: Typ)

  @SerialVersionUID(645511391258153528L)
  sealed case class Constdef(name: String, axiomName: String)

  @SerialVersionUID(-6072692905282064541L)
  sealed case class Axiom(entity: Entity, prop: Prop)

  @SerialVersionUID(-5240318743403678874L)
  sealed case class Prop(typargs: Array[(String, Sort)], args: Array[(String, Typ)], term: Term)

  @SerialVersionUID(3258375583870971926L)
  sealed case class ThmId(serial: Long, theory_name: String)

  @SerialVersionUID(-5164559171920544926L)
  sealed case class Thm(entity: Entity, prop: Prop, deps: Array[String], proofBoxes: Array[ThmId], proof: Proof)

  @SerialVersionUID(-8070090537415643108L)
  sealed case class Theory(
      name: String,
      consts: Array[Const],
      axioms: Array[Axiom],
      thms: Array[Thm],
      constdefs: Array[Constdef])
}
