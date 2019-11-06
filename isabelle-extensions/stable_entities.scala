object EntityWrapper {
  /* Simplification of:
  sealed abstract class Typ
  case class Type(name: String, args: List[Typ] = Nil) extends Typ
  case class TFree(name: String, sort: Sort = Nil) extends Typ
  case class TVar(name: Indexname, sort: Sort = Nil) extends Typ */

  type Typ = String

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

  @SerialVersionUID(-6600248326866346277L)
  sealed case class Entity(name: String, id: Long, startPos: Int, endPos: Int) extends Serializable

  @SerialVersionUID(8214256252769140008L)
  sealed case class Constant(entity: Entity, typargs: Array[String], typ: Typ) extends Serializable

  @SerialVersionUID(4245087039579397970L)
  sealed case class Theorem(
      entity: Entity,
      typargs: Array[String],
      args: Array[(String, Typ)],
      term: Term,
      deps: Array[String],
      proof: Proof)
      extends Serializable

  @SerialVersionUID(-7187211222156107352L)
  sealed case class Theory(name: String, consts: Array[Constant], thms: Array[Theorem]) extends Serializable
}
