package de.qaware.findfacts.theoryimporter

/** Importer's view on isabelle theories. Should be implemented by value classes wrapping isabelle types. */
object TheoryView {

  // scalastyle:off
  trait Prop extends Any {
    def typargs: List[(String, List[String])]
    def args: List[(String, Typ)]
    def term: Term
  }
  object Prop {
    def unapply(arg: Prop): Option[(List[(String, List[String])], List[(String, Typ)], Term)] =
      Some(arg.typargs, arg.args, arg.term)
  }

  trait Indexname extends Any {
    def name: String
    def index: Int
  }
  object Indexname {
    def unapply(arg: Indexname): Option[(String, Int)] = Some(arg.name, arg.index)
  }

  sealed trait Typ extends Any
  trait TypeTyp extends Any with Typ {
    def name: String
    def args: List[Typ]
  }
  object TypeTyp {
    def unapply(arg: TypeTyp): Option[(String, List[Typ])] = Some(arg.name, arg.args)
  }
  trait TFree extends Any with Typ {
    def name: String
    def sort: List[String]
  }
  object TFree {
    def unapply(arg: TFree): Option[(String, List[String])] = Some(arg.name, arg.sort)
  }
  trait TVar extends Any with Typ {
    def name: Indexname
    def sort: List[String]
  }
  object TVar {
    def unapply(arg: TVar): Option[(Indexname, List[String])] = Some(arg.name, arg.sort)
  }

  sealed trait Term extends Any
  trait ConstTerm extends Any with Term {
    def name: String
    def typargs: List[Typ]
  }
  object ConstTerm {
    def unapply(arg: ConstTerm): Option[(String, List[Typ])] = Some(arg.name, arg.typargs)
  }
  trait Free extends Any with Term {
    def name: String
    def typ: Typ
  }
  object Free {
    def unapply(arg: Free): Option[(String, Typ)] = Some(arg.name, arg.typ)
  }
  trait Var extends Any with Term {
    def name: Indexname
    def typ: Typ
  }
  object Var {
    def unapply(arg: Var): Option[(Indexname, Typ)] = Some(arg.name, arg.typ)
  }
  trait Bound extends Any with Term {
    def index: Int
  }
  object Bound {
    def unapply(arg: Bound): Option[Int] = Some(arg.index)
  }
  trait Abs extends Any with Term {
    def name: String
    def typ: Typ
    def body: Term
  }
  object Abs {
    def unapply(arg: Abs): Option[(String, Typ, Term)] = Some(arg.name, arg.typ, arg.body)
  }
  trait App extends Any with Term {
    def fun: Term
    def arg: Term
  }
  object App {
    def unapply(arg: App): Option[(Term, Term)] = Some(arg.fun, arg.arg)
  }

  sealed trait Proof extends Any

  trait MinProof extends Any with Proof

  trait PBound extends Any with Proof {
    def index: Int
  }
  object PBound {
    def unapply(arg: PBound): Option[Int] = Some(arg.index)
  }

  trait Abst extends Any with Proof {
    def name: String
    def typ: Typ
    def body: Proof
  }
  object Abst {
    def unapply(arg: Abst): Option[(String, Typ, Proof)] = Some(arg.name, arg.typ, arg.body)
  }

  trait AbsP extends Any with Proof {
    def name: String
    def hyp: Term
    def body: Proof
  }
  object AbsP {
    def unapply(arg: AbsP): Option[(String, Term, Proof)] = Some(arg.name, arg.hyp, arg.body)
  }

  trait Appt extends Any with Proof {
    def fun: Proof
    def arg: Term
  }
  object Appt {
    def unapply(arg: Appt): Option[(Proof, Term)] = Some(arg.fun, arg.arg)
  }

  trait AppP extends Any with Proof {
    def fun: Proof
    def arg: Proof
  }
  object AppP {
    def unapply(arg: AppP): Option[(Proof, Proof)] = Some(arg.fun, arg.arg)
  }

  trait Hyp extends Any with Proof {
    def hyp: Term
  }
  object Hyp {
    def unapply(arg: Hyp): Option[Term] = Some(arg.hyp)
  }

  trait PAxm extends Any with Proof {
    def name: String
    def types: List[Typ]
  }
  object PAxm {
    def unapply(arg: PAxm): Option[(String, List[Typ])] = Some(arg.name, arg.types)
  }

  trait OfClass extends Any with Proof {
    def typ: Typ
    def cls: String
  }
  object OfClass {
    def unapply(arg: OfClass): Option[(Typ, String)] = Some(arg.typ, arg.cls)
  }

  trait Oracle extends Any with Proof {
    def name: String
    def prop: Term
    def types: List[Typ]
  }
  object Oracle {
    def unapply(arg: Oracle): Option[(String, Term, List[Typ])] = Some(arg.name, arg.prop, arg.types)
  }

  trait PThm extends Any with Proof {
    def serial: Long
    def theoryName: String
    def name: String
    def types: List[Typ]
  }
  object PThm {
    def unapply(arg: PThm): Option[(Long, String, String, List[Typ])] =
      Some(arg.serial, arg.theoryName, arg.name, arg.types)
  }

  trait Position extends Any {
    def offset: Int
    def endOffset: Int
  }
  object Position {
    def unapply(arg: Position): Option[(Int, Int)] = Some(arg.offset, arg.endOffset)
  }

  trait Entity extends Any {
    def name: String
    def pos: Position
    def serial: Long
  }
  object Entity {
    def unapply(arg: Entity): Option[(String, Position, Long)] = Some(arg.name, arg.pos, arg.serial)
  }

  trait Type extends Any {
    def entity: Entity
    def args: List[String]
  }
  object Type {
    def unapply(arg: Type): Option[(Entity, List[String])] = Some(arg.entity, arg.args)
  }

  trait Const extends Any {
    def entity: Entity
    def typargs: List[String]
    def typ: Typ
  }
  object Const {
    def unapply(arg: Const): Option[(Entity, List[String], Typ)] = Some(arg.entity, arg.typargs, arg.typ)
  }

  trait Axiom extends Any {
    def entity: Entity
    def prop: Prop
  }
  object Axiom {
    def unapply(arg: Axiom): Option[(Entity, Prop)] = Some(arg.entity, arg.prop)
  }

  trait Thm extends Any {
    def entity: Entity
    def prop: Prop
    def deps: List[String]
    def proof: Proof
  }
  object Thm {
    def unapply(arg: Thm): Option[(Entity, Prop, List[String], Proof)] = Some(arg.entity, arg.prop, arg.deps, arg.proof)
  }

  trait Constdef extends Any {
    def name: String
    def axiomName: String
  }
  object Constdef {
    def unapply(arg: Constdef): Option[(String, String)] = Some(arg.name, arg.axiomName)
  }

  trait Typedef extends Any {
    def name: String
    def axiomName: String
  }
  object Typedef {
    def unapply(arg: Typedef): Option[(String, String)] = Some(arg.name, arg.axiomName)
  }

  trait Theory extends Any {
    def name: String
    def session: String
    def types: List[Type]
    def consts: List[Const]
    def axioms: List[Axiom]
    def thms: List[Thm]
    def constdefs: List[Constdef]
    def typedefs: List[Typedef]
  }
  object Theory {
    def unapply(arg: Theory)
      : Option[(String, String, List[Type], List[Const], List[Axiom], List[Thm], List[Constdef], List[Typedef])] =
      Some(arg.name, arg.session, arg.types, arg.consts, arg.axioms, arg.thms, arg.constdefs, arg.typedefs)
  }
}
