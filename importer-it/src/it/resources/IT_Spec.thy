theory IT_Spec imports Main
begin

(*SPEC:BEGIN:Check comment*)
(* This is a comment *)
(*SPEC:VERIFY
    command should equal("(" + "*")
    entities should be(empty)
SPEC:END*)

(*SPEC:BEGIN:Check latex element*)
section\<open>A section\<close>
(*SPEC:VERIFY
    command should equal("section")
    entities should be (empty)
SPEC:END*)

(*SPEC:BEGIN:Check datatype and constructors*)
datatype 'a li = N | C 'a "'a li"
(*SPEC:VERIFY
    command should equal("datatype")

    // Should define the "li" type
    types.map(_.name) should contain ("IT_Spec.li")
    val typ = types.find(_.name == "IT_Spec.li").get

    // Should define two type c-tors
    constants.map(_.name) should contain allOf("IT_Spec.li.N", "IT_Spec.li.C")

    // Check c-tors
    val cCtor = constants.find(_.name == "IT_Spec.li.C").get
    cCtor.constantType should equal ("'a ⇒ 'a IT_Spec.li ⇒ 'a IT_Spec.li")
    cCtor.uses should contain ("Type.IT_Spec.li")

    val nCtor = constants.find(_.name == "IT_Spec.li.N").get
    nCtor.constantType should equal("'a IT_Spec.li")
    nCtor.uses should contain ("Type.IT_Spec.li")

    // There should be an constant that uses type constructors and self type
    atLeast(1, constants.map(_.uses)) should contain allOf("Type.IT_Spec.li", "Constant.IT_Spec.li.C", "Constant.IT_Spec.li.N")
SPEC:END*)

(*SPEC:BEGIN:Check value*)
value "C (1::nat) N"
(*SPEC:VERIFY
    command should equal("value")
    entities should be(empty)
SPEC:END*)

(*SPEC:BEGIN:Check type synonym*)
type_synonym nLi = "nat li"
(*SPEC:VERIFY
    command should equal("type_synonym")

    // Should define a single entity for synonym
    entities should have size 1

    // Entity should be a type
    types should have size 1
    val t = types.head
    t.name should equal("IT_Spec.nLi")

    // Type should use "li" and "nat"
    t.uses should contain theSameElementsAs Seq("Type.IT_Spec.li", "Type.Nat.nat")
SPEC:END*)

(*SPEC:BEGIN:Check fun*)
fun app where
  "app N a = C a N"
| "app (C l ls) a = C l (app ls a)"
(*SPEC:VERIFY
    command should equal("fun")

    // "Fun" creates lots of constants, but there should at least be one with the HOL type, and one for the function
    constants.size should be > 1
    constants.map(_.name) should contain("IT_Spec.app")
    constants.map(_.constantType) should contain ("'a IT_Spec.li ⇒ 'a ⇒ 'a IT_Spec.li")

    // Check uses of a constant
    atLeast(1, constants.map(_.uses)) should contain allOf("Type.IT_Spec.li", "Constant.IT_Spec.li.N", "Constant.IT_Spec.li.C")
SPEC:END*)

(*SPEC:BEGIN:Check primrec and const dependencies*)
primrec rev :: "'a li \<Rightarrow> 'a li" where
  "rev N = N"
| "rev (C x xs) = app (rev xs) x"
(*SPEC:VERIFY
    command should equal("primrec")

    // Check constant
    constants should have size 1
    val const = constants.head
    const.name should equal("IT_Spec.rev")
    const.constantType should equal("'a IT_Spec.li ⇒ 'a IT_Spec.li")
    // "Constant.IT_Spec.li.C" is at argument position and thus not present here
    const.uses should contain allOf("Type.IT_Spec.li", "Constant.IT_Spec.li.N", "Constant.IT_Spec.app")
SPEC:END*)

(*SPEC:BEGIN:Check short lemma and proposition dependencies*)
lemma app_rev [simp]: "app (rev xs) x = rev (C x xs)"
  by (induction xs) auto
(*SPEC:VERIFY
    command should equal("lemma")

    // Check fact
    entities should have size 1
    facts should have size 1
    val fact = facts.head
    fact.name should equal("IT_Spec.app_rev")
    fact.uses should contain allOf("Constant.IT_Spec.app", "Constant.IT_Spec.rev")

    // Check implicit proposition dependencies
    fact.uses should contain ("Type.IT_Spec.li")
SPEC:END*)

(*SPEC:BEGIN:Check long Isar-style theorem and proof dependencies*)
theorem
  assumes "app xs x = C x xs"
  shows "rev xs = xs"
proof -
  from assms show ?thesis
  proof (induction xs)
    case (C x1 xs)
    then show ?case by simp
  qed (simp)
qed
(*SPEC:VERIFY
    command should equal("theorem")

    // Check that the whole proof is in src
    block.src should include ("qed (simp)")

    // Check fact
    entities should have size 1
    facts should have size 1
    val fact = facts.head
    fact.name should not be(empty)
    fact.uses should contain allOf("Type.IT_Spec.li", "Constant.IT_Spec.app", "Constant.IT_Spec.rev")

    // Check proof-only dependency
    fact.uses should contain ("Fact.IT_Spec.app_rev")
SPEC:END*)

(*SPEC:BEGIN:Check definition and HOL dependency*)
definition"palindrome \<equiv> \<lambda> xs. rev xs = xs"
(*SPEC:VERIFY
    command should equal("definition")
    
    // Check definition
    constants should have size 1
    val const = constants.head
    const.name should equal("IT_Spec.palindrome")
    const.constantType should equal("'a IT_Spec.li ⇒ HOL.bool")
    const.uses should contain allOf("Type.IT_Spec.li", "Constant.IT_Spec.rev")
    
    // Check HOL usage
    const.uses should contain("Type.HOL.bool")
SPEC:END*)

end