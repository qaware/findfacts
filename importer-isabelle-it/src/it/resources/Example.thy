theory Example imports Main
begin

(*SPEC:BEGIN:Test fun*)
fun fun_const :: "nat \<Rightarrow> 'a" where
  "fun_const 0 = undefined"
| "fun_const (Suc n) = fun_const n"
(*SPEC:VERIFY
ctx.block.sourceFile should be ("Example.Example")
ctx.block.startLine should be (ctx.startLine)
ctx.block.entities.map(_.name) should contain ("fun_const")
SPEC:END *)

lemma "fun_const 0 = fun_const 1"
  apply (rule sym)
  apply (auto)
  done

lemma somlem: "fun_const 1 = fun_const 2"
    apply (rule sym)
  apply (auto)
  by (simp add: numeral_2_eq_2)

primrec primrec_const :: "nat \<Rightarrow> 'a" where
  "primrec_const (Suc n) = fun_const n"

function function_const where
  "function_const 0 = 0"
| "function_const (Suc n) = n"
     apply (auto)
    using fun_const.cases by auto

abbreviation abbreviation_const :: "nat \<Rightarrow> nat" where
  "abbreviation_const n \<equiv> function_const 0"

abbreviation "(abbreviation_const_short :: nat \<Rightarrow> nat) n \<equiv> 0"

definition definition_const where
  "definition_const n \<equiv> n"

definition "definition_const_short n \<equiv> n"

end