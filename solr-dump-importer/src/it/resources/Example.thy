theory Example imports Main
begin

(*SPEC:BEGIN:1*)
fun fun_const :: "nat \\<Rightarrow> 'a" where
  "fun_const 0 = undefined"
| "fun_const (Suc n) = undefined"
(*SPEC:1:VERIFY
entity("1").sourceFile should be ("Example.Example")
entity("1").startPos should be (begin("1"))
entity("1").endPos should be (end("1"))
entity("1").name should be ("fun_const")
entity("1").constType should be ("\"nat \\<Rightarrow> 'a\"")
SPEC:1:END*)

primrec primrec_const :: "nat \<Rightarrow> 'a" where
  "primrec_const (Suc n) = fun_const n"

function function_const where
  "function_const 0 = 0"
| "function_const (Suc n) = n"
  sorry

abbreviation abbreviation_const :: "nat \<Rightarrow> nat" where
  "abbreviation_const n \<equiv> function_const 0"

abbreviation "(abbreviation_const_short :: nat \<Rightarrow> nat) n \<equiv> 0"

definition definition_const where
  "definition_const n \<equiv> n"

definition "definition_const_short n \<equiv> n"

end