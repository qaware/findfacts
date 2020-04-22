theory Executor_Test_Spec imports Main
begin

(*SPEC:BEGIN:Check test built by framework*)
value "''test''"
(*SPEC:VERIFY
    ctx.name should equal("Check test built by framework")
    ctx.src should equal("value \"''test''\"\n")
    ctx.startLine should be(ctx.block.startLine)

    ctx.block.theory should equal("Spec-Tests.Executor_Test_Spec")
    ctx.block.src should equal("value \"''test''\"\n")
    ctx.block.command should equal("value")
    ctx.block.entities should be(empty)
SPEC:END*)

(*SPEC:BEGIN:Check values imported from context*)
fun test where
  "test _ = undefined"
(*SPEC:VERIFY
    block should be(ctx.block)
    entities should be(block.entities)
    constants should contain theSameElementsAs entities.filter(_.kind == Kind.Constant)
    facts should contain theSameElementsAs entities.filter(_.kind == Kind.Fact)
    types should contain theSameElementsAs entities.filter(_.kind == Kind.Type)
    blocks should contain (block)
    blocks should have size 2
SPEC:END*)

end