# Reviewing the Rocq-verified SSA loop passes

This guide says how to review the empty-loop deletion and bounds-check
elimination passes of the SSA backend (`-ssa-delete-empty-loops`,
`-ssa-bounds-check-elim`) and the Rocq development behind them. Sections 1
and 2 are about every pass: how the execution of SSA code is reasoned
about, and what remains to be checked by eye because no proof covers it.
Section 3 is what the loop passes share; sections 4 and 5 say, for each
pass, what is claimed, how the proof is put together and how the theorem
carries over to the rewrite. `rocq/HACKING.md` is the companion page for
changing the models; this one is for reading them.

The passes are `backend/ssa/loop/delete_empty_loops.ml` and
`backend/ssa/loop/bounds_check_elim.ml`. Their decisions, which loops to
bypass and which checks to remove, are the OCaml extracted from
`DeleteEmptyLoops.v` and `BoundsCheckElim.v` and the models they use
(`backend/ssa/loop/rocq/`, with the generic SSA models in
`backend/ssa/rocq/`, backend-wide definitions in `backend/rocq/` and the
models of OxCaml's own types in `rocq/`). The extracted `.ml`/`.mli` files
are committed next to the models and are the implementation. The rewrite
of the graph once a decision is made is hand-written OCaml.

## 1. How SSA execution is reasoned about

This is the part to read most carefully: it is where the meaning of the
theorems comes from, and no proof can check it against the compiler. The
files are `SsaSemanticsSpec.v` and `SsaContextSpec.v` in
`backend/ssa/rocq/`.

### 1.1 Registers, not expressions

`SsaSemanticsSpec.v` gives a program state three components: the current
block, the instructions of it that remain to be executed, and a register
file. A register is either result `index` of instruction `id`
(`Result id index`) or parameter `index` of a block (`Param b index`);
the register file is a partial map `register -> option Z`. An unbound
register has no value: there are no defaults for instructions that have
not executed.

An operand *reads* the register of its SSA value; it never re-evaluates
the value's defining operation. Only executing an instruction evaluates
an operation (`execute_instruction`: read the operands, apply
`eval_operation`, write the single result register). This is deliberate.
A `value` term of the model carries its defining operation and operands,
so a semantics that evaluated values syntactically would give a meaning
to any term one can write, including terms that correspond to no
instruction of the graph, and theorems about such terms would say nothing
about the program. Instead, `SsaExecutionProofs.v` derives the equation of
a value only where the value is *available*: its definition site dominates
the current point (`available_value`), so by the frame lemmas its register
was written by that instruction and not since. These derived equations
(`coherent_at`) are what the affine and counter proofs use.

### 1.2 One deterministic step

`step` executes one instruction (`Step_instruction`) or one terminator.
`Step_goto` reads every argument first, then binds the target's
parameters simultaneously, leaving a parameter unbound when its argument
is `Omitted_since_unused`. `Step_switch` reads the index, requires it to
be non-negative and in range, and enters the selected target, which must
have no parameters. Arithmetic wraps at 64 bits (`wrap64`), comparisons
are `IntegerComparison.eval`, and a comparison result is 0 or 1. There is
no rule for anything else: no return, call, load, store, allocation or
trap marker. A state to which no rule applies is stuck, and a stuck
finite execution is not a successful one.

`step` is deterministic (`step_deterministic`), so a statement such as
`steps_until exit s (enter exit regs)` describes the unique execution
from `s`. `steps_until target` means "the execution first enters a state
of block `target`": zero or more steps from states of other blocks, then
a state of `target`. This is what makes "bypass the loop" (section 4.1)
meaningful without mentioning the loop: which blocks the execution visits
before the exit is irrelevant.

### 1.3 Code outside the modelled fragment is observed

A pass's theorem speaks exactly, through `step`, about a fragment of the
graph whose every instruction is whitelisted: the body of a loop for the
empty-loop deletion (section 4.3), and nothing at all for the bounds-check
elimination (section 5.3). The code around the fragment, from the
function's entry onwards, is another matter: it contains loads, calls,
allocations, trap pushes and whatever else the function does, and
modelling all of that would need a memory and a calling convention.
`SsaContextSpec.v` avoids this with a second relation, `context_step`, an
over-approximation of what the machine does to registers:

- an exactly modelled instruction, a `Goto` or a `Switch` takes a `step`;
- any other instruction (`Context_opaque_instruction`) writes arbitrary
  in-range words to exactly its declared result registers and leaves
  every other register alone; its memory effects are left unconstrained
  rather than modelled as no-ops, which suffices because no conclusion of
  the proofs concerns memory and the exactly executed code reads none;
- any other terminator (`Context_opaque_transfer`) jumps to any of the
  block's successors, the exception handler of a `Call` or `Raise`
  included, writing arbitrary words to the target's parameters;
- `Push_trap` and `Pop_trap` step without touching a register.

Every instruction and terminator of a finished graph has one of these
rules, except an exactly modelled operation with other than one result,
which the extracted gate rejects for the whole graph (section 3.5). So
every real execution is represented by some `context_steps` run, and the
input contract of a pass can require one (sections 4.6 and 5.4). The
theorems of `SsaContextProofs.v` establish, once for exact and opaque steps
alike, that the observations stay bounded words, that available modelled
values satisfy their equations (`context_execution_values_coherent`) and
that every register live at the reached point is bound
(`context_execution_live_registers_defined`). Opaque results carry no
equation; the affine analysis sees them as atoms.

## 2. What must be checked by inspection

Rocq checks the proofs. It cannot check that the definitions mean what
the compiler does. The following are the places where that has to be
done by reading; the loop passes add the gate of section 3.5 and their
input contracts (sections 4.6 and 5.4).

### 2.1 The semantics against the backend

In `SsaSemanticsSpec.v`: `eval_operation` (64-bit wrapping addition and
subtraction, `Const_int` through `Int64.of_nativeint`, the shifts and
`Ior` used for bounds), `comparison_value` with
`backend/rocq/IntegerComparison.v` (signed and unsigned comparisons, in
the constructor order of `typing/scalar.ml`), the untagged 0/1
result of a comparison, and `eval_specific`: a target-specific operation
computes the linear combination of its arguments that the target's
`Operation.specific_linear` gives for it, wrapped to 64 bits, and is
opaque when the target gives none. That reading is trusted (section
2.3); it is how the re-tagging of a `for` loop's counter is
understood. `Step_goto` and `Step_switch` against
`backend/ssa/ssa.mli`: all arguments read before any parameter is bound,
omitted arguments leave the parameter unbound, `Switch` selects by a
non-negative in-range index and enters a parameterless block. `wrap64`,
`unsigned64` and `in_int64` in `rocq/OxCamlInt64.v` are shared by the
axioms and by the semantics, so a mistake in them would not be caught by
any proof.

The whitelist `supported_operation` is extracted and run; its arity
checks matter because a malformed instruction must not get a totality
guarantee. Result counts are observed (`Array.length typ` in the
extraction) but machine types are not: the proof relies on the compiler's
typing conventions for the operations it models.

### 2.2 The graph contract: `finished_graph`

Each field of `SsaProgressSpec.finished_graph` is something `Ssa.finish_graph`
(`backend/ssa/ssa.ml`) is believed to guarantee of the graph the pass
receives. Where to look:

| Field | Meaning | Code to compare with |
|---|---|---|
| `graph_well_formed` (`SsaExecutionSpec.well_formed_ssa`) | every instruction has one definition site; instruction operands have strictly earlier dominating definitions and terminator operands dominating ones; parameters are distinct and canonical; blocks are reachable and closed under successors | `check_value_invariants`, `check_terminator`, `compute_reachability_and_trap_stacks` in `ssa.ml` |
| `graph_predecessors_closed`, `graph_edges` (`SsaGraphSpec.control_edges`) | predecessors, `all_successors` and terminators describe the same edges, the exception successor of a `Call` or `Raise` included | `finish_graph` and the predecessor and successor tables it fills |
| `graph_switch_edges` (`switch_edge_multiplicity`) | a `Switch` with a repeated target produces repeated predecessor entries | the same |
| `graph_dominance`, `graph_dominance_complete` | `Block.dominates` answers true exactly when every path from the entry passes through the dominator | `compute_dominators`, `Block.dominates` |
| `graph_strict_dominators` | `Block.strict_dominators` lists distinct blocks of the graph | `Block.strict_dominators` (the walk up the immediate dominators) |
| `graph_layout` (`control_layout`) | `Goto` arguments match the target's parameters in number and order; an omitted argument feeds a parameter no operand anywhere reads; `Switch` targets have no parameters | `check_terminator` |
| `graph_definitions` (`op_def_correct`) | the table maps every instruction to its block | `compute_op_def` in `delete_empty_loops.ml`, shared by `bounds_check_elim.ml` |

Statements such as `control_edges` quantify over the type `block`
without a membership guard. Read `block` as the reachable blocks of one
finished graph, the only ones the pass receives: `finish_graph` prunes the
others, no reachable block refers to them, and `Block.dominates` is not
defined on them (header of `SsaModel.v`).

### 2.3 The extraction directives

The `*Extract.v` files say which OxCaml type or value realises each model
type and primitive, and they are trusted as written:

- `backend/ssa/rocq/SsaModelExtract.v`: `value`, `instr` and
  `Terminator.t` are matched through the OCaml functions given as the
  last argument of their `Extract Inductive`; check that they preserve
  identities, the operation and its operands, operand and target order,
  and the result count. Each `Parameter` observation maps to a function
  of `Ssa` (`Block.predecessors`, `Block.all_successors`,
  `Block.dominates`, `Block.strict_dominators`, `Block.params`,
  `Block.body`, `Block.terminator`), at most wrapped with
  `Array.to_list`; `continuation` and `Terminator.arg` must list their
  constructors in the order of `ssa.mli`.
- `backend/rocq/Operation.v` and `OperationExtract.v`: `Operation.t` and
  `Operation.integer_operation`, copied constructor for constructor and
  mapped back onto the compiler's types. Keep them in step with
  `backend/operation.mli`: a constructor added there must be added to
  both, in the same position, or the extracted matches silently
  misalign. `specific_linear` extracts to the target's
  `Arch.linear_specific_operation`, the one statement about a target's
  semantics the proofs rest on: check against the target's emitter that
  every operation it answers for computes exactly the combination
  `c1 * arg1 + ... + cn * argn + k` of its arguments, wrapped to 64 bits
  (on amd64 the register forms of `Ilea`, on arm64 `Ishiftarith` with a
  non-negative shift), and that it answers `None` for every other.
- `backend/rocq/IntegerComparisonExtract.v`: the constructor order of
  `typing/scalar.ml`.
- `rocq/OxCamlExtract.v`: `int` and `Int64` operations, list and option
  functions, fold argument order, short-circuit booleans.
- `rocq/IntArithExtract.v`: the four overflow tests `add_fits`,
  `mul_fits`, `add_fits64` and `mul_fits64`, a few lines of OCaml each.
- `backend/ssa/loop/rocq/Extract.v`: the entry points (`find_deletions`,
  `find_eliminations` and the affine functions the unit test calls) and
  the literal for `graph_block_limit`.

`make -C rocq all` must reproduce the committed `.ml`/`.mli` exactly.

### 2.4 Axioms and primitives

`Print Assumptions LoopEligibilityProofs.find_deletions_eligible`,
`Print Assumptions BoundsCheckExecutionProofs.find_eliminations_sound` and
`make -s -C rocq assumptions` list them: the axioms of
`rocq/OxCamlInt64.v` (each the documented behaviour of one `Int64`
operation), the equality and set axioms of `SsaModel.v` (identity of
blocks and values, sets ordered by block id), the axioms about the list
functions mapped onto OxCaml's in `rocq/OxCamlList.v`, the four overflow
tests of `rocq/IntArith.v`, and Rocq's own axioms for its primitive
63-bit integers, plus classical logic, used for case splits on
definedness; and the parameter `Operation.specific_linear`, the target's
reading of its specific operations (section 2.3). None of them states
anything about SSA executions.

## 3. The loop passes

Both passes work on the natural loops of a finished graph, and their
decisions rest on the same analyses: `NaturalLoop.v` finds the loops,
`InductionVar.v` their basic induction variables, `AffineSsa.v` the
comparison facts of the branches dominating a block, and
`FourierMotzkin.v` decides what those facts entail. What follows is what
their proofs share; sections 4 and 5 give what is particular to each.

### 3.1 What is claimed

Each pass has one result to review: a closed proposition, bound as a
`Definition` of type `Prop` with the suffix `_statement` at the end of a
`*Spec.v` file in `backend/ssa/loop/rocq/`, and proved under the same name
without the suffix in the matching `*Proofs.v` file. `make -C rocq claims`
checks that each theorem has exactly the statement bound there. For the
empty-loop deletion it is `find_deletions_eligible_statement`
(`LoopEligibilitySpec.v`, section 4.1); for the bounds-check elimination,
`find_eliminations_sound_statement` (`BoundsCheckExecutionSpec.v`,
section 5.1). Both are stated on the original graph through
`finished_graph` (section 2.2), the semantics of section 1 and the
extracted decision itself; the loops the analyses discover, their
induction variables, the guards and the affine facts the decisions rely
on appear in neither.

What is *not* claimed:

- Nothing about `false` answers: a loop the pass keeps may well be
  deletable, a check it keeps may well be redundant. Every theorem is a
  soundness result for positive answers.
- Nothing about the rewritten graph. The theorems are stated on the
  original graph; the step to "the rewritten function behaves like the
  original" is the simulation of sections 4.7 and 5.5, written out but
  not mechanised, and the OCaml rewrites are reviewed as ordinary code.
- Nothing is `Admitted`.
  `Print Assumptions LoopEligibilityProofs.find_deletions_eligible` and
  `Print Assumptions BoundsCheckExecutionProofs.find_eliminations_sound`
  list the axioms the theorems rest on; section 2.4 says what they are.

### 3.2 The shape of the proofs

Nothing in this section needs review: the statements of sections 4.1 and
5.1 do not mention the certificates or the operational theorems below, so
Rocq checking the proofs is the whole guarantee. It is here so that the
proofs can be followed. The development has two kinds of theorems.

**Analysis certificates** read a boolean decision back into its meaning.
Each analysis `X.v` (`NaturalLoop.v`, `InductionVar.v`, `AffineSsa.v`,
`LoopTermination.v`, `DeadInductionVar.v`, `DeleteEmptyLoops.v`,
`InductionRange.v`, `BoundsCheckElim.v`, ...) is the code that is
extracted, and `XProofs.v` holds the named predicates giving its intended
meaning, the lemmas and the theorems (`rocq/HACKING.md`, "The layout of a
model"). The top certificates are
`DeleteEmptyLoopsProofs.find_deletions_spec` (section 4.2) and
`BoundsCheckElimProofs.find_eliminations_spec` (section 5.2);
`find_deletions_supported` and `find_eliminations_supported` read back
the applicability gate `graph_supported` of section 3.5, which runs before
the analyses of either pass.

**Operational theorems** take a certificate as a hypothesis and prove what
it implies for executions (sections 4.2 and 5.2). Their hypotheses about
the graph and the run are the fields of `finished_graph` and the input
contract of the pass and nothing else: no counter recurrence, invariant
bound, guard outcome or finite exit is assumed. Those are conclusions.

The files involved, bottom up:

| File | Role |
|---|---|
| `rocq/OxCamlInt.v`, `OxCamlInt64.v`, `OxCamlList.v`, `IntArith.v`, `OxCamlOption.v` | Models of OxCaml's own types and arithmetic: definitions and axioms only, trusted with their `*Extract.v` (section 2.3); their lemmas are in `*Proofs.v` (no review needed) |
| `backend/rocq/IntegerComparison.v`, `Operation.v` | The semantics of integer comparisons (section 2.1); the model's copy of `Operation.t` (section 2.3) |
| `backend/ssa/rocq/SsaModel.v` | The graph as the models see it: abstract blocks with a few observations, and `Inductive` copies of the compiler's values, instructions and terminators (section 2.3) |
| `SsaGraphSpec.v`, `SsaDominanceSpec.v` | Terminators, edges and the `control_edges` contract; dominance, paths and program points |
| `SsaSemanticsSpec.v` | The execution semantics (section 1) |
| `SsaExecutionSpec.v` | Well-formed SSA (`well_formed_ssa`) |
| `SsaLivenessSpec.v` | Registers read before redefinition |
| `SsaProgressSpec.v` | The `finished_graph` contract and the defined-input condition for `Switch` |
| `SsaContextSpec.v` | Word observations of the code around the modelled fragment (section 1.3); the `reached_header` contract |
| `SsaGraph.v`, `SsaDominance.v`, `SsaSemantics.v` | The checks the loop analyses run on a graph (extracted; no review needed) |
| `Ssa*Proofs.v` | The equations of available values, progress, the invariants of the observation semantics, and every lemma (no review needed) |
| `backend/ssa/loop/rocq/FourierMotzkin.v`, `AffineExpr.v`, `LoopComparisons.v`, `NaturalLoop.v`, `AffineSsa.v`, `InductionVar.v`, `LoopTermination.v`, `DeadInductionVar.v`, `DeleteEmptyLoops.v`, `InductionRange.v`, `BoundsCheckElim.v` | The analyses (extracted; no review needed), each with a `*Proofs.v` giving its meaning and certificate (no review needed) |
| `DeleteEmptyLoopsSpec.v` | The two statements of section 4.1.1 |
| `LoopExecutionProofs.v` | Finite traversals of the loop body (no review needed, like every `*Proofs.v` below) |
| `LoopInvariantExecutionProofs.v` | Values defined before the loop do not change during it |
| `LoopCounterExecutionProofs.v` | The counter's arithmetic in actual execution; finite exit |
| `LoopValueSnapshotProofs.v`, `LoopGuardExecutionProofs.v`, `LoopRangeExecutionProofs.v` | The affine facts the proofs rely on, justified by actual execution |
| `LoopLiveOutProofs.v` | Agreement on live-out registers |
| `LoopEligibilitySpec.v`, `LoopEligibilityProofs.v` | The statement for empty-loop deletion (section 4.1) and its proof |
| `InductionRangeExecutionProofs.v` | The certified bounds of loop counters hold in every reached state of their loops |
| `BoundsCheckExecutionSpec.v`, `BoundsCheckExecutionProofs.v` | The statement for bounds-check elimination (section 5.1) and its proof |

Every model is split into `*Spec.v`, implementation and `*Proofs.v` files
(`rocq/HACKING.md`, "The layout of a model"). What has to be read is the
Spec files, `SsaModel.v`, the models of OxCaml's types in `rocq/` and
`backend/rocq/`, and the `*Extract.v` files; nothing else.

The arithmetic models (`FourierMotzkin.v`, `AffineExpr.v`,
`LoopComparisons.v`) sit beside these and are used by the certificates.
Their theorems (`entails_sound`, `facts_sound`, ...) are about
mathematical integers; the operational proofs turn them into statements
about registers.

### 3.3 Values that do not change during the loop

`LoopInvariantExecutionProofs.v` gives
`InductionVarProofs.loop_invariant_value` its operational meaning: a value
defined in a strict dominator of the header is defined outside the body,
so while execution stays in the body its register is never rewritten (the
frame lemmas of section 1.1 again); a constant has its literal value
wherever it is available. This is how the bound of the exit comparison is
known to be the same at every visit of the header (section 4.4), and the
same argument, for the observed run, is how the upper bound of a certified
counter keeps its value throughout its loop (section 5.3). It is proved,
not assumed.

### 3.4 Affine facts from actual branches

The no-overflow side conditions of the termination argument (section 4.4)
and the entailments that discharge a bounds check (section 5.2) come from
`AffineSsa.guards_at`, which collects comparison facts from the `Switch`
terminators of a block's strict dominators, and `FourierMotzkin.entails`.
Their theorems are about a total valuation of syntax. Three files connect
them to execution:

- `LoopValueSnapshotProofs.v` builds such a valuation from a register file
  (`snapshot_value`; the registers at header entry for the deletion, at
  the branch for the bounds check), extending it to all syntax by
  arithmetic interpretation, and proves (`snapshot_agrees_available`)
  that it agrees with the registers on every available canonical value.
  Fabricated expressions get a value too, but nothing is claimed about
  them beyond this agreement.
- `LoopGuardExecutionProofs.v` proves that every guard `guards_at` collects
  was in fact taken in the direction it assumes: the block on that side
  of the branch has the branching block as its sole predecessor
  (`SsaDominance.sole_predecessor`) and dominates the block the facts are
  collected for, so every path to that block takes that edge.
  `switch_edge_multiplicity` is needed here: a `Switch` to `[next; next]`
  establishes nothing, and the contract makes such a branch show up as
  two predecessor entries.
- `LoopRangeExecutionProofs.v` turns an entailed range of a value into a
  fact about the value's register.

### 3.5 The gate and the limits

`DeleteEmptyLoops.graph_supported` must run before the analyses of
either pass and reject the whole graph on failure (see `find_deletions`
and `find_eliminations`): at most 2^32 blocks, and no instruction
anywhere in the graph belongs to an exactly modelled operation family
while having other than one result, the one instruction shape the
observation semantics of section 1.3 has no rule for. Its
`graph_block_limit` extracts to `4_294_967_296`. The Fourier-Motzkin and
linearization budgets (`FourierMotzkin.v`, `AffineSsa.v`) only make the
analyses more conservative.

## 4. Empty-loop deletion

The pass is `backend/ssa/loop/delete_empty_loops.ml`. Its decision, which
loops to bypass, is `DeleteEmptyLoops.find_deletions`, extracted from
`DeleteEmptyLoops.v`: a loop whose header parameters are induction
variables with no use but their own update and the exit test, whose body
has only whitelisted instructions, and which the exit test shows to
terminate, is bypassed from its header to its exit.

### 4.1 What is claimed

The result to review is `find_deletions_eligible_statement`, at the end
of `backend/ssa/loop/rocq/LoopEligibilitySpec.v`; `LoopEligibilityProofs.v`
proves it as `find_deletions_eligible`:

```coq
Definition find_deletions_eligible_statement : Prop :=
  ∀ (blocks : list block) (entry : block) (defs : op_def)
    (header exit : block),
  finished_graph blocks entry defs ->
  (header, exit) ∈ (find_deletions defs blocks) ->
  ∀ (initial_regs header_regs : registers),
    reached_header entry header initial_regs header_regs ->
    eligible_entry header exit header_regs.
```

In words: take a finished SSA graph as the builder guarantees it
(`finished_graph`, section 2.2) and any pair `(header, exit)` that the
extracted decision `find_deletions` reports for it. Whenever the function
is entered with machine words in its registers and execution reaches
`header` with register file `header_regs` (`reached_header`, section 4.6),
the loop at `header` may be bypassed towards `exit`: `eligible_entry`.

`eligible_entry header exit header_regs` (the same file) says:

- `exit` takes no block parameters;
- executing the header's instructions once from `header_regs` yields
  some `first_header_regs` (`SsaSemanticsSpec.executes_insns`);
- from the header's terminator with those registers, the execution of
  the *original* graph first arrives at `exit` after finitely many steps,
  entering it with some `exit_regs` (`SsaSemanticsSpec.steps_until`);
- `first_header_regs` and `exit_regs` agree on every register live at the
  entry of `exit`, that is, read somewhere before being redefined
  (`SsaLivenessSpec.agree_on_live`).

Because `SsaSemanticsSpec.step` is deterministic, the third point is about
*the* execution from the header: it neither diverges nor gets stuck
before the exit, and since `step` has rules only for whitelisted integer
operations, `Goto` and `Switch`, the skipped execution touches neither
memory nor the trap stack. The loop the analysis discovered, its body and
its induction variables do not appear in the statement.

#### 4.1.1 Two statements the unmechanised parts rely on

Two further statements, in `backend/ssa/loop/rocq/DeleteEmptyLoopsSpec.v`,
are relied on by the parts of the argument that are not in Rocq. Both are
about the pairs `find_deletions` reports and are stated through the graph
and the semantics only:

- `find_deletions_exit_edge_statement`: the exit is a target of the
  header's own `Switch`. The simulation of section 4.7 needs it: the
  rewrite replaces that terminator by a `Goto` to the exit, so the
  rewritten graph has a subset of the original's edges, and a register
  live at the exit's entry is live at the header's terminator.
- `find_deletions_context_covered_statement`: no instruction of the graph
  is an exactly modelled operation with other than one result, the one
  instruction shape the observation semantics of section 1.3 has no rule
  for. It is what makes the `reached_header` premise satisfiable for
  every real execution reaching the header (section 4.6).

`DeleteEmptyLoopsProofs.v` proves both, and `make -C rocq claims` checks
them as well.

### 4.2 The certificate and the operational theorems

The top certificate is `DeleteEmptyLoopsProofs.find_deletions_spec`: every
pair that `find_deletions` returns comes with a natural loop and its basic
induction variables satisfying `deletion_certificate`, whose conjuncts are

- `exit_branch_to loop exit`: the header's terminator is the simple exit
  test that `find_exit_branch` (`NaturalLoop.v`) recognises, and `exit`
  is where its failing side goes;
- `LoopTermination.analyze op_def bivs = Terminates`;
- `region_acyclic_except loop.(body) loop.(header)`: the body without
  the header has no cycle, so execution cannot loop without revisiting
  the header;
- `params_are_dead_bivs op_def blocks loop bivs`: every header parameter
  is a basic induction variable whose only uses are its own update and
  the exit comparison, itself used only by the header's branch;
- `body_in_supported_fragment loop`: every block of the body, header
  included, has only whitelisted instructions and ends in `Goto` or
  `Switch`;
- `exits_only_to loop exit`: every edge leaving the body goes to `exit`.

`find_deletions_supported` reads back the applicability gate
`graph_supported` of section 3.5; its second half is
`find_deletions_context_covered_statement` of section 4.1.1.

The operational theorems are in `LoopEligibilityProofs.v`:
`certificate_region_progress` says the loop's execution does not get
stuck, `certificate_finite_exit_eligible` turns a finite execution to the
exit into `eligible_entry`, and `certificate_entry_eligible` combines them
with the finite-exit construction of section 4.4. `find_deletions_eligible`
instantiates the last with the certificate that `find_deletions_spec`
supplies for each reported pair. The hypotheses of these theorems about
the graph are the fields of `finished_graph` and `reached_header` and
nothing else: no counter recurrence, invariant bound, guard outcome or
finite exit is assumed. Those are conclusions.

### 4.3 The loop runs exactly

The loop's own execution, from the header to the exit, uses `step`: by
`body_in_supported_fragment` every instruction on the way is
whitelisted, so `step` has a rule for each, and `exits_only_to` keeps the
execution inside the body until it reaches `exit`. The code before the
loop, from the function's entry to the header, is observed instead
(section 1.3): `reached_header` asks for a `context_steps` run from the
entry state to the header (section 4.6), and the theorems of
`SsaContextProofs.v` supply what the loop's execution needs of it, that
the registers hold bounded words, that available values satisfy their
equations and that the live registers are bound.

### 4.4 Finite exit, by construction

The termination argument is operational and finite; it never speaks of an
infinite trace.

- `LoopExecutionProofs.region_iteration` builds, from `region_acyclic_except`
  and progress, a finite traversal from the header back to the header or
  out to `exit`, as a sequence of `step`s inside the body.
- `LoopCounterExecutionProofs.v` reads the counter's update off such a
  traversal: the value the back edge's `Goto` passes for the header
  parameter is the parameter plus or minus the step, as `biv_spec` says.
- `LoopComparisonsProofs.v` supplies the arithmetic: from the counter's start,
  its step and the invariant bound, and given the no-overflow facts of
  section 3.4, the exit comparison fails after some number `n` of
  updates (`loop_exits`).
- `LoopCounterExecutionProofs.v` then iterates `region_iteration` up to `n`
  times. Each iteration either leaves the body, necessarily to `exit` by
  `exits_only_to`, or returns to the header with the counter advanced
  once; after `n` of them the header's own `Switch` takes the exit edge.

Progress at each step (`SsaProgressProofs.supported_state_progress`) needs the
operands to be bound and `Switch` indices to be in range. Boundness comes
from liveness: an operand about to be read is live, a live register at
the reached header is bound by `reached_header`, and `control_layout`
ensures that an omitted `Goto` argument never feeds a parameter that is
read. In-range indices are the `switches_defined` field of
`reached_header`: out-of-range indices are undefined behaviour in this
SSA, so they are an input assumption, not something to prove.

### 4.5 Live-out agreement

`SsaLivenessSpec.register_live_at p r` says that some structural path from
`p` reads `r` before redefining it, including reads in later blocks and
on later entries of the loop; infeasible paths count, which only makes
the condition stronger. `LoopLiveOutProofs.v` shows that every register live at
the entry of `exit` holds, when the execution reaches the exit, the value
it held after the first execution of the header's instructions: a value
defined before the loop is untouched by the loop's steps (section 3.3); a
header parameter cannot be live there, since `params_are_dead_bivs`
allows it no use outside the loop's own control; and a result of a header
instruction that is live there depends, by the same deadness, only on
values defined before the loop, so it is the same after the first header
execution as at the exit. The header itself is retained by the rewrite
and executes once, as it did on the first iteration before it (the
header of `DeleteEmptyLoopsProofs.v` spells this out).

### 4.6 The input contract: `reached_header`

Its four fields (`SsaContextSpec.v`) are the only assumptions about how the
function was entered and ran:

- `inputs_bounded`: the initial registers hold 64-bit words;
- `live_inputs_defined`: the registers live at the function's entry are
  bound;
- `header_reached`: the observation semantics takes the entry state to
  `enter header header_regs`, so section 1.3 has to be right that every
  real prefix is represented: `context_step` must have a rule for every
  instruction shape of a real graph but the one that
  `find_deletions_context_covered_statement` (section 4.1.1) excludes;
- `switches_defined`: from the header on, every `Switch` the exact
  execution reaches has an in-range index.

### 4.7 From the theorem to the rewrite

`find_deletions_eligible` is per loop and per entry of its header, on the
original graph. The step from it to "the rewritten function behaves like
the original" is the standard liveness simulation, which is not in Rocq.
With `P` the original graph and `P'` the graph in which each reported
header's terminator has been replaced by `Goto exit` with no arguments,
and liveness always meaning `register_live_at` in `P`:

Relate a state of `P` and a state of `P'` at the same program point when
their register files agree on the registers live at that point, and
everything else (memory, trap stack, current block and remaining
instructions) is equal.

- Every step other than a rewritten header's terminator is the same
  instruction or terminator in both graphs. It reads only registers live
  at its point (`Live_read`), so it writes the same values; a register
  live after the step is either written by it or live before it
  (`Live_predecessor`). The relation is preserved.
- At a rewritten header's terminator, `P` runs `steps_until exit` to
  `enter exit exit_regs` while `P'` takes `Goto exit` with its own
  `first_header_regs'`, which agrees with `first_header_regs` on the
  registers live at the header's terminator. Since `exit` is a target of
  the header's `Switch` (`find_deletions_exit_edge_statement`, so a
  successor by `control_edges`) and has no parameters, nothing is defined
  on entering it, so by `Live_predecessor` every register live at the entry of
  `exit` is live at the header's terminator; with
  `agree_on_live (block_entry_point exit) first_header_regs exit_regs`
  from `eligible_entry`, the two states at `exit` are related. The
  skipped segment writes no memory and no trap marker, because `step`
  has no rule that would, and `step` is deterministic, so `P` does not
  diverge or get stuck before the exit: `P'` loses no behaviour.
- `P'` has a subset of `P`'s edges (the same statement), so every
  register live in `P'` is live in `P`; the relation is at least as strong
  as `P'` needs.

Several headers are rewritten in one pass over the graph. The argument is
per rewritten terminator and the `P` side is always the original graph,
in which each header's `eligible_entry` holds; so it applies to each
independently, whatever blocks the skipped segments visit. (The reported
loops are in fact pairwise disjoint, since `acyclic_except` rejects a
nested cycle, but the simulation does not need this.)

The OCaml rewrite is reviewed as ordinary code. In
`backend/ssa/loop/delete_empty_loops.ml`, `find_deletions` builds the
table of defining blocks (`compute_op_def`: every `Op` of every block, as
`SsaGraphSpec.op_def_correct` requires) and calls the extracted
`DeleteEmptyLoops.find_deletions` on the finished graph; `run` then
applies an `Ssa_reducer` whose only change is `visit_terminator`: for a
reported header, replace the terminator by
`Continue { continuation = Goto exit_target; args = [||] }`, keeping the
header's instructions. Check that the reducer maps blocks and values
consistently, that pruning the now unreachable body removes nothing
live, and that the rewrite is applied to the graph that was analysed.

## 5. Bounds-check elimination

The pass is `backend/ssa/loop/bounds_check_elim.ml`. Its decision, which
checks to remove, is `BoundsCheckElim.find_eliminations`, extracted from
`BoundsCheckElim.v`: a bounds check is a two-way `Switch` on the unsigned
comparison by which the compiler tests `0 <= index < length` at once,
and it is removed when the facts known on entry to its block, from the
branches dominating it and from the certified bounds of the loop counters
around it, entail that the comparison is true.

### 5.1 What is claimed

The result to review is `find_eliminations_sound_statement`, at the end of
`backend/ssa/loop/rocq/BoundsCheckExecutionSpec.v`;
`BoundsCheckExecutionProofs.v` proves it as `find_eliminations_sound`:

```coq
Definition find_eliminations_sound_statement : Prop :=
  ∀ (blocks : list block) (entry : block) (defs : op_def)
    (b target : block),
  finished_graph blocks entry defs ->
  (b, target) ∈ (find_eliminations defs entry blocks) ->
  ∀ (initial_regs : registers) (reached : state),
  bounded_registers initial_regs ->
  context_steps (enter entry initial_regs) reached ->
  current_block reached = b -> remaining reached = [] ->
  SsaSemanticsSpec.step reached (enter target (current_registers reached)).
```

In words: take any pair `(b, target)` that the extracted decision
`find_eliminations` reports for a finished graph. Whenever the function
is entered with machine words in its registers and the observation
semantics of section 1.3 (`context_steps`, which represents every real
execution) reaches the end of the instructions of `b`, the next step of
the *original* graph enters `target` with the registers unchanged: the
branch ending `b`, a two-way `Switch` on the unsigned comparison the
compiler emits for an array bounds check, takes its edge to `target`.
Replacing the branch by `Goto target` therefore changes no execution, and
since no segment is skipped there is no liveness condition to meet. The
guards, loop counters and affine facts the decision relied on do not
appear in the statement.

The two further statements of section 4.1.1 have no counterpart here: the
rewrite skips nothing, so the simulation of section 5.5 needs no fact
about the graph's edges, and the input contract is the premise list of the
statement itself (section 5.4).

### 5.2 The certificate and the operational theorems

The top certificate is `BoundsCheckElimProofs.find_eliminations_spec`:
every pair that `find_eliminations` returns comes with the block's
terminator, a two-way `Switch` on a comparison `unsigned_check` recognises
(an index against a limit, a value or a literal, under `<u` or `<=u`, or
the two values exchanged under `>u` or `>=u`), with a list of counter
certificates and the conjuncts

- `certificate_spec` (`InductionRangeProofs.v`) of each counter
  certificate: its counter is a basic induction variable
  (`InductionVar.v`) of a loop `NaturalLoop.v` finds, with a positive
  literal step; the loop's header is not the entry block; every entry of
  the header passes a literal at least the certified lower bound; when
  the certificate names an upper bound, that value is defined in a strict
  dominator of the header, is an operand of some two-way branch on a
  comparison, and every predecessor of the header passes a value
  `argument_at_most` showed, from the guards there, to be at most it; and
  at every back edge `no_wrap_at` showed, from the guards there and the
  certified bounds, that the update cannot wrap;
- a bound on the number of certificates, for the affine atom capacity;
- `check_passes`: the facts of the branches dominating the block
  (`AffineSsa.guards_at`), the side facts of the linearizations and the
  bounds of the certified counters whose loops contain the block
  (`range_facts`) entail `0 <= index` and `index <= limit - 1` (or
  `index <= limit` for `<=u`), with the index linearized modulo 2^64
  (`linearize_goal`) and the limit exactly.

`find_eliminations_supported` reads back the gate of section 3.5.

The operational theorems are `InductionRangeExecutionProofs.counter_bounded`,
which carries the certified bounds of a counter along the observed run
(section 5.3), and `BoundsCheckExecutionProofs.find_eliminations_sound`,
which combines them with the guard facts of section 3.4, both evaluated
under the valuation of `LoopValueSnapshotProofs.v` built from the registers
at the branch, to show that the comparison's register holds 1 and the
`Switch` steps to the reported target. The only hypotheses about the graph
and the run are `finished_graph`, bound inputs and `context_steps`.

### 5.3 Counter bounds along the observed run

The bounds-check elimination has no exactly executed segment: its theorem
is about the observed run up to the branch (section 1.3), so
`context_steps` is its whole input contract (section 5.4), and the facts
the decision relied on have to hold in every state that run reaches. For
the guards this is section 3.4. For the counters,
`InductionRangeExecutionProofs.counter_bounded` says that in every state
the observation semantics reaches inside a certified loop's body, the
counter's register holds a word within the certified bounds: at least the
lower bound and at most the word in the upper bound's register. It is an
induction over `context_steps`. Only a transfer into the header writes the
counter: over a back edge it receives the update, which the certificate
showed does not wrap, and from any other predecessor a literal at least
the lower bound; either way the value passed is at most the upper bound,
by `argument_at_most`. Every other step leaves the counter's register
alone, and the upper bound's too, because its definition lies outside the
body (section 3.3); an opaque transfer cannot enter the header, whose
predecessors all end in a `Goto`.

### 5.4 The input contract

The input contract of the bounds-check elimination is the premise list of
`find_eliminations_sound` itself: `bounded_registers` of the initial
registers (the `inputs_bounded` field of `reached_header`, section 4.6)
and a `context_steps` run from the entry to the branch (its
`header_reached` field, with the block of the check in place of the
header), so section 1.3 has to be right here too that every real
execution is represented. It needs neither live inputs nor defined
`Switch` indices, since it skips nothing.

### 5.5 From the theorem to the rewrite

The simulation is immediate. With `P` the original graph and `P'` the
graph in which each reported block's terminator has been replaced by
`Goto target` with no arguments, `find_eliminations_sound` says that at
such a terminator `P` steps to `enter target` with the registers
unchanged, which is the step `P'` takes; so the relation of section 4.7
is preserved with no segment skipped, and nothing about liveness is
needed.

The rewrite in `backend/ssa/loop/bounds_check_elim.ml` is the same as the
one of section 4.7: `find_eliminations` builds the table of defining
blocks with `Delete_empty_loops.compute_op_def` and calls the extracted
`BoundsCheckElim.find_eliminations` on the finished graph, and `run`
applies an `Ssa_reducer` whose only change is `visit_terminator`,
replacing a reported block's terminator by
`Continue { continuation = Goto target; args = [||] }`. The same checks
apply; the block that raised the exception becomes unreachable when the
check was its only predecessor, and is pruned.

## 6. Reading order and commands

1. `LoopEligibilitySpec.v`: `eligible_entry` and
   `find_deletions_eligible_statement`. Then print the premises:

   ```coq
   From OxCamlLoop Require Import LoopEligibilitySpec LoopEligibilityProofs.
   Print find_deletions_eligible_statement.
   Check find_deletions_eligible : find_deletions_eligible_statement.
   Print Assumptions find_deletions_eligible.
   Print eligible_entry.
   Print SsaSemanticsSpec.steps_until.
   Print SsaProgressSpec.finished_graph.
   Print SsaContextSpec.reached_header.
   ```

   The statement is a closed proposition, not in a `Section`, so `Print`
   shows every premise; the `Check` is what `make -C rocq claims` runs.
2. `BoundsCheckExecutionSpec.v`: `find_eliminations_sound_statement`,
   likewise:

   ```coq
   From OxCamlLoop Require Import BoundsCheckExecutionSpec
     BoundsCheckExecutionProofs.
   Print find_eliminations_sound_statement.
   Check find_eliminations_sound : find_eliminations_sound_statement.
   Print Assumptions find_eliminations_sound.
   ```

   Its premises are `finished_graph`, `bounded_registers` and
   `context_steps`.
3. `SsaSemanticsSpec.v`, `SsaLivenessSpec.v` and `SsaContextSpec.v`:
   section 1, against the backend (section 2.1). Nothing else in
   `backend/ssa/rocq/` but `SsaModel.v` and `SsaModelExtract.v` needs
   reading.
4. `SsaProgressSpec.finished_graph` and `SsaExecutionSpec.well_formed_ssa`
   against `ssa.ml` (section 2.2).
5. `DeleteEmptyLoopsSpec.v`: the two statements the simulation (section
   4.7) and the input contract (section 4.6) rely on.
6. The `*Extract.v` files (section 2.3) and the OCaml wrappers (sections
   4.7 and 5.5).
7. `make -C rocq all` (Rocq 9 and the repository's `ocamlformat` on
   `PATH`) checks every proof and confirms that the extraction
   reproduces the committed files; `make -s -C rocq assumptions` prints
   the audit report, whose first part lists the global axioms and whose
   second prints every declaration's type, Section premises included.
   `make -C rocq review` typesets this guide and every reviewed file as
   one PDF, `rocq/review.pdf`, each identifier linked to its definition.

Rocq vocabulary, for a reader with none: `Inductive` declares a variant
type, like OxCaml `type t = A | B`; `Definition` a function or constant,
like `let`; `Fixpoint` a recursive function, like `let rec`; `Record` a
record type, whose fields are `Prop`s when it states a contract; `Lemma`
and `Theorem` state a property, and the text between `Proof.` and `Qed.`
is a script the checker runs to establish it. `Z` is the type of
unbounded mathematical integers; `int` is Rocq's primitive signed 63-bit
integer, which is exactly OxCaml's `int`, and `to_Z` reads one as a
mathematical integer. `a ▷ b` (`SsaModel.v`) says that block `a`
dominates block `b`, and `x ∈ l` is list membership.

Proof scripts (`Proof. ... Qed.`) can be skipped: Rocq checks them. The
tests are in `testsuite/tests/backend/ssa/loop/`, where the
`[%%expect_ssa X86_64{|...|}]` blocks show the SSA each function ends up
with and the number of loops deleted or checks removed (they are checked
on x86-64 only, since the SSA is printed after instruction selection),
and in `oxcaml/tests/backend/ssa/`.

## 7. What not to take on trust

- A hypothesis stated through an analysis function silently trusts that
  function. The development avoids this at the top: `finished_graph` and
  `reached_header` are stated through the primitives of `SsaModel.v` and
  the semantics, never through implementation code. Lower down, hypotheses
  such as `biv_spec`, `deletion_certificate` or `certificate_spec` are
  stated through code, but the development discharges them itself
  (`InductionVarProofs.analyze_spec`, `find_deletions_spec`,
  `find_eliminations_spec`, ...), so nothing at the top depends on their
  truth of the program. When reading a lemma, ask which of its premises
  the main theorem discharges and which it passes on.
- A conclusion stated through code cannot make a theorem vacuous; it only
  means opening the code to learn what is claimed.
- A valuation of all syntax (`LoopValueSnapshotProofs.snapshot_value`) is a
  proof device. The only thing proved about it is agreement with the
  registers on available canonical values; no theorem gives a meaning to
  a fabricated expression.
- Equality of values is identity of the compiler's data
  (`Ssa.Value.equal` compares the instruction id and the result index).
  Two values of one graph are equal to it exactly when they are the same
  term, and the models never construct values.
- The analyses may be wrong in the conservative direction. A kept loop
  or check needs no justification; a deleted loop or removed check needs
  the whole of section 4.1 or of section 5.1.
