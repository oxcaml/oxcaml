# Reviewing the Rocq-verified empty-loop deletion

This page says how to review the empty-loop deletion pass of the SSA
backend (`-ssa-delete-empty-loops`) and the Rocq development behind it:
what is claimed, how the proof is put together, how the execution of SSA
code is reasoned about, and what remains to be checked by eye because no
proof covers it. `rocq/HACKING.md` is the companion page for changing the
models; this one is for reading them.

The pass is `backend/ssa/loop/delete_empty_loops.ml`. Its decision, which
loops to bypass, is the OCaml extracted from `DeleteEmptyLoops.v` and the
models it uses (`backend/ssa/loop/rocq/`, with the generic SSA models in
`backend/ssa/rocq/`, backend-wide definitions in `backend/rocq/` and the
models of OxCaml's own types in `rocq/`). The extracted `.ml`/`.mli` files
are committed next to the models and are the implementation. The rewrite
of the graph once the decision is made is hand-written OCaml.

## 1. What is claimed

The result to review is `find_deletions_eligible`, at the end of
`backend/ssa/loop/rocq/LoopEligibility.v`:

```coq
Theorem find_deletions_eligible :
  ∀ (blocks : list block) (entry : block) (defs : op_def)
    (header exit : block),
  finished_graph blocks entry defs ->
  (header, exit) ∈ (find_deletions defs blocks) ->
  ∀ (initial_regs header_regs : registers),
    reached_header entry header initial_regs header_regs ->
    eligible_entry header exit header_regs.
```

In words: take a finished SSA graph as the builder guarantees it
(`finished_graph`, section 5.2) and any pair `(header, exit)` that the
extracted decision `find_deletions` reports for it. Whenever the function
is entered with machine words in its registers and execution reaches
`header` with register file `header_regs` (`reached_header`, section 5.3),
the loop at `header` may be bypassed towards `exit`: `eligible_entry`.

`eligible_entry header exit header_regs` (part 1 of the same file) says:

- `exit` takes no block parameters;
- executing the header's instructions once from `header_regs` yields
  some `first_header_regs` (`SsaSemantics.executes_insns`);
- from the header's terminator with those registers, the execution of
  the *original* graph first arrives at `exit` after finitely many steps,
  entering it with some `exit_regs` (`SsaSemantics.steps_until`);
- `first_header_regs` and `exit_regs` agree on every register live at the
  entry of `exit`, that is, read somewhere before being redefined
  (`SsaLiveness.agree_on_live`).

Because `SsaSemantics.step` is deterministic, the third point is about
*the* execution from the header: it neither diverges nor gets stuck
before the exit, and since `step` has rules only for whitelisted integer
operations, `Goto` and `Switch`, the skipped execution touches neither
memory nor the trap stack. The loop the analysis discovered, its body and
its induction variables do not appear in the statement.

What is *not* claimed:

- Nothing about `false` answers: a loop the pass keeps may well be
  deletable. Every theorem is a soundness result for positive answers.
- Nothing about the rewritten graph. The theorem is stated on the
  original graph; the step to "the rewritten function behaves like the
  original" is the liveness simulation of section 4, written out but not
  mechanised, and the OCaml rewrite is reviewed as ordinary code.
- Nothing is `Admitted`. `Print Assumptions find_deletions_eligible`
  lists the axioms the theorem rests on; section 5.5 says what they are.

## 2. The shape of the proof

The development has two kinds of theorems, which the review has to keep
apart.

**Analysis certificates** read a boolean decision back into its meaning.
Each model file (`NaturalLoop.v`, `InductionVar.v`, `AffineSsa.v`,
`LoopTermination.v`, `DeadInductionVar.v`, `DeleteEmptyLoops.v`, ...) has
the four-part layout described in `rocq/HACKING.md`: the code that is
extracted (part 1), named predicates giving its intended meaning (part
2), lemmas (part 3), theorems (part 4). The top certificate is
`DeleteEmptyLoops.find_deletions_spec`: every pair that `find_deletions`
returns comes with a natural loop and its basic induction variables
satisfying `deletion_certificate`, whose conjuncts are

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
`graph_supported`, run before the analyses: at most 2^32 blocks, and no
instruction anywhere in the graph belongs to an exactly modelled
operation family while having other than one result (the one instruction
shape the execution semantics has no rule for).

**Operational theorems** take the certificate as a hypothesis and prove
what it implies for executions. In `LoopEligibility.v`,
`certificate_region_progress` says the loop's execution does not get
stuck, `certificate_finite_exit_eligible` turns a finite execution to the
exit into `eligible_entry`, and `certificate_entry_eligible` combines them
with the finite-exit construction of section 3.5. `find_deletions_eligible`
instantiates the last with the certificate that `find_deletions_spec`
supplies for each reported pair. The hypotheses of these theorems about
the graph are the fields of `finished_graph` and `reached_header` and
nothing else: no counter recurrence, invariant bound, guard outcome or
finite exit is assumed. Those are conclusions.

The files involved, bottom up:

| File | Role |
|---|---|
| `backend/ssa/rocq/SsaModel.v` | The graph as the models see it: abstract blocks with a few observations, and `Inductive` copies of the compiler's values, instructions and terminators (section 5.4) |
| `SsaGraph.v`, `SsaDominance.v` | Structural facts any pass may need: terminators, uses, edges, dominance and paths |
| `SsaSemantics.v` | The execution semantics (section 3) |
| `SsaExecution.v` | Well-formed SSA and the equations of the values available at a program point |
| `SsaLiveness.v` | Registers read before redefinition |
| `SsaProgress.v` | Progress of a supported state; the `finished_graph` contract |
| `SsaContext.v` | Word observations of the code before the loop; the `reached_header` contract |
| `backend/ssa/loop/rocq/LoopExecution.v` | Finite traversals of the loop body |
| `LoopInvariantExecution.v` | Values defined before the loop do not change during it |
| `LoopCounterExecution.v` | The counter's arithmetic in actual execution; finite exit |
| `LoopValueSnapshot.v`, `LoopGuardExecution.v`, `LoopRangeExecution.v` | The affine facts the termination argument relies on, justified by actual execution |
| `LoopLiveOut.v` | Agreement on live-out registers |
| `LoopEligibility.v` | The statement and its proof |

The arithmetic models (`FourierMotzkin.v`, `AffineExpr.v`,
`LoopComparisons.v`) sit beside these and are used by the certificates.
Their theorems (`entails_sound`, `facts_sound`, ...) are about
mathematical integers; the operational proofs turn them into statements
about registers.

## 3. How SSA execution is reasoned about

This is the part to read most carefully: it is where the meaning of the
theorem comes from, and no proof can check it against the compiler.

### 3.1 Registers, not expressions

`SsaSemantics.v` gives a program state three components: the current
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
about the program. Instead, `SsaExecution.v` derives the equation of a
value only where the value is *available*: its definition site dominates
the current point (`available_value`), so by the frame lemmas its
register was written by that instruction and not since. These derived
equations (`coherent_at`) are what the affine and counter proofs use.

### 3.2 One deterministic step

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
a state of `target`. This is what makes "bypass the loop" meaningful
without mentioning the loop: which blocks the execution visits before the
exit is irrelevant.

### 3.3 The loop runs exactly; the prefix is observed

The loop's own execution, from the header to the exit, uses `step`: by
`body_in_supported_fragment` every instruction on the way is
whitelisted, so `step` has a rule for each, and `exits_only_to` keeps the
execution inside the body until it reaches `exit`.

The code *before* the loop, from the function's entry to the header, is
another matter: it contains loads, calls, allocations, trap pushes and
whatever else the function does, and modelling all of that would need a
memory and a calling convention. `SsaContext.v` avoids this with a second
relation, `context_step`, an over-approximation of what the machine does
to registers:

- an exactly modelled instruction, a `Goto` or a `Switch` takes a `step`;
- any other instruction (`Context_opaque_instruction`) writes arbitrary
  in-range words to exactly its declared result registers and leaves
  every other register alone; its memory effects are left unconstrained
  rather than modelled as no-ops, which suffices because no conclusion of
  the proof concerns memory and the loop itself reads none;
- any other terminator (`Context_opaque_transfer`) jumps to any of the
  block's successors, the exception handler of a `Call` or `Raise`
  included, writing arbitrary words to the target's parameters;
- `Push_trap` and `Pop_trap` step without touching a register.

Every instruction and terminator of a finished graph has one of these
rules, except an exactly modelled operation with other than one result,
which the extracted gate rejects for the whole graph. So every real
prefix reaching the header is represented by some `context_steps` run,
and `reached_header` can require one (section 5.3). The theorems of
`SsaContext.v` establish, once for exact and opaque steps alike, that the
observations stay bounded words, that available modelled values satisfy
their equations (`context_execution_values_coherent`) and that every
register live at the reached point is bound
(`context_execution_live_registers_defined`). Opaque results carry no
equation; the affine analysis sees them as atoms.

### 3.4 Values that do not change during the loop

`LoopInvariantExecution.v` gives `InductionVar.loop_invariant_value` its
operational meaning: a value defined in a strict dominator of the header
is defined outside the body, so while execution stays in the body its
register is never rewritten (the frame lemmas again); a constant has its
literal value wherever it is available. This is how the bound of the exit
comparison is known to be the same at every visit of the header. It is
proved, not assumed.

### 3.5 Finite exit, by construction

The termination argument is operational and finite; it never speaks of an
infinite trace.

- `LoopExecution.region_iteration` builds, from `region_acyclic_except`
  and progress, a finite traversal from the header back to the header or
  out to `exit`, as a sequence of `step`s inside the body.
- `LoopCounterExecution.v` reads the counter's update off such a
  traversal: the value the back edge's `Goto` passes for the header
  parameter is the parameter plus or minus the step, as `biv_spec` says.
- `LoopComparisons.v` supplies the arithmetic: from the counter's start,
  its step and the invariant bound, and given the no-overflow facts of
  section 3.6, the exit comparison fails after some number `n` of
  updates (`loop_exits`).
- `LoopCounterExecution.v` then iterates `region_iteration` up to `n`
  times. Each iteration either leaves the body, necessarily to `exit` by
  `exits_only_to`, or returns to the header with the counter advanced
  once; after `n` of them the header's own `Switch` takes the exit edge.

Progress at each step (`SsaProgress.supported_state_progress`) needs the
operands to be bound and `Switch` indices to be in range. Boundness comes
from liveness: an operand about to be read is live, a live register at
the reached header is bound by `reached_header`, and `control_layout`
ensures that an omitted `Goto` argument never feeds a parameter that is
read. In-range indices are the `switches_defined` field of
`reached_header`: out-of-range indices are undefined behaviour in this
SSA, so they are an input assumption, not something to prove.

### 3.6 Affine facts from actual branches

The no-overflow side conditions of the termination argument come from
`AffineSsa.guards_at`, which collects comparison facts from the `Switch`
terminators of the header's strict dominators, and
`FourierMotzkin.entails`. Their theorems are about a total valuation of
syntax. Three files connect them to execution:

- `LoopValueSnapshot.v` builds such a valuation from the register file
  at header entry (`snapshot_value`), extending it to all syntax by
  arithmetic interpretation, and proves (`snapshot_agrees_available`)
  that it agrees with the registers on every available canonical value.
  Fabricated expressions get a value too, but nothing is claimed about
  them beyond this agreement.
- `LoopGuardExecution.v` proves that every guard `guards_at` collects
  was in fact taken in the direction it assumes: the block on that side
  of the branch has the branching block as its sole predecessor
  (`SsaDominance.sole_predecessor`) and dominates the header, so every
  path to the header takes that edge. `switch_edge_multiplicity` is
  needed here: a `Switch` to `[next; next]` establishes nothing, and the
  contract makes such a branch show up as two predecessor entries.
- `LoopRangeExecution.v` turns the entailed range of the bound into a
  fact about the bound's register.

### 3.7 Live-out agreement

`SsaLiveness.register_live_at p r` says that some structural path from
`p` reads `r` before redefining it, including reads in later blocks and
on later entries of the loop; infeasible paths count, which only makes
the condition stronger. `LoopLiveOut.v` shows that every register live at
the entry of `exit` holds, when the execution reaches the exit, the value
it held after the first execution of the header's instructions: a value
defined before the loop is untouched by the loop's steps (section 3.4); a
header parameter cannot be live there, since `params_are_dead_bivs`
allows it no use outside the loop's own control; and a result of a header
instruction that is live there depends, by the same deadness, only on
values defined before the loop, so it is the same after the first header
execution as at the exit. The header itself is retained by the rewrite
and executes once, as it did on the first iteration before it (the
header of `DeleteEmptyLoops.v` spells this out).

## 4. From the theorem to the rewrite

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
  registers live at the header's terminator. Since `exit` is a successor
  of the header and has no parameters, nothing is defined on entering
  it, so by `Live_predecessor` every register live at the entry of
  `exit` is live at the header's terminator; with
  `agree_on_live (block_entry_point exit) first_header_regs exit_regs`
  from `eligible_entry`, the two states at `exit` are related. The
  skipped segment writes no memory and no trap marker, because `step`
  has no rule that would, and `step` is deterministic, so `P` does not
  diverge or get stuck before the exit: `P'` loses no behaviour.
- `P'` has a subset of `P`'s edges, so every register live in `P'` is
  live in `P`; the relation is at least as strong as `P'` needs.

Several headers are rewritten in one pass over the graph. The argument is
per rewritten terminator and the `P` side is always the original graph,
in which each header's `eligible_entry` holds; so it applies to each
independently, whatever blocks the skipped segments visit. (The reported
loops are in fact pairwise disjoint, since `acyclic_except` rejects a
nested cycle, but the simulation does not need this.)

The OCaml rewrite is reviewed as ordinary code. In
`backend/ssa/loop/delete_empty_loops.ml`, `find_deletions` builds the
table of defining blocks (`compute_op_def`: every `Op` of every block, as
`SsaGraph.op_def_correct` requires) and calls the extracted
`DeleteEmptyLoops.find_deletions` on the finished graph; `run` then
applies an `Ssa_reducer` whose only change is `visit_terminator`: for a
reported header, replace the terminator by
`Continue { continuation = Goto exit_target; args = [||] }`, keeping the
header's instructions. Check that the reducer maps blocks and values
consistently, that pruning the now unreachable body removes nothing
live, and that the rewrite is applied to the graph that was analysed.

## 5. What must be checked by inspection

Rocq checks the proofs. It cannot check that the definitions mean what
the compiler does. The following are the places where that has to be
done by reading.

### 5.1 The semantics against the backend

In `SsaSemantics.v`: `eval_operation` (64-bit wrapping addition and
subtraction, `Const_int` through `Int64.of_nativeint`, the shifts and
`Ior` used for bounds), `comparison_value` with
`backend/rocq/IntegerComparison.v` (signed and unsigned comparisons, in
the constructor order of `typing/scalar.ml`), and the untagged 0/1
result of a comparison. `Step_goto` and `Step_switch` against
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

### 5.2 The graph contract: `finished_graph`

Each field of `SsaProgress.finished_graph` is something `Ssa.finish_graph`
(`backend/ssa/ssa.ml`) is believed to guarantee of the graph the pass
receives. Where to look:

| Field | Meaning | Code to compare with |
|---|---|---|
| `graph_well_formed` (`SsaExecution.well_formed_ssa`) | every instruction has one definition site; instruction operands have strictly earlier dominating definitions and terminator operands dominating ones; parameters are distinct and canonical; blocks are reachable and closed under successors | `check_value_invariants`, `check_terminator`, `compute_reachability_and_trap_stacks` in `ssa.ml` |
| `graph_predecessors_closed`, `graph_edges` (`SsaGraph.control_edges`) | predecessors, `all_successors` and terminators describe the same edges, the exception successor of a `Call` or `Raise` included | `finish_graph` and the predecessor and successor tables it fills |
| `graph_switch_edges` (`switch_edge_multiplicity`) | a `Switch` with a repeated target produces repeated predecessor entries | the same |
| `graph_dominance`, `graph_dominance_complete` | `Block.dominates` answers true exactly when every path from the entry passes through the dominator | `compute_dominators`, `Block.dominates` |
| `graph_strict_dominators` | `Block.strict_dominators` lists distinct blocks of the graph | `Block.strict_dominators` (the walk up the immediate dominators) |
| `graph_layout` (`control_layout`) | `Goto` arguments match the target's parameters in number and order; an omitted argument feeds a parameter no operand anywhere reads; `Switch` targets have no parameters | `check_terminator` |
| `graph_definitions` (`op_def_correct`) | the table maps every instruction to its block | `compute_op_def` in `delete_empty_loops.ml` |

Statements such as `control_edges` quantify over the type `block`
without a membership guard. Read `block` as the reachable blocks of one
finished graph, the only ones the pass receives: `finish_graph` prunes the
others, no reachable block refers to them, and `Block.dominates` is not
defined on them (header of `SsaModel.v`).

### 5.3 The input contract: `reached_header`

Its four fields (`SsaContext.v`) are the only assumptions about how the
function was entered and ran:

- `inputs_bounded`: the initial registers hold 64-bit words;
- `live_inputs_defined`: the registers live at the function's entry are
  bound;
- `header_reached`: the observation semantics takes the entry state to
  `enter header header_regs`, so section 3.3 has to be right that every
  real prefix is represented;
- `switches_defined`: from the header on, every `Switch` the exact
  execution reaches has an in-range index.

### 5.4 The extraction directives

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
  misalign.
- `backend/rocq/IntegerComparisonExtract.v`: the constructor order of
  `typing/scalar.ml`.
- `rocq/OxCamlExtract.v`: `int` and `Int64` operations, list and option
  functions, fold argument order, short-circuit booleans.
- `rocq/IntArithExtract.v`: the four overflow tests `add_fits`,
  `mul_fits`, `add_fits64` and `mul_fits64`, a few lines of OCaml each.
- `backend/ssa/loop/rocq/Extract.v`: the entry points (`find_deletions`
  and the affine functions the unit test calls) and the literal for
  `graph_block_limit`.

`make -C rocq all` must reproduce the committed `.ml`/`.mli` exactly.

### 5.5 Axioms and primitives

`Print Assumptions find_deletions_eligible` and
`make -s -C rocq assumptions` list them: the axioms of
`rocq/OxCamlInt64.v` (each the documented behaviour of one `Int64`
operation), the equality and set axioms of `SsaModel.v` (identity of
blocks and values, sets ordered by block id), the axioms about the list
functions mapped onto OxCaml's in `rocq/OxCamlList.v`, the four overflow
tests of `rocq/IntArith.v`, and Rocq's own axioms for its primitive
63-bit integers, plus classical logic, used for case splits on
definedness. None of them states anything about SSA executions.

### 5.6 The gate and the limits

`DeleteEmptyLoops.graph_supported` must run before the analyses and
reject the whole graph on failure (see `find_deletions`); its
`graph_block_limit` extracts to `4_294_967_296`. The Fourier-Motzkin and
linearization budgets (`FourierMotzkin.v`, `AffineSsa.v`) only make the
analyses more conservative.

## 6. Reading order and commands

1. `LoopEligibility.v`, part 1: `eligible_entry` and
   `find_deletions_eligible`. Then print the premises:

   ```coq
   From OxCamlLoop Require Import LoopEligibility.
   Check @find_deletions_eligible.
   Print Assumptions find_deletions_eligible.
   Print eligible_entry.
   Print SsaSemantics.steps_until.
   Print SsaProgress.finished_graph.
   Print SsaContext.reached_header.
   ```

   The theorem is not in a `Section`, so `Check` shows every premise.
2. `SsaSemantics.v` parts 1 and 2, `SsaLiveness.v` parts 1 and 2, then
   `SsaContext.v` up to `reached_header`: section 3 above, against the
   backend (section 5.1).
3. `SsaProgress.finished_graph` and `SsaExecution.well_formed_ssa`
   against `ssa.ml` (section 5.2).
4. `DeleteEmptyLoops.v` parts 1 and 2, and the theorem statements of the
   analysis models, for what the certificate says.
5. The `*Extract.v` files (section 5.4) and the OCaml wrapper
   (section 4).
6. `make -C rocq all` (Rocq 9 and the repository's `ocamlformat` on
   `PATH`) checks every proof and confirms that the extraction
   reproduces the committed files; `make -s -C rocq assumptions` prints
   the audit report, whose first part lists the global axioms and whose
   second prints every declaration's type, Section premises included.

Proof scripts (`Proof. ... Qed.`) can be skipped: Rocq checks them. The
tests are in `testsuite/tests/backend/ssa/loop/`, where the
`[%%expect_ssa X86_64{|...|}]` blocks show the SSA each function ends up
with and the number of loops deleted (they are checked on x86-64 only,
since the SSA is printed after instruction selection), and in
`oxcaml/tests/backend/ssa/`.

## 7. What not to take on trust

- A hypothesis stated through an analysis function silently trusts that
  function. The development avoids this at the top: `finished_graph` and
  `reached_header` are stated through the primitives of `SsaModel.v` and
  the semantics, never through part-1 code. Lower down, hypotheses such
  as `biv_spec` or `deletion_certificate` are stated through code, but
  the development discharges them itself (`InductionVar.analyze_spec`,
  `find_deletions_spec`, ...), so nothing at the top depends on their
  truth of the program. When reading a lemma, ask which of its premises
  the main theorem discharges and which it passes on.
- A conclusion stated through code cannot make a theorem vacuous; it only
  means opening the code to learn what is claimed.
- A valuation of all syntax (`LoopValueSnapshot.snapshot_value`) is a
  proof device. The only thing proved about it is agreement with the
  registers on available canonical values; no theorem gives a meaning to
  a fabricated expression.
- Equality of values is identity of the compiler's data
  (`Ssa.Value.equal` compares the instruction id and the result index).
  Two values of one graph are equal to it exactly when they are the same
  term, and the models never construct values.
- The analyses may be wrong in the conservative direction. A kept loop
  needs no justification; a deleted one needs all of section 1.
