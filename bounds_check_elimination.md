# SSA range analysis for array bounds-check elimination

## Goal

Eliminate provably-redundant array bounds checks in the SSA backend, for
induction variables whose range is bounded by an array's length — both
explicit `for` loops (`for i = 0 to Array.length a - 1`) and recursive
functions that get loopified to the same shape. flambda2 leaves these checks
in at `-O3`, so there is real value in removing them here.

## What the checks look like in SSA

A checked read `a.(i)` lowers (via to_cmm) to an unsigned comparison guarding
the load, with the out-of-bounds edge raising `Invalid_argument`:

```
  v = idx <u len            ; unsigned compare, len = (tagged) Array.length
  if v then goto IN else goto RAISE
IN:  ... load a[idx] ...
RAISE: raise Invalid_argument
```

`idx <u len` is exactly `0 <= idx < len` (unsigned subsumes the negative case).
Everything is on tagged machine integers; the analysis treats the tagging ops
(`<<1`, `+1`, `|1`, `>>s1`) as ordinary integer operations, so tagged and
untagged values are handled uniformly.

Two representative shapes (see `foo.ml`):

**Loopified recursive (`sum_rec`)** — guard at the top of the loop:
```
B2: header(i, acc)              ; i: IV, init 0, step +1 (tagged: init 1, step +2)
  if i >=s n then return acc else goto B4
B4:                             ; reached only via the else edge ⇒ i <s n holds
  if i <u n then load else raise
```
The check `i <u n` is *dominated* by a branch establishing `i <s n`. With
`i >= 0` (the IV starts at 0 and only increases), `0 <= i <s n` ⇒ `i <u n`.

**`for` loop (`sum_for`)** — bound test at the bottom (rotated loop):
```
B3: header(counter, acc)
  i = 1 + (counter << 1)              ; tagged index = 2*counter + 1
  if i <u Parraylength then B4 else raise   ; the check is the header terminator
B4: ... load ...
  if counter+1 <=s for_stop_naked then B3(back) else return
                                      ; for_stop_naked = (Parraylength - 2) >>s 1
```
No dominating guard, so we need the IV range: `counter ∈ [0, for_stop_naked]`,
giving `i = 2*counter+1 <= 2*for_stop_naked+1`. Using the shift identity
`2*(x >>s 1) <= x` with `for_stop_naked = (Parraylength-2)>>s1`, we get
`i <= Parraylength-1 < Parraylength`, and `i >= 1 > 0`.

## Abstract domain

Each SSA value is described by an **affine form** over *atoms*:
`const + Σ cᵢ·atomᵢ`, where an atom is an SSA value we do not decompose
(function params, loads, multiplies-by-variable, and — see below — right
shifts). `linearize` builds these structurally over `Const_int`, `Iadd`,
`Isub`, `Intop_imm (Iadd | Isub | Ilsl k)`. Anything else becomes an atom.

A **fact** is an inequality `affine >= 0`. A bounds check `idx <u len` is
eliminated when the fact set entails both `idx >= 0` and `len - idx - 1 >= 0`
(together `0 <= idx < len`; see *Soundness* below for why this licenses
removing the unsigned check).

Index computations are frequently target-specific instructions: on arm64
`i = 1 + counter<<1` is `Ispecific (Ishiftarith (Ishiftadd, 1))`, on amd64 it
would be an `Ilea`. `linearize` therefore asks the target via a new
`Arch.specific_operation_as_affine : specific_operation -> (int array * int)
option` hook, which reports a specific op as `disp + Σ coeffᵢ·argᵢ` when it is
an affine combination of its arguments (and `None` otherwise). The hook lives
in `backend/arm64/arch.ml` and `backend/amd64/arch.ml` so the SSA pass stays
target-independent.

## Where the facts come from (all from real def-use edges)

1. **IV bounds** (reusing `Induction_var.Make(S)`):
   - *Lower*: for a basic IV with constant step > 0, `i - min(init) >= 0`.
     Sound because `i` is non-decreasing and starts at `init`.
   - *Upper* (rotated loops): the value flowing on the back edge is the next
     header value; if a guard dominating the back-edge source bounds it by a
     loop-invariant `U` (i.e. `U - backedge_arg` is a recorded fact and `U` is
     loop-invariant), and every entry edge satisfies `init <= U`, then
     `U - i >= 0` is a loop invariant.
2. **Dominator guards**: walking the immediate-dominator chain to the check's
   block, each dominating `Branch{cond=(a ◦ b); ifso; ifnot}` contributes
   `a ◦ b` (taken edge `ifso`) or its negation (edge `ifnot`) as a linear
   fact. Only signed comparisons are linearized (`Clt/Cle/Cgt/Cge/Ceq`);
   unsigned and `Cne` are dropped (can't be expressed soundly as a single
   affine inequality). This discharges `sum_rec`.
3. **Shift side-facts**: when `linearize` meets `x >>s k`, it atomizes the
   result `t` and records the *tautologies* `2ᵏ·t <= x` and `x <= 2ᵏ·t + 2ᵏ-1`
   (true for all `x` — no parity assumption). The lower one closes `sum_for`.
4. *(Optional, sound)* recognized array-length values are `>= 0`.

## Decision procedure

**Fourier–Motzkin** over the rationals: to decide `facts ⊨ goal >= 0`, add the
integer negation `-goal - 1 >= 0` and test the system for
infeasibility (eliminate atoms pairwise; if a constant `< 0` is forced, the
system is UNSAT and the goal is entailed). Rational-UNSAT implies integer
entailment, so this is sound. Atom counts per loop are tiny, so cost is
negligible. This is a real, complete-for-the-fragment procedure rather than a
bag of ad-hoc rules.

## Soundness

### Array identity

Every fact is derived from an actual def-use edge (`for_stop = Parraylength-2`
is a real definition; the shift bounds are tautologies; guards come from real
branches). So a check `idx <u len` is discharged *only* when the loop bound and
*that specific* `len` are genuinely linked in the data flow. A loop bounded by
a different array's length simply will not produce facts that entail the goal,
so the check is left in place. We never pattern-match "this looks like a
length" to assume a relation, so we cannot be fooled into using the wrong
array. (An explicit "same array base for the access and the length" check can
be layered on as belt-and-braces, but is not required for soundness.)

### Signed/unsigned and overflow (correcting "63-bit rules out wraparound")

Replacing the machine op `idx <u len` (a 64-bit *unsigned* compare) with
`true` is sound iff, for the actual register values at that point,
`idx <u len` always holds. The analysis proves, over the integers, `0 <= idx`
and `idx < len`. Two things make that imply the unsigned comparison:

1. **Signed facts ⇒ unsigned conclusion.** The facts constrain the values'
   two's-complement (signed) interpretations. If `0 <= idx` and `idx < len`
   hold as signed integers, then `idx ∈ [0, len) ⊆ [0, 2^63)`, so both
   operands have their sign bit clear and their unsigned interpretation equals
   their signed value; hence `idx <u len ⇔ idx < len`. This step needs *only*
   `idx >= 0` and `idx < len` — no width assumption.

2. **The facts hold for the actual values (no overflow).** Each fact must be
   valid for the run-time 64-bit values:
   - Signed-comparison guards (`<s`, `>=s`, …) and the shift identities
     `2^k·(x>>s k) <= x <= 2^k·(x>>s k) + 2^k-1` are exact for the actual
     signed value — unconditionally sound.
   - The affine *decomposition* of an index op (`idx = 1 + 2·counter`) and
     **IV monotonicity** (`i >= init`) are the only places a 64-bit
     *signed overflow* could falsify a fact.

   These last two are sound here because the quantities are OCaml `int`s. An
   OCaml `int` has a 63-bit logical range `[-2^62, 2^62)`; its machine encoding
   is the untagged value itself (`< 2^62`) or the tagged `2v+1 ∈ (-2^63, 2^63)`
   — both strictly inside the signed 64-bit range. The operations a loop index
   undergoes (tag/untag, `+ const`, `× 2^k` for the small `k` of an element
   scale, and monotone `+ step`) map a 63-bit logical value to an in-range
   64-bit word, so no signed overflow occurs and the affine/monotonicity facts
   match the machine. *This* is the precise content of the discarded "63-bit"
   remark: 63-bit logical width is what keeps the (possibly doubled) machine
   encodings and their loop-stepping inside `[-2^63, 2^63)`.

   The guarantee is specifically about **`int`-typed (63-bit) operands**, which
   is the case for ordinary array / string / bytes bounds checks. For an index
   or length that is a full-width `Nativeint`/`Int64`, a monotone IV could in
   principle wrap, so the pass must not fire there. `Cmm.machtype_component` is
   `Int` for both tagged `int`s and unboxed `int64#`/`nativeint`, so we cannot
   distinguish them by type. Instead the pass keys off the *shape* the frontend
   only ever emits for a genuine array / string / bytes bounds check — an
   unsigned (`<u`/`<=u`) comparison guarding a load whose out-of-bounds edge
   **raises** — and whose operands are therefore tagged `int`s. Concretely,
   `try_eliminate` only fires when the `ifnot` edge of the checked branch
   reaches a `Raise`/`Invalid` terminator (through a chain of gotos); a
   hand-written unsigned comparison on `int64#`/`nativeint` values does not have
   this raising failure path, and in any case the wrap it would need requires an
   unrealizable ~2^63 iterations. (A stronger, self-contained alternative — not
   relied on here — is that an in-bounds index is `< len ≤ Sys.max_array_length
   ≈ 2^57`, reached by `< 2^57` monotone steps from `0`, hence far below any
   overflow threshold; this is why recognising `len` as a genuine array length
   would let us drop even this restriction.)

## Transformation

When both goals are entailed, rewrite the bounds-check terminator
`Branch{cond; ifso; ifnot}` to `Goto ifso` (via `Block.set_terminator`). The
raise path becomes unreachable and downstream DCE removes it. No constant needs
to be materialised (a finished graph cannot construct new instructions anyway).

## Integration

A standalone pass `Bounds_check_elim.Make(S : Ssa.Finished_graph)` reusing
`Induction_var.Make(S)`. New flag `-ssa-bounds-check-elim` (default off), run in
`compile_via_ssa` after `Ssa_simplify`, before `Cfg_of_ssa`.

## Staging / validation

- **Stage A** (IV lower bound + dominator guards): eliminates `sum_rec` and any
  explicit `if i < n then a.(i)`.
- **Stage B** (IV upper bound from the back-edge guard + shift facts):
  eliminates the classic `sum_for`.

Validation targets are `sum_rec` and `sum_for` in `foo.ml`, checked via
`ocamlopt -dssa -O3` (the bounds-check `Branch` should become a `Goto`).
