# Overview: pipeline and scope ledger

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter situates the object of this formalism — the Flambda 2 term
language and the Simplify pass — inside the wider compiler pipeline, and then
records honestly, in the **scope ledger**, which parts of Flambda 2 the rest of
the document actually formalizes.

Nothing here is normative; there are no rule blocks. Every factual claim about
pass ordering cites a code anchor (`path#function`) so the claim can be checked
against the code it describes.

## The Flambda 2 pipeline

Flambda 2 is the middle-end of the OxCaml compiler: it consumes `Lambda`, the
untyped desugared IR shared with upstream OCaml, and produces `Cmm`, the
lower-level IR from which the backend generates machine code. (An alternative
`to_jsir` exit exists; see below.) The driver
(`middle_end/flambda2/flambda2.ml#lambda_to_cmm`) threads a program through four
stages.

### 1. CPS and closure conversion (context only)

`Lambda_to_flambda.lambda_to_flambda`
(`middle_end/flambda2/from_lambda/lambda_to_flambda.ml#lambda_to_flambda`)
translates `Lambda` into the Flambda 2 term language in a single combined pass.
Two things happen together. Continuation-passing-style conversion
(`from_lambda/lambda_to_flambda.ml#cps`) makes control flow explicit: every join
point and every branch target becomes a named continuation, and every function
gains a *return* continuation and an *exception* continuation. Closure
conversion (`from_lambda/closure_conversion.ml#close_program`) turns nested
functions and their captured variables into explicit sets of closures with
function slots and value slots.

The resulting IR — "raw Flambda", the term language chapters 02–06 formalize —
has three structural properties that the whole formalism relies on and that are
documented at `middle_end/flambda2/terms/flambda.mli` (module header): it is in
*double-barrelled* CPS (two continuations per function, one for normal return
and one for exceptions); continuations are *second class* (they cannot be stored
in variables or returned, only defined and called); and, despite being CPS, it
retains a conventional `let`-binding construct and is in A-normal form (ANF), so
every intermediate value has a name and there are no nested subexpressions.

CPS and closure conversion are **context only** in this formalism: the term
language they produce is formalized, but the translation *from* `Lambda` is
described here and not given rules. When a later chapter needs to explain why a
term has a particular shape, it may point back to this stage.

### 2. Simplify

`Simplify.run` (`middle_end/flambda2/simplify/simplify.ml#run`), invoked from
`flambda2.ml#flambda_to_flambda0`, is the heart of Flambda 2 and the primary
subject of this formalism (chapters 09–12). It consumes raw Flambda and produces
optimized Flambda together with the final typing environment, the set of all
`Code` bound in the unit, and slot offsets. Simplify is where constant folding,
canonicalization, dead-code elimination, continuation and function inlining,
unboxing, and the switch/`match` optimizations happen; it is driven by the
abstract domain (Flambda types, chapters 07–08) that tracks what is known about
each name.

### 3. Reaper (context only)

When enabled (`-flambda2-reaper` / `-O4`), `Flambda2_reaper.Reaper.run`
(called from `flambda2.ml#flambda_to_flambda0`) runs after Simplify. It is a
*control-flow-insensitive, whole-unit* analysis followed by a rewrite: it
computes, for every variable and every block field / closure slot, the set of
sources and usages, then removes provably dead code, poisons unused values,
changes calling conventions, and performs cross-function and closure unboxing.
Its model is described in `docs/reaper.md`. The Reaper is **context only** here:
it is described but not formalized, and it is off by default.

### 4. to_cmm (formalized in chapters 15–20)

`To_cmm.unit` (`middle_end/flambda2/to_cmm/to_cmm.ml#unit`) lowers optimized
Flambda to `Cmm`. It makes concrete what Flambda kept abstract: tagging/boxing
become arithmetic and allocation, field access becomes address computation plus
memory operations, continuations become either inlined sequential code (for
non-recursive continuations used exactly once) or `Cmm` static jump/catch, and
closure/value slots are assigned concrete offsets. Crucially, `to_cmm` relies on
Simplify having already done all desired unboxing; it introduces box/unbox only
as forced by kinds. Its design is described in `docs/to_cmm.md`, and it is
**formalized** in chapters [`15`](15-cmm.md)–[`20`](20-to-cmm-soundness.md): a
core Cmm machine, the control ([`16`](16-to-cmm-control.md)) and data
([`18`](18-to-cmm-data.md)) translation, the representation relation `≈`
([`17`](17-representation.md)), the concrete allocation/region/GC model
([`19`](19-cmm-memory-gc.md)), and a cross-language soundness statement
([`20`](20-to-cmm-soundness.md)) — for a 64-bit little-endian target
([`15`](15-cmm.md) §0). There is a parallel exit, `To_jsir.unit`
(`middle_end/flambda2/to_jsir/to_jsir.ml#unit`), that lowers to js_of_ocaml's IR
instead; see `to_jsir/README.md`. `to_jsir` remains **context only**.

### -Oclassic mode

Flambda 2 has two modes, selected by `Flambda_features.mode`
(`middle_end/flambda2/ui/flambda_features.ml#mode`, gated on
`flambda_features.ml#classic_mode`). In `Normal` mode the pipeline above runs in
full. In `Classic` mode (`-Oclassic`) the **Simplify pass is skipped
entirely**: `flambda2.ml#flambda_to_flambda0` matches on the `Classic` metadata
produced by closure conversion and returns the raw Flambda unchanged, having
computed the export approximations (for cross-module inlining) *during* closure
conversion rather than during Simplify. Classic mode is a cheaper,
lower-optimization path; it is **context only** here — this formalism describes
`Normal` mode. (The `encyclopaedia.md` companion likewise explicitly excludes
classic mode.)

## How Simplify runs

Simplify is a single traversal with a downwards and an upwards pass over each
expression, carrying accumulators and an abstract environment. Chapter
[`09-simplify-structure.md`](09-simplify-structure.md) formalizes that
architecture — the downwards/upwards split, the accumulators, flow analysis,
lifting, and join points. The concrete rewrites it performs (constant folding,
CSE, switch simplification, continuation inlining/specialization, dead code) are
in [`10-simplify-rewrites.md`](10-simplify-rewrites.md); function inlining, with
its decision oracle and cost model, is in
[`11-inlining.md`](11-inlining.md); and unboxing of continuation parameters,
function parameters, and results is in [`12-unboxing.md`](12-unboxing.md). The
abstract domain those rewrites query is defined in
[`07-types-domain.md`](07-types-domain.md) and
[`08-meet-join.md`](08-meet-join.md).

## Scope ledger

This is a living table. Each phase that extends the formalism should update the
status of the relevant rows. "Formalized" means a chapter gives rules or
judgments for it; "context only" means it is described but deliberately not
given rules; "out of scope" means it is not treated here at all.

| Feature area | Status | Where |
|---|---|---|
| Term syntax: expressions, `named`, binders, `Simple`, name modes, trap actions | formalized | 02 |
| Kinds, subkinds, arities, unarization; well-formedness | formalized | 03 |
| Operational semantics: `Let`, `Let_cont`, `Apply`, `Apply_cont`, `Switch`, `Invalid`, traps, regions | formalized | 04 |
| Scalar primitives (arithmetic, comparison, conversion) | formalized | 05 |
| Memory primitives (blocks, closures, arrays, strings), effects/coeffects | formalized | 06 |
| Abstract domain (types, typing env, aliases, levels, existentials, concretization) | formalized | 07 |
| Meet / join / provers / reification | formalized | 08 |
| Simplify architecture (downwards/upwards, accumulators, flow, lifting, join points) | formalized | 09 |
| Simplify rewrites (const fold, CSE, switch, continuation inlining/specialization, dead code) | formalized | 10 |
| Function inlining (rule + decision oracle + cost model, descriptive) | formalized | 11 |
| Unboxing (continuation params, function params, results) | formalized | 12 |
| CPS conversion (`from_lambda/`) | context only | 01 |
| Closure conversion | context only | 01 |
| Reaper (interprocedural DCE + unboxing) | context only | 01, `docs/reaper.md` |
| to_cmm: Cmm target machine (control + memory ops); control-flow translation; representation relation; data/primitive lowering; allocation/regions/GC; cross-language soundness | formalized (64-bit little-endian) | 15–20; `docs/to_cmm.md` |
| to_cmm: 32-bit / big-endian targets; instruction selection below Cmm; probe/method/effect lowering | out of scope | — (15 §0; 20 §5) |
| to_jsir lowering | context only | 01, `to_jsir/README.md` |
| -Oclassic mode | context only | 01 |
| cmx import/export (`Flambda_cmx`, cross-module info) | context only | 01; 07 §6 |
| Abstract-domain precision: symbol projections, recursion depth / `rec_info` in types, alloc modes in types, individual coercions | modeled shallowly / opaque | 07 §6 |
| Method calls (`Call_kind.Method`: `Self`/`Public`/`Cached`) | out of scope | — |
| Algebraic-effect call kinds (`Call_kind.Effect`: `Perform`/`Reperform`/`With_stack`/`Resume`) | out of scope | — |
| Probes (`Probe_is_enabled`, `Enter_inlined_apply`) | out of scope | — |
| SIMD vectors: naked-vector values/constants, boxed vectors, vector arrays, 128/256/512-bit string/bytes/bigstring accessors, SIMD reinterpret array access, `Reinterpret_boxed_vector`, representation and lowering | formalized | 04 §1.1; 05 (`P.Unary.ReinterpretBoxedVector`); 06 (`P.Binary.ArrayLoad.Vector`, `P.Ternary.ArraySet.Vector`, `P.Unchecked.WideAccess`); 15 §1–2; 17 (`R.Val.NakedNumber`, `R.Obj.Boxed`, `R.Obj.Array`); 18 (`TC.Prim.BoxUnbox`, `TC.Prim.ReinterpretBoxedVector`, `TC.Prim.StringLoad`, `TC.Prim.ArrayAccess.Vector`) |
| SIMD intrinsic *operations* (arch-specific vector arithmetic `external`s recognized below Cmm; the vector `Cstatic_cast`/`Creinterpret_cast` variants) | out of scope | — (15 §1) |
| Mixed-block detailed layout (`Mixed_block_shape`, value-prefix/flat-suffix split) | formalized | 06 (`P.MixedShape.*`, `P.Variadic.MakeBlock.Mixed`, `P.Unary.BlockLoad.Mixed`, `P.Binary.BlockSet.Mixed`, `P.Static.MixedBlock`); 03 (`WF.Prim.MakeBlockMixed`, shape grammar); 07/08 (block-shape typing, `T.Meet.BlockShape`) |
| Bigarrays (`Bigarray_load`/`_set`/`_length`/`_get_alignment`, kinds/layouts, element decode, multi-dim indexing) | formalized | 06 (`P.Binary.BigarrayLoad`, `P.Ternary.BigarraySet`, `P.Unary.BigarrayLength`, `P.Binary.BigarrayGetAlignment`, `P.Bigarray.Indexing`); 17 (`R.Obj.Bigarray`); 18 (`TC.Prim.BigarrayAccess`, `TC.Prim.BigarrayLength`) |
| Flambda invariant checks (`Flambda_features.check_invariants`) | out of scope | — |

Notes on the "out of scope" rows: these constructs all exist in the code — the
call kinds in `middle_end/flambda2/terms/call_kind.mli` (`Method`, `Effect`),
and the probe primitives in `middle_end/flambda2/terms/flambda_primitive.mli`
(`nullary_primitive`). They are listed here so their absence
from later chapters is a deliberate omission, not an oversight. The operational
semantics (04) covers `Function` and `C_call` application; `Method` and `Effect`
applications are noted there as unformalized. A later phase may promote any of
these rows to "formalized" — as one did for SIMD *values and accessors* (the
row above): what remains out of scope for SIMD is only the vector *arithmetic*,
which never appears as a Flambda primitive (intrinsics are `external`s lowered
below Cmm by instruction selection).

## Reading map (for humans)

- *What does this term mean / what is its syntax?* → 02 (syntax), 03 (kinds and
  well-formedness).
- *What does this program do when it runs?* → 04 (operational semantics), 05–06
  (what each primitive computes).
- *What does Simplify know about a name, and how?* → 07 (the type grammar and
  typing environment), 08 (how types are combined and queried).
- *Why did Simplify make this change (or not)?* → 09 (when/where rewrites fire),
  10 (the local rewrites), 11 (inlining decisions), 12 (unboxing decisions).
- *Is the optimizer allowed to do this — is it sound?* → 13 (soundness statement
  and global invariants).
- *Does the formalism match the code on a concrete example?* → 14 (validation
  case studies).
- *Which rule covers X, and where is it implemented?* → `rule-index.md`.

For prose background outside the formalism proper, see the companion documents
in the parent directory: `../encyclopaedia.md` (optimizations as
source-to-source transformations, Normal mode only), `../types.md`,
`../inlining.md`, `../unboxed_params.md`, `../backend_exceptions.md`.
