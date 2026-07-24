# Plan: `Eval.inject` — injecting runtime values into quotes

Status: plan only, no implementation yet.  Companion doc:
`standalone_approx_plan.md` (the `%reify_approx` primitive and
`Value_approximation.Standalone`, both already implemented on this branch).

## Summary

`inject` lets metaprograms embed arbitrary runtime values (refs, closures,
file descriptors, ...) into quotes, valid only for evaluation within the
current program execution.  An `Injector.t` represents a compilation unit
synthesised at runtime: `inject` stores the value under a fresh name and
returns a quote that is just an identifier reference `Inject_cu.name`;
`eval` builds the module block, cmi and cmx for that unit in memory and
registers them with the compiler/JIT machinery before compiling the quote.

Target API (from the design doc):

```ocaml
module Eval : sig
  module Injector : sig type t end
  val inject : Injector.t @ <[open]> -> 'a eval -> 'a expr @ <[open]>
  val eval :
    (Injector.t @ <[open]> -> 'a expr @ <[open]>) @ local once -> 'a eval
end
```

## What exists today (survey)

- **Quote runtime**: `stdlib/camlinternalQuote.ml{,i}` — `Code.t` /
  `Exp.t` ASTs; crucially `Identifier.Module.global_module : string -> t`,
  `Identifier.Value.dot`, `Exp.ident`, `Code.of_exp`, so `inject` can build
  the `Inject_cu.name` quote directly.  `stdlib/quote.ml` (`Expr.int` etc.)
  is the model: build a `Code.t`, `Obj.magic` it to the typed `'a expr`.
- **Eval pipeline**: `otherlibs/eval/eval.ml` — closes the quote, prints it
  (`let eval = (...)`), re-parses, typechecks (`Typemod`), translates
  (`Translmod` + `Slambda.eval` + `Simplif`), then JIT-loads
  (`Jit.jit_load_program`) and reads the module block via
  `Jit.jit_lookup_symbol`.  Serialised under `compile_mutex`.
- **Artifact supply**: cmi/cmx bundles are marshalled into the executable
  (`caml_bundled_cmis_this_exe` etc.); cmis are served via the
  `Persistent_env.Persistent_signature.load` hook and cmxs via
  `Compilenv.cache_unit_info`.  Bundle contents are driven by
  `ui_quoted_cmi` / `ui_quoted_cmx` (written from `Env.quoted_impls ()` in
  `middle_end/compilenv.ml:331`).
- **JIT**: `external/ocaml-jit/lib/jit.ml` keeps a global symbol table
  (`Globals.symbols`), aggregated per loaded phrase, with fallback to
  `ndl_loadsym` for statically linked symbols.  There is a lookup entry
  point but no way yet to *register* a symbol at a runtime address.
- **Classic-mode cmx approximations**:
  `Flambda_cmx.prepare_cmx_from_approx ~machine_width ~approxs:(...
  Value_approximation.t Symbol.Map.t) ~module_symbol ...` builds exactly the
  export info we need; `load_symbol_approx` is the read side.
- **`%reify_approx` + `Standalone`** (done, this branch): `reify_approx foo`
  compiles (classic mode, at Closure_conversion time) to a constant string —
  the marshalled `Value_approximation.Standalone.t` of `foo`'s
  approximation.  `Standalone.of_marshalled_string` +
  `Standalone.to_approximation ~find_code` rebuild a real
  `Code_or_metadata.t Value_approximation.t`.
- **Current-module references**: known broken-ish; see
  `testsuite/tests/quotation/eval/current_unit/eval_before_init_ok.ml`
  (internal ticket 7260), disabled under `-Oclassic`.

## Milestone 1 — Injector core (correctness, no optimisation)

All in `otherlibs/eval` unless noted.

1. **Types**.
   ```ocaml
   module Injector = struct
     type entry = { name : string; value : Obj.t }
     type t = { cu_name : string; values : entry Dynarray.t }
   end
   ```
   `cu_name` from a gensym with a reserved, unlinkable-by-accident prefix,
   e.g. `Eval_inject__<counter>` (the existing `Eval___<i>` pattern; keep a
   distinct prefix so injector units are recognisable).  Value names
   `v0, v1, ...`.

2. **`inject`**: push `{name; value = Obj.repr x}`; build the quote as
   `Code.of_exp Loc.unknown (Exp.mk (Exp_desc.ident (Identifier.Value.dot
   (Identifier.Module.global_module cu_name) name)) [])` and `Obj.magic` to
   `'a expr` (mirror of `Quote.Expr`).

3. **`eval f`**: create the injector, apply `f`, then before the existing
   compilation pipeline runs:
   - **Module block**: allocate a block of size `Dynarray.length values`
     with the values as fields.  Because JIT-generated code will refer to it
     by symbol (a data reference, not traced by the GC), the block must be
     kept alive and stationary: C stub in `eval_stubs.c` that mallocs a
     one-word cell holding the block pointer and calls
     `caml_register_generational_global_root` on it — this pins liveness;
     for address stability the block itself should be allocated outside the
     moving heap (e.g. `caml_alloc_shr`? No — major-heap blocks don't move,
     but ensure it is not young: allocate via a stub using
     `caml_alloc_shr`/`caml_alloc` + `caml_oldify` or simply build it in
     OCaml and `Gc.minor ()`... decide in review; the stub approach with an
     explicit major-heap allocation is the least subtle).  The *root cell*'s
     address is what the symbol resolves to iff generated code goes through
     one indirection; simpler is to resolve the symbol directly to the block
     and rely on major-heap non-moving + the root for liveness.  Removal of
     the root waits for code unloading (per design doc).
   - **cmi**: construct `Cmi_format.cmi_infos_lazy` in memory: a signature
     of `Sig_value` items, one per injected name, each of type `'a`
     (fresh genericised `Tvar`).  Serve it by extending the
     `Persistent_env.Persistent_signature.load` hook already installed in
     `eval.ml` (a `Hashtbl` of injector units consulted before the bundle
     map).  Decision recorded: expose `'a` rather than `Obj.t`, and rely on
     the typed `inject` API for soundness; revisit if the typer needs
     `type_inspection`-style annotations (see "Printing" below).
   - **cmx**: minimal `Cmx_format.unit_infos` — correct `ui_unit`,
     `ui_defines = [unit]`, `ui_format` describing a flat block of N values
     (`Lambda.main_module_block_format`), `ui_export_info = None` for M1 —
     registered with `Compilenv.cache_unit_info`.
   - **JIT symbol registration**: add to `external/ocaml-jit`:
     `Jit.jit_register_symbol : string -> Obj.t -> unit` inserting into
     `Globals.symbols` so relocations against the injector unit's module
     symbol (`Symbol.for_compilation_unit cu |> Symbol.linkage_name`)
     resolve to the block.  Check the relocation path handles an
     absolute data address of a heap block (same mechanism the JIT uses for
     previously JIT-loaded data symbols).
   - The rest of the existing `eval` pipeline is unchanged: the quote now
     merely contains `Inject_cu.vN` identifiers, which typecheck against the
     in-memory cmi and compile to loads from the registered module block.

4. **API surface for M1**: ship without the `<[open]>` mode
   (`val inject : Injector.t -> 'a eval -> 'a expr`,
   `val eval : (Injector.t -> 'a expr) @ local once -> 'a eval`), with the
   escape hazard documented; mode enforcement is Milestone 3.  Optional
   interim guard: stamp each injector-produced `Code.t` with the injector id
   and fail `eval` if a quote mentions a foreign/expired injector unit
   (scan of identifiers at eval time — cheap, catches the worst misuse:
   evaluating an injector reference in a different `eval` or after
   printing/reloading).

5. **Printing** (design-doc note): `Quote.print` on an injected expr
   currently would print `Inject_cu.vN`, which is meaningless outside the
   process.  Follow the doc: print as `[%inject "..."]` (an extension node
   carrying the name), so debug output is recognisable; the `eval`
   implementation never round-trips through printing for injector units
   beyond the existing `let eval = (...)` pipeline, which is fine because
   the cmi makes the identifier well-typed (`'a` + the usual annotation
   discipline of `Typedtree.type_inspection` for inspected types).

6. **Tests** (`testsuite/tests/quotation/eval/inject/`): inject an `int
   ref` and mutate through the evaluated code; inject a closure and call
   it; two injectors in one program; injected value inside a bigger quote
   (splice); GC stress (minor+major cycles between inject and eval, and
   after eval, verifying the block stays valid); re-eval of a second quote
   from the same... (not allowed — `once`); error case for the interim
   guard.

## Milestone 2 — Optimisation via `%reify_approx`

1. **`approx` type**: in `Eval`, `type approx` abstract; internally the
   marshalled `Standalone.t` string.  Declaration inside eval:
   `external reify_approx : 'a = "%reify_approx"` used at type
   `_ -> approx` (our primitive has type `'a`; note each application site
   needs warning 20 silenced).  Entries become
   `{ name; value; approx : approx }`.

2. **The inlining problem (important design wrinkle)**.  The doc's
   `let[@inline always] inject t x = ... reify_approx x ...` does **not**
   work with the current primitive: `reify_approx x` is resolved to a
   string constant during Closure_conversion *of the unit that defines
   `inject`* (where `x`'s approximation is Unknown).  Classic-mode inlining
   then splices already-lowered Flambda into callers, so every call site
   sees the baked-in Unknown string.

   **Decision (from Mark, 2026-07-21)**: post-inlining resolution (a second
   pass over inlined bodies) is ruled out.  We do call-site expansion,
   without ppx, directly in the compiler:
   - Declare `inject` itself as a surface primitive:
     `external inject : Injector.t -> 'a eval -> 'a expr = "%inject"`
     (arity 2; name TBD).
   - `translprim` lowers a full application `inject t x` to (native):
     `let tmp = x in
      <inject_with_approx> t tmp (Lprim (Preify_approx, [tmp], loc))`
     where `<inject_with_approx>` is the ordinary runtime injector function
     referenced via `Lambda.transl_prim` (`lambda.mli:1293`; precedent:
     `CamlinternalLazy.force`).  This requires the injector runtime to live
     in the stdlib (natural home: `CamlinternalQuote`, which already has
     `Dynarray`/`Obj` available; `Eval` re-exports it).  The `Preify_approx`
     application is now syntactically in the *user's* unit, so
     Closure_conversion resolves it with the caller's approximation of `x`
     (closure conversion tracks approximations of let-bound variables, so
     the `tmp` binding is fine).
   - Bytecode lowering: same call with `Lconst (Const_immstring "")` as the
     approx argument (matching the existing bytecode stub semantics).
   - Partial applications of `inject` get eta-expanded by `translprim`; in
     the wrapper the reified argument is the wrapper's parameter, so the
     approx degrades to Unknown — acceptable, and equivalent to the
     non-inlined case in the design doc.
   - `[@inline always]` on an OCaml-level `inject` is then unnecessary.

3. **Building the injector cmx export info**: at `eval` time, for each
   entry demarshal the approx and rebuild it:
   - `Standalone.of_marshalled_string` then
     `Standalone.to_approximation ~find_code`.
   - `find_code ~code_id:_ ~code_id_linkage_name ~function_slot:_ ~symbol`:
     when `symbol` is `Some s`, recover code by consulting the *original*
     unit's cmx via the existing loader path
     (`Flambda_cmx.load_symbol_approx` against the Compilenv-cached bundle
     gives a `Closure_approximation` whose `Code_or_metadata.t` — and its
     authoritative code id / function slot — should be preferred wholesale
     over our reconstructed ones).  When `symbol = None`, look the code up
     by `code_id_linkage_name`: `Standalone.code_id` now records the
     original code ID's full linkage name (stamp included — implemented
     2026-07-21 on this branch), which identifies exported code exactly;
     eval-time needs a lookup of the bundled units' `Exported_code` by
     linkage name (small addition, e.g. a lazily-built
     linkage-name-to-code-id index over the cached cmxs).
   - Assemble `approxs : ... Symbol.Map.t` mapping the injector's *module
     symbol* to `Block_approximation` of the per-field approxs (that is how
     classic mode publishes member approximations), and call
     `Flambda_cmx.prepare_cmx_from_approx` to obtain `ui_export_info` for
     the in-memory `unit_infos`.  `Exported_code`: empty — the code bodies
     live in the original units' cmxs, which the eval-time compiler imports
     as normal.
   - The `eval`-time compile runs with `Clflags.set_o3` today; approx-based
     import needs the *classic-mode read path* to be exercised — verify
     `load_symbol_approx`-style import of an approx-only cmx works under
     `-O3` simplify (it should: `Typing_env` has
     `add_approximations`/`type_to_approx` bridges — see
     `typing_env.ml:1087-1244`); otherwise consider compiling eval'd quotes
     with `-Oclassic` when injector approxs are present.

4. **`%reify_approx` under `-O2`/`-O3` (first compile in simplify mode)**.
   **Decision (from Mark, 2026-07-21)**: capture the Simplify-time
   approximation rather than accepting Unknown.  Design:
   - New *Flambda* primitive (none exists today), e.g.
     `Flambda_primitive.Unary (Reify_approx, arg)`, kind Value result;
     pure for effects/coeffects purposes but not CSE-able across distinct
     arguments (it is resolved before to_cmm, so classification mostly
     affects Simplify bookkeeping).
   - `Closure_conversion.close_primitive`: in classic mode, resolve
     immediately as today (Simplify does not run in classic mode); in
     simplify mode, emit the Flambda primitive instead.
   - `Simplify` (new `simplify_unary_prim` case): take the argument's type
     from the current typing env and convert it to a classic-mode
     approximation using the existing conversion — `type_to_approx`,
     currently a local function inside
     `Typing_env.Serializable.extract_symbol_approx`
     (`typing_env.ml:1220`); factor it out / expose an entry point taking
     an arbitrary `Type_grammar.t` plus a `find_code` callback (the
     simplifier has code metadata available via the denv).  Then
     `Standalone.create` -> marshal -> replace the primitive with a lifted
     immutable-string constant (existing lifted-constants machinery).
   - Any occurrence Simplify cannot resolve (shouldn't happen, but e.g.
     unreachable code) falls back to the marshalled `Unknown` string;
     to_cmm must never see the primitive.
   - Note the quoted-cmx marking (item 5 below) must also run at Simplify
     time in this mode.

5. **Quoted-cmx marking**: when `close_primitive` resolves a
   `reify_approx`, every compilation unit mentioned by the resulting
   approximation (symbols, code ids — collectable via
   `Value_approximation.free_names`) must end up in the *current* unit's
   `ui_quoted_cmx`/`ui_quoted_cmi` so those cmxs are bundled and available
   at runtime.  Implementation: thread the CU set from
   `Closure_conversion`'s `Acc` into `Compilenv` (join with
   `Env.quoted_impls ()` at `middle_end/compilenv.ml:331`).

6. **Tests**: inject a top-level function from the test unit and check the
   eval'd call gets inlined (observable via `-dflambda`/inlining report in
   a `compilers.reference`, or behaviourally via `[@inlined]` warnings);
   check a bundled dependency's cmx is present when only reify_approx
   references it.

## Milestone 3 — `<[open]>` mode enforcement

There is no `<[open]>` mode in the tree today (no occurrences in `typing/`;
quotes have a cmi-level `staticity` notion only).  This is a typing work
stream, largely independent:
- new mode (axis or quote-scoped annotation `@ <[open]>` on `expr`s and on
  `Injector.t`) expressing "tied to the current program execution: cannot
  be printed/persisted, cannot escape the `eval` callback";
- `eval`'s callback type `(Injector.t @ <[open]> -> 'a expr @ <[open]>)
  @ local once` makes the injector unusable outside and the resulting expr
  un-leakable;
- `Quote.print`/`string_of_expr` must require the non-open mode (or accept
  open exprs and print `[%inject ...]` for debugging only, per the doc).
Until it lands, M1's runtime guard covers the gap.  Needs its own design
review with the modes folk; keep this plan updated with the outcome.

## Milestone 4 — Current-module references (ticket 7260)

Reuse the injector mechanism for quotes mentioning the enclosing (possibly
not-yet-initialised) module:
- `CamlinternalQuote`: new constructor (design doc: `Ref of Obj.t *
  Path.t`) — an identifier quote carrying both the runtime value and the
  source path.  Built by `lambda/translquote.ml` when quoting a reference
  to the current unit: instead of (or in addition to) reading the module
  block, capture the value directly (for `let`-bound values already
  initialised at quote-creation time this is a plain read of the local
  binding — which also fixes the before-initialisation hazard, since quote
  creation sites dominate their uses of the bound value).
- Printing for later/off-line evaluation prints just the path.
- Immediate `eval`: scan the closed quote for `Ref` nodes, `inject` each
  into the (possibly implicit) injection module, taking the optimisation
  data from the path — i.e. the current unit's own approx for that symbol,
  which is available at runtime because the current unit's cmx is bundled
  when it requires metaprogramming.  Note this needs an injector to exist
  even for the plain `eval : 'a expr -> 'a eval` entry point: create an
  internal one on demand.
- Re-enable/extend `eval_before_init_ok.ml` (including `-Oclassic`).

## Open questions / risks

- **Code-id round-trip for symbol-less closures**: RESOLVED (2026-07-21).
  `Standalone.code_id` now stores the code ID's full linkage name
  (stamp-bearing, hence exact), implemented on this branch;
  `to_approximation`'s `find_code` receives it as
  `code_id_linkage_name:Linkage_name.t`.  Remaining work is the eval-time
  `Exported_code`-by-linkage-name lookup (M2.3).
- **First compile under `-O2`/`-O3`**: DECIDED (2026-07-21) — capture the
  Simplify-time approximation via a new Flambda primitive and convert it to
  a classic-mode approximation with the factored-out `type_to_approx`
  (M2.4).  The eval-time direction (importing an approx-only cmx under
  `-O3`) still needs verifying (M2.3).
- **GC/address stability of the module block** (M1.3): confirm major-heap
  blocks are acceptable relocation targets for the JIT, or add an
  indirection cell; this is the most likely source of subtle bugs — write
  the GC stress test first.
- **Persistent-env/global state**: injector units join the same mutated
  compilerlibs state noted as fragile at the top of `eval.ml`; injector
  unit names must never collide with real units (reserved prefix; also
  consider rejecting on-disk cmis with that prefix).
- **Thread safety**: `inject` mutates the injector without a lock; the
  callback runs before `compile_mutex` is taken.  Either document
  single-threaded use of an injector or make `Dynarray` pushes locked.
- **Marshal versioning**: injected approx strings never leave the process
  (same compiler build), so `Standalone` marshal compatibility is not a
  concern for inject itself.

## Typing: inversion of the [eval] reduction (implemented 2026-07-21)

The design-doc signature `inject : Injector.t -> 'a eval -> 'a expr` was
not callable at any concrete type: unification had no inversion for [eval]
(only the forward reduction `<[ty]> eval --> ty`), and annotations cannot
help because they reduce eagerly.  Implemented in `typing/ctype.ml`:

- `is_qeval_ground_ty`: a type built entirely from arrows, (unboxed)
  tuples and toplevel-in-quotations type constructors, with no variables —
  exactly the types on which [eval] acts as the identity.
- `lift_qeval_ground_ty`: structural copy of such a type (sharing nodes
  builds stage-incoherent types on which expand/reduce diverges — learnt
  the hard way).
- Four new cases in `unify3`, guarded by `is_flexible_ty` (so rigid
  eval-towers keep their existing error messages): `'a eval ~ ty` with
  [ty] ground solves `'a := <[ty]>`; similarly for [Tquote_eval].

Consequence: injected values must have *ground* types; polymorphic values
(e.g. [List.rev]) need a monomorphising annotation at the inject site.
Tests: `testsuite/tests/quotation/typing/eval_inference.ml`.

## JIT bug found and fixed along the way (2026-07-21)

`Eval.eval <[ List.rev [1; 2; 3] ]>` segfaulted on the branch baseline
(pre-existing; nothing to do with inject).  Root cause: the arm64 binary
emitter (`backend/arm64/binary_emitter/encode_directive.ml`) emits
same-section `.quad local-label` data references (e.g. the tail pointers of
static list constants) macOS-style: relocation against the nearest global
symbol with the offset stored *inline in the data*; but the JIT relocation
application (`Relocation.compute_value`, `R_AARCH64_ABS64`) overwrites the
slot with the bare symbol address, losing the offset — so every cons cell
tail pointed at the module block, and walking the list dereferenced a
block header (the 0xb00 crash).  Fix: when assembling for the JIT, keep
the original (directly resolvable) target and carry the addend in the
relocation record, which `For_jit.compute_value` already applies; the
cross-section and unresolved-symbol paths got the same record-addend
treatment.

## Progress log

- [x] M1: injector core.  `CamlinternalQuote.Injector` +
      `inject_with_approx`; `%inject` surface primitive lowered in
      `translprim` to a call of the runtime function with
      `Preify_approx` applied at the call site (no new Lambda constructor
      needed); `Eval.Injector`/`inject`/`eval_with_injector`; out-of-heap
      module block via `caml_eval_alloc_injected_module_block` (dyn-global
      root; `caml_initialize` for the write barrier); in-memory cmi by
      typing a synthetic `val vN : 'a` interface (saving-normalised and
      forced); `Jit.jit_register_symbol`; layered
      `Persistent_signature.load` hook.  Test:
      `testsuite/tests/quotation/eval/inject_test.ml` (refs, closures, GC
      stress incl. compaction, empty injector).
- [x] M2.2: call-site reification (see decision above) — done as part of
      the `%inject` lowering.
- [x] M2.3: injector cmx export info built at eval time
      (`build_injector_export_info` in `otherlibs/eval/eval.ml`): module
      symbol mapped to a block approximation of the demarshalled entries;
      code recovered via `Flambda_cmx.load_symbol_approx` against the
      Compilenv cache; `prepare_cmx_from_approx`.  Remaining CR: recover
      code by `code_id_linkage_name` for symbol-less closures.
      Two consumption bugs found and fixed while verifying propagation
      (2026-07-21):
      1. The current unit must NOT be excluded from the reified-approx
         quoted-cmx marking: approximations routinely reference the
         program's own symbols, and the unit's cmx is resolved at link time
         like any other, so excluding it left `find_code` unable to recover
         the code at runtime.
      2. Closure approximations must be replaced *wholesale* by the
         authoritative ones from the original unit's cmx
         (`prefer_authoritative` in eval.ml) whenever a symbol is
         available: the code IDs and function slots reconstructed from the
         standalone form have fresh stamps, which do not meet the original
         ones referenced by the closure types of other units, silently
         degrading the type to Unknown.
- [x] Verification tooling and results (2026-07-21):
      - `OCAML_EVAL_DUMP=rawflambda,flambda,cmm` dumps the IRs of the
        runtime compilation of evaluated quotes to stderr;
        `OCAML_EVAL_SHOW_WARNINGS=1` lets its warnings through (notably 55
        from `[@inlined]`); `OCAML_EVAL_DEBUG_INJECT=1` traces the
        approximation reconstruction and prints the injector unit's
        exported typing env (self-check).
      - Results: an injected constant constant-folds at eval time
        (`$(inject n) + 1` becomes `43`); a call to an injected top-level
        function under `[@inlined]` is inlined silently (the whole phrase
        folds to a constant); `Sys.opaque_identity` control and symbol-less
        local closures produce warning 55 as expected (the latter pending
        the `code_id_linkage_name` lookup).  Regression test:
        `testsuite/tests/quotation/eval/inject_inlining_test.ml` (runs with
        `OCAML_EVAL_SHOW_WARNINGS`, expects the control's warning and the
        top-level function's silence).
- [x] M2.5: quoted-cmx marking for reified approximations:
      `Value_approximation.compilation_units` (shared helper) →
      `Acc.reified_approx_units` → `close_program_result` → `flambda2.ml`
      → `Compilenv.record_reified_approx_units` → `ui_quoted_cmx`.
      NOTE (from Mark): when M2.4 lands, the same recording must also
      happen at Simplify's resolution site (its own accumulator in the
      downwards accumulator, surfaced through `Simplify.run`'s result,
      then recorded by `flambda2.ml`); the shared helper is where the
      unit computation lives so it is not duplicated.
- [x] Tests: `inject_classic_test.ml` (-Oclassic first compile; real
      approximations; same-unit functions, cross-unit `List.rev`
      monomorphised, local closure, ref).
- [x] Symbol-less local closures (2026-07-22).  Two mechanisms:
      1. *Manufactured lookup symbols* (classic mode): for every reified
         closure approximation without a symbol, `close_primitive`
         manufactures a symbol and registers the closure's full
         approximation in the unit's exported approximations
         (`Acc.add_symbol_approximation`), rooted via the reified-names
         accumulator; the standalone form records it in a new
         `lookup_symbol` field, and the eval side recovers the
         approximation wholesale (original code ID *and* function slot —
         necessary because an inlined body's value-slot projections
         conflict with a fresh-stamped slot in the closure type, producing
         invalid code).
      2. *Code lookup by linkage name* (fallback): reified approximations'
         free names are now extra roots for cmx export reachability
         (`extra_root_names` threaded through `prepare_cmx*`), so the code
         of local closures is exported; `Exported_code.find_by_linkage_name`
         recovers it (with the original code ID) at demarshalling time.
         `Standalone.to_approximation`'s callback now returns a
         `closure_resolution` (`Resolved` / `Code` / `Unknown_code`).
      Tests: local-closure cases in inject_inlining_test.ml (silent
      [@inlined]) and inject_classic_test.ml.
- [x] M2.4: Simplify-time reification (2026-07-22).  New Flambda unary
      primitive `Reify_approx`, emitted by `close_primitive` in simplify
      mode and resolved in `Simplify_unary_primitive.simplify_reify_approx`
      from the argument's type: canonical consts and symbols become
      `Value_const`/`Value_symbol` (symbols recover their full type from
      the original cmx at demarshal time — and at -O3 most values,
      including capturing local closures, are lifted to symbols, so this
      covers a lot); single-closure types become `Closure_approximation`s
      gated on [not is_my_closure_used] (a fresh-slot closure type would
      conflict with the inlined body's value-slot projections — remaining
      CR: needs exported closure types for such closures).  The resolved
      approximation is marshalled and lifted as an immutable-string
      constant.  Reified names are accumulated in `Downwards_acc`
      (mirroring `code_ids_to_remember`, including the save/restore in
      `Simplify_set_of_closures`), surfaced through `Simplify.run`'s
      result, recorded to `Compilenv.record_reified_approx_units` and used
      as `extra_root_names` for the cmx export — the per-phase recording
      structure anticipated by the earlier design note.  `Reify_approx`
      never reaches `To_cmm` (fatal there).  Test:
      inject_inlining_o3_test.ml (control warns; top-level function
      inlines silently at -O3).
      Correction (2026-07-22, from Mark): the tests' local-closure case
      originally sat in the toplevel module initializer, where Simplify
      lifts the closure to a symbol, so the -O3 test was exercising the
      Value_symbol path rather than a dynamic closure.  The case now lives
      under an [@inline never] function.
      Follow-up (2026-07-22): dynamic closures now work under -O3 too.
      The my_closure gate is gone: [simplify_reify_approx] manufactures a
      lookup symbol per symbol-less closure (mirroring classic mode) and
      registers the approximation in a new [Downwards_acc] accumulator
      ([reified_lookup_approxs], with the usual set-of-closures
      save/restore); [Flambda_cmx.prepare_cmx_file_contents] gains
      [~extra_approxs], exported as extra symbol equations merged into the
      serialised typing env via
      [Serializable.create_from_closure_conversion_approx] +
      [Serializable.merge] (such symbols have no definition or static data;
      they are pure cmx lookup keys, as in classic mode).  The eval side
      needed no changes.  inject_inlining_o3_test now expects the dynamic
      closure to inline silently.
      Remaining gap (both modes): closures whose approximation/type is not
      visible at the injection site, e.g. results of non-inlinable calls
      (classic mode does not track call-result approximations; at -O3 the
      callee's result type may not reveal the closure).  Also note the
      reaper (off by default) does not know about reified-approx roots;
      if enabled it could delete code the approximations reference.
- [ ] M4: current-module references.
- [ ] M3: `<[open]>` modes.

## Suggested implementation order

1. M1 (injector core + JIT symbol registration + tests) — no dependency on
   the approx work.  Requires deciding the stdlib home for the injector
   runtime up front (needed by the `%inject` lowering in M2.2).
2. M2.2 (`%inject` surface primitive + call-site reification), then M2.3/
   M2.5 (cmx building + quoted-cmx marking) for the classic-mode first
   compile.
3. M2.4 (Simplify-time reification via the new Flambda primitive) so
   `-O2`/`-O3` first compiles produce useful approxs.
4. M4 (current-module references) — builds directly on M1/M2 machinery.
5. M3 (modes) in parallel as a typing work stream.

Post-inlining resolution (a second pass over inlined bodies) was
considered and ruled out (2026-07-21).
