# Optimize lpoly closures: present closure-converted templates as true closures to flambda2

## IMPLEMENTATION STATUS (2026-08-19): landed on working tree, 13/13 layout_poly tests pass

Deviations from the plan below, as implemented:

- **No SLminttoken/Lpoly_token**: slot identity derives from `Template_id`
  (extracted from slambdaeval into `lambda/template_id.ml`), which already has
  owner+stamp global uniqueness, per-duplication freshness (templates re-register
  per evaluation), and cross-module marshalling via CU_data. Prims carry
  `Lambda.template_ref = Template_id of Template_id.t | Template_var of
  Slambdaident.t` (the latter resolved by eval from the slambda env, expecting
  an `SLVclosure`). `lkindtemplate` gained `ktmpl_name` for symbol naming.
- **Prim payloads**: `Pset_of_closures { template; layouts; mode }` (args =
  captures); `Pclose_template { template; mode }` (args = [Lcode var; env]);
  `Pproject_value_slot { index; layout }` (index into the enclosing Lcode's
  code_slots; project from the Lcode's closure var). No own-token family:
  site sets use ordinary `Value_slot.create` slots (full dead-slot elim);
  only env-set slots are deterministic (negative stamps via
  `Slot.create_deterministic`, derivation in
  `middle_end/flambda2/from_lambda/lpoly_slots.ml`: fn slot = -stamp-1,
  value slot leaf = -cantor(cantor(stamp,capture),leaf)-1).
- **Lcode = { code_fun; code_closure_var; code_slots : layout list }**.
  Handled only as `Llet(_, id, Lcode ...)`: `CC.close_lcode` emits code only
  (via `close_one_function ?lcode`), registers id in the CC env
  (`Env.add_lcode_binding`), binds no variable. Lcode idents are also tracked
  in the lambda-level env (`Lambda_to_flambda_env.register_lcode_ident`) and
  filtered from `free_idents_of_body` in `cps_function` (they'd otherwise
  become value slots of enclosing functions); the set is propagated into the
  fresh env created per function.
- **Pclose_template arg0 must not be CPS-converted**: special case in
  lambda_to_flambda's Llet-of-Lprim path passes it through as `IR.Var`.
- **Pinning env slots** (deterministic stamps treated as always-used) landed in:
  name_occurrences.ml (`value_slot_is_used_or_imported`), slot_offsets.ml
  (both used-predicates), renaming.ml (`Import_map.value_slot_is_used`),
  type_grammar.ml (`remove_unused_..._value_slot_indexed_product` — needed or
  the exported *types* lose the slots and importing units simplify projections
  to Invalid), set_of_closures_contents.ml (`remove_unused_value_slots`),
  reaper/rebuild.ml (keep + fatal on representation change).
- **Per-site code specialization**: flambda2 re-simplifies code per referencing
  set, so each Pclose_template site gets specialized code (enables per-site
  dead-capture elimination; code-size tradeoff vs the old shared body +
  per-site curry stubs). Same-unit case verified: entire template machinery
  optimizes away completely.
- objinfo references promoted for cross_module_static.

## Context

Layout polymorphism (`let poly_`) closure-converts at the lambda level.
`slambda_fracture.ml:334-431` turns each `Lkindtemplate` into (a) a runtime
environment block built with `Pmakeblock` over the captured values and (b) a
compile-time slambda template whose body is the original `lfunction` with a
prepended env parameter destructured via `Pmixedfield`. After the
one-instantiation-per-cu merge (78ebd1bf43), each unique (template x layout
args) is evaluated once per compilation unit, hoisted to a toplevel
`Llet name = <specialized (n+1)-ary Lfunction>` (slambdaeval.ml:315-350,
878-883, name mangled from template id + layout args), and each
`Lkindinstantiate` site becomes a runtime partial application
`Lapply (Lvar name) [env]`.

Flambda2 therefore sees an opaque block plus a partial application and cannot
apply its closure optimizations: dead value-slot elimination, closure
typing/aliasing through projections, lifting, reaper environment unboxing.
This change makes flambda2 see real closures.

## Confirmed design decisions (user)

1. **`Pset_of_closures`** builds the env block; flambda2-side it becomes a
   `Set_of_closures` with one synthetic `Function_declarations.Deleted`
   function slot (null code ptr + closure-info word), so all existing
   slot_offsets/GC/to_cmm invariants (>=1 function slot at offset 0) hold
   unchanged. (flambda2 hard-requires this: slot_offsets.ml:153-165, 258-264;
   to_cmm_set_of_closures.ml:643-659, 690, 783; Project_value_slot addresses
   fields relative to a function slot offset, to_cmm_primitive.ml:1254-1267.)
2. **Collapsed shape, no env parameter**: the specialized function keeps its
   original arity; its body refers to captures through a **named closure
   variable** (maps to flambda's `my_closure`) via `Pproject_value_slot`. The
   closure name + slot descriptor live on a **new distinct lambda constructor
   `Lcode`** (user decision 2026-08-19), not a field on `lfunction`:
   the form is stage-restricted (post-eval, native, only as the hoisted
   instantiation's `Llet` RHS) and exhaustive matching forces every pass to
   handle it explicitly instead of silently treating it as an ordinary
   closure. Each instantiation site builds a real closure
   pairing the shared specialized code with value slots **flat-copied from the
   env block** (projections of env's slots), allocated at that site's
   `kinst_mode`. Explicitly does not rely on flambda2 inlining.
3. **Slot identity via stable lambda-level tokens**, minted **during slambda
   eval** (not fracture — eval duplicates template bodies without freshening,
   `eval_lam` is a plain `Lambda.map`, slambdaeval.ml:457): fracture emits
   `SLlet tok = SLminttoken` binders and prims carry `Tok_ref of
   Slambdaident.t`; eval resolves them to concrete tokens (new `SLVtoken`
   value) exactly like `Psplicevar -> layout`. Tokens travel cross-module
   inside the marshalled template store (`CU_data`, slambdaeval.ml:274-291).
   from_lambda reconstructs *identical* `Value_slot.t`/`Function_slot.t`
   values from tokens on every side (slot identity = (compilation_unit,
   name_stamp), identifiers/slot.ml:49-121), using a segregated stamp space
   (negative stamps) so they can't collide with flambda2's own `next_stamp`
   counter within the defining unit.
4. Captured values may have unboxed-product / splice-var layouts
   (typing/typeopt.ml:1272), so **unarization is required**: one token per
   capture; from_lambda expands each capture into leaf `Value_slot`s (one per
   non-void unarized component via `Flambda_arity.Component_for_creation.
   from_lambda`), with leaf identity derived deterministically from
   (token stamp, leaf index). Layouts are concrete post-eval, so both units
   compute the same expansion.

## Target lambda shapes (post-eval, native)

```
(* def site, defining unit — runtime half of the template value *)
let env = Pset_of_closures { env_slot_tokens; capture_layouts; mode = ktmpl_env_mode }
            (capture_1, ..., capture_n)

(* toplevel per (unit x template x layouts), hoisted by Ctx.instantiate *)
let name = Lcode
             { code_fun = { params = original params;   (* original arity *)
                             body =
                               let cap_i = Pproject_value_slot { token = own_tok_i; ... } (Lvar c) in
                               ... original body ... };
               code_closure_var = c;
               code_env_function_slot = env_function_slot_token;
               code_slots = [(own_tok_i, env_tok_i, layout_i); ...] }

(* each instantiation site *)
Pclose_template (Lvar name, env)     (* second prim form: build a set of closures
   reusing name's code, with own_tok_i := Pproject_value_slot(env_tok_i) of env,
   allocated at kinst_mode *)
```

Key points:
- **Two token families.** `env_tok_i` (minted once per def-site evaluation,
  via `SLlet`s around the template value) name the env set's slots;
  `own_tok_i` (minted by `SLminttoken` *inside* the template body, hence once
  per cached instantiation per unit) name the site closures' slots. Decoupling
  them means the env set (Deleted slot, size 2) and site sets (real function
  slot, size 2 or 3 by arity) never share slots, so no cross-set offset
  constraints. Nested templates work automatically: an inner def site's
  `SLminttoken`s re-evaluate per outer instantiation, giving fresh identities
  per duplication.
- All site sets in a unit share one code id and one function slot
  (from the descriptor's tokens/own function-slot minted with the
  instantiation), exactly like inlining-duplicated sets.
- Bytecode stays on the **old fracture output** (env parameter + Pmixedfield +
  partial application) via a `~target:(Native|Bytecode)` parameter.

## Implementation stages (each keeps the tree compiling)

### Stage 0 — feasibility spike (throwaway)
Verify flambda2 accepts, from closure conversion: (a) a code definition with
no accompanying set (`Acc.add_code`, cf. closure_conversion.ml:2575), whose
body projects value slots from `my_closure` that no set yet supplies; (b) two
`Set_of_closures` sharing that code id + function slot with the slots
populated. Check simplify (how/when the code is simplified without a defining
set; simplify_set_of_closures.ml), invariant checks, slot_offsets, to_cmm.
**Critical**: never materialize a slot-less "prototype" set for the toplevel
`Llet name = Lcode ...` — its `exactly_this_closure` type would make the
code's projections simplify to Invalid. The Llet must bind a dummy and
register (code_id, descriptor) in the conversion env instead.
If code-sharing is fundamentally rejected, stop and revisit shape with user.

### Stage 1 — token module + primitive/slambda skeletons
- New `lambda/lpoly_token.ml{,i}`: `{ cu : Compilation_unit.t; stamp : int }`,
  `mint : unit -> t` (dedicated counter, `Compilation_unit.get_current_exn`),
  print/equal/hash. Pattern: lambda/static_label.ml.
- lambda.mli:153 / lambda.ml:197 constructors:
  - `Pset_of_closures of { tokens : slot_token list; capture_layouts : layout list; mode : locality_mode }` (args = captures)
  - `Pclose_template of { mode : locality_mode }` (args = [function value; env value]; descriptor read off the function)
  - `Pproject_value_slot of { token : slot_token; capture_layouts : layout list; index : int }` (unary; result layout = nth layout; carries the layout list so leaf base offsets are computable, like Pmixedfield carries its shape)
  - `slot_token = Tok of Lpoly_token.t | Tok_ref of Slambdaident.t` (Tok_ref only pre-eval)
  - slambda: `SLminttoken`; slambdaeval `Types.value`: `SLVtoken of Lpoly_token.t`.
- Exhaustive-match arms (reference commit c5faf94dfc):
  lambda.ml `primitive_may_allocate` :2763 (`Some mode` / `Some mode` / `None`),
  `primitive_can_raise` :2942 (false), `primitive_result_layout` :3408
  (layout_block / Pvalue Pgenval / nth capture_layout);
  printlambda.ml :407 + :981 + slambda printer; tmc.ml :865;
  translprim.ml `lambda_primitive_needs_event_after` :2541 (false);
  value_rec_compiler.ml `compute_static_size` :176+ (fatal — poly is already
  rejected under letrec, value_rec_compiler.ml:250);
  slambda_fracture.ml `fracture_prim` :475 (fatal: cannot appear in tlambda);
  bytecomp/blambda_of_lambda.ml :397 (fatal: bytecode uses old path);
  closure_conversion.ml :1276 assert list; lambda_to_flambda_primitives.ml
  :1868; lambda_to_lambda_transforms passthrough.
- slambdaeval: `eval_prim` (:674, has env) resolves `Tok_ref` -> `Tok` and
  substitutes capture_layouts; `SLminttoken` eval mints; `SLVtoken` in
  value type/printer/Mangling (fatal in `symbol_arg_of_value`);
  `assert_primitive_contains_no_splices` :809 walks capture_layouts and
  rejects residual `Tok_ref`.

### Stage 2 — deterministic slots in flambda2
identifiers/slot.ml{,i} (functor `Slot.Make`, both Value_slot and
Function_slot include it): `create_deterministic ... ~stamp:int` (assert
negative) and `has_deterministic_stamp` (stamp < 0). Leaf slot stamp
derivation: injective encoding of (token stamp, leaf index) into negatives
(e.g. -(cantor_pair token_stamp leaf_idx) - 1); function-slot token stamp
`-(stamp)-1` in the Function_slot namespace (separate module, no collision).

### Stage 3 — `Lcode` constructor (user decision: distinct
constructor, NOT a field on lfunction)
New lambda constructor, stage-restricted like `Lkindtemplate`/`Lsplice`
("should only exist after slambda eval, native only"; fatal elsewhere via
`Lambda.fatal_error_invalid_constructor`):

```ocaml
| Lcode of
    { code_fun : lfunction;
      code_closure_var : Ident.t;              (* body projects from this *)
      code_env_function_slot : slot_token;
      code_slots : (slot_token * slot_token * layout) list } (* own, env, layout *)
```

Rationale: the form is not an ordinary closure expression (its value only
exists via per-site `Pclose_template`), and exhaustive matching forces every
lambda pass to decide explicitly instead of silently treating it as a normal
`Lfunction` (which in from_lambda's generic arm would materialize the unsound
prototype set). The private `lfunction` record and all its consumers stay
untouched.

Match arms to add (mostly one-line fatal/delegate; the compile errors are the
checklist): lambda.ml `free_variables` (subtract params + code_closure_var),
`make_key`, `shallow_iter`, `shallow_map`, `subst`/rename (freshen
code_closure_var, leave tokens), transl passes with exhaustive lambda
matches, simplif traversals (conservative: opaque, don't move/dup),
printlambda; blambda_of_lambda (fatal — bytecode uses the old path);
slambdaeval `eval_lam_shallow` (substitute layouts + resolve Tok_refs in
descriptor and embedded lfunction); from_lambda top-level dispatch (handled
only as `Llet (name, Lcode ...)`, stage 4).

### Stage 4 — from_lambda translation
- New helper (e.g. from_lambda/lpoly_slots.ml): token + concrete layouts ->
  Deleted `Function_slot.t` + per-capture leaf `Value_slot.t`/kind lists
  (unarization via `Flambda_arity`; voids contribute nothing; kinds via
  `Flambda_kind.With_subkind`).
- `Pset_of_closures` cannot go through convert_lprim (`Set_of_closures` is a
  `Named.t`, not a primitive; Singleton binding rejected, flambda.ml:1161).
  Add an arm in `close_primitive` (closure_conversion.ml:1192, before the
  :1345 fallthrough) building
  `Set_of_closures.create ~value_slots (Function_declarations.create <Deleted slot, size 2>)`,
  zipping leaf slots with the already-unarized args (`Simple.t list list`),
  kind-checking (mirror :3200-3217), calling
  `Acc.add_set_of_closures_offsets ~is_phantom:false` (:3236), alloc mode via
  `Alloc_mode.For_allocations.from_lambda`. Extend `close_let`'s continuation
  (:1495-1521) with a `Set_of_closures` arm using
  `Bound_pattern.set_of_closures [var]` (mirror close_let_rec :3354-3373).
- `Pproject_value_slot` goes through convert_lprim: one
  `Unary (Project_value_slot { project_from; value_slot = leaf_j }, arg)` per
  leaf, wrapped by `H.maybe_create_unboxed_product` (pattern:
  Punboxed_product_field, lambda_to_flambda_primitives.ml:1950-1975). The
  let-of-prim path (lambda_to_flambda.ml:599-679) already binds unarized ids.
  `project_from`: for projections on the named closure var, the enclosing
  function's own slot; on other values (env), the token-derived slot.
- Named-closure-var wiring: `Llet (name, Lcode t)` is the only
  accepted position; cps_function-equivalent handling records the descriptor
  on `Function_decl` (closure_conversion_aux); `close_one_function` (:2577)
  maps code_closure_var -> my_closure (Env additions around :2664-2710) and
  compiles the code against the descriptor's deterministic function slot and
  own-token-derived slots. Note closure_conversion_aux.ml:1140-1150 already
  special-cases Project_value_slot-on-my_closure for tail-recursion analysis.
- Toplevel `Llet name = Lcode t`: emit code only (Acc.add_code),
  register `name -> (code_id, function_slot, descriptor)` in the conversion
  env, bind name to a dummy; fatal on uses of name outside `Pclose_template`,
  and fatal on `Lcode` in any other position.
- `Pclose_template (Lvar name, env)`: look up name's registration; emit
  `Project_value_slot { project_from = d.env_function_slot; value_slot = env_leaf }`
  of env for every leaf; build `Set_of_closures` with the shared code
  id/function slot and own-token slots; `Bound_pattern.set_of_closures`;
  offsets registration; alloc mode from the prim's mode + regions.
- Cross-unit offsets: projections of an imported unit's env slots resolve via
  `EO.imported_offsets` like normal cross-unit inlining. For pass-through
  chains (M3 instantiates M1's template via M2 without touching M1's flambda
  info), force-load the token's owner unit's flambda cmx when converting a
  foreign `Tok` (thread the cmx_loader hook into closure conversion).

### Stage 5 — fracture + eval rewrite (native only)
- `Slambda.eval` / `Slambda_fracture.fracture` gain `~target`. Call sites:
  driver/compile.ml:53 Bytecode, toplevel/byte/topeval.ml Bytecode,
  optcomp/optcompile.ml:99 Native, driver/jscompile.ml:50 Native (flambda2 +
  to_jsir; to_jsir handles Deleted, to_jsir_set_of_closures.ml:58),
  toplevel/native/opttoploop.ml Native, otherlibs/eval Native (verify).
- Lkindtemplate (slambda_fracture.ml:334-431), native: mint env tokens via
  `SLlet t_i = SLminttoken` around the template value; runtime half =
  `Pset_of_closures { Tok_ref t_i; capture_layouts; ktmpl_env_mode }` over the
  captures (replaces Pmakeblock :417-421). Template body: `SLlet own_i =
  SLminttoken` + `Lcode { code_fun; code_closure_var;
  code_env_function_slot = Tok_ref env_fs; code_slots = (Tok_ref own_i,
  Tok_ref t_i, layout_i) list }` where code_fun's body is prefixed with
  `Llet (Alias, layout_i, cap_i, Pproject_value_slot {Tok_ref own_i; index=i}
  (Lvar code_closure_var))` (replaces closure_param + Pmixedfield :353-401).
  Function mode: alloc_heap (code shared; closed), original kind unchanged
  (no extra param). Bytecode: old code verbatim.
- Lkindinstantiate (:432-461), native: runtime =
  `Lprim (Pclose_template { mode = kinst_mode }, [Lsplice app; fun_r])`
  (replaces the partial Lapply). `Lsplice app` evaluates to `Lvar name`
  (slambdaeval.ml:350). Bytecode: old Lapply verbatim.
- `Ctx.instantiate` unchanged (env is never an SLinstantiate argument —
  mangling only accepts layouts, slambdaeval.ml:267-271, and the cached body
  is shared across sites with different envs).

### Stage 6 — pinning env slots against invisible uses
The env set's only projections may live inside the marshalled template
(invisible to the defining unit's flambda2), so its slots must not be
dead-slot-eliminated there: OR `Value_slot.has_deterministic_stamp`-and-env
into `Name_occurrences.value_slot_is_used_or_imported`
(nominal/name_occurrences.ml:872), expr_builder.ml:300-328
(remove_unused_value_slots), reaper filtering (reaper/rebuild.ml:436-444) and
guard reaper env-unboxing for such sets. **Only the env family** is pinned:
distinguish families (e.g. stamp parity or a bit in the token) so site sets'
own-token slots keep full dead-slot elimination — that is the headline win
(unused capture => dead projection => slot dropped from every site set).

### Stage 7 — tests and promotion
Existing: `make -s test-one-no-rebuild DIR=layout_poly` (cross_module_static
objinfo references will change — inspect then promote), transitive_deps,
ordering. New tests: dead capture eliminated from site sets; nested templates
instantiated at >=2 layouts (per-duplication tokens); local env and local
instantiation; cross-module via pass-through module (stage 4 cmx loading);
unboxed-product and sub-word (#int8/#float32) captures; same programs under
bytecode; `-Oclassic` run.

## Verification

- `make -s boot-compiler` after every stage; `make -s fmt` before finishing.
- `make -s test-one-no-rebuild DIR=layout_poly`; promote references only after
  manually inspecting diffs; then full `make -s test`.
- Inspect `-dslambda`/`-dlambda` (tokens, Pset_of_closures env, descriptor,
  Pclose_template) and `-dflambda`/`-drawflambda`: env block is a
  Set_of_closures with a Deleted slot; site sets share one code id; in the
  dead-capture test the site sets have one fewer value slot and no dead
  projection; check a flambda-invariants build.
- Cross-module: compile lib + user, objinfo both, confirm imported offsets
  resolve (no Slot_offsets fatal) and the program runs.

## Risks / open items

- **Stage 0 spike is load-bearing**: code shared by multiple
  closure-conversion-produced sets, and code emitted with no defining set,
  may hit simplify assumptions (which set drives code simplification). If
  rejected, the shape must be revisited with the user.
- Lambda passes between eval and from_lambda (simplif) must treat
  `Lcode` as opaque (don't move/duplicate/eta); the distinct
  constructor makes each pass's choice explicit, but the conservative arms
  still need writing carefully.
- Pass-through cmx force-loading hook location not pinned down.
- Confirm pinned-but-locally-unused env sets still get offsets into
  `Exported_offsets` at finalize.
- One-instantiation-per-cu emits identically-named toplevel definitions in
  every instantiating unit; token freshness per unit keeps each copy
  self-consistent, but confirm nothing assumes cross-unit identity of the
  copies' slots.
- otherlibs/eval and native toplevel lightly exercised; verify their eval
  target choice.
