# Plan: Standalone value approximations and `%reify_approx`

Status: in progress.  Keep this document updated as decisions are made.

## Goal

1. `Value_approximation.Standalone`: a self-contained ("standalone") mirror of
   `'code Value_approximation.t` that names no hash-consed types, so it can be
   `Marshal`ed and read back in a different process.
2. A new Lambda primitive `%reify_approx : 'a` (not a function type).  In
   classic-mode Flambda 2, `reify_approx foo` compiles to a constant string
   literal: the `Marshal`ed `Standalone.t` of `foo`'s approximation.
3. A function to unmarshal such a string back to `Standalone.t` and then to a
   `'code Value_approximation.t`, using the usual creation functions
   (`Symbol.create`-family, `Code_id.create`, `Function_slot.create`, ...).

## Key facts discovered

- In classic mode `'code` is instantiated as `Code_or_metadata.t`
  (`closure_conversion_aux.ml:157`:
  `type value_approximation = Code_or_metadata.t Value_approximation.t`).
- `classic_mode_types` (dune library `flambda2_classic_mode_types`) does NOT
  depend on `terms` where `Code_or_metadata` lives.  So `Standalone` stays
  polymorphic: `create : 'code Value_approximation.t -> Standalone.t` simply
  drops the code, and the reverse direction takes a callback to supply `'code`.
- **Decision (from Mark)**: we do not save `'code` at all.  In the
  `Closure_approximation` case, when a `symbol` is present the code can be
  looked up from the symbol at demarshal time (cf. how
  `Flambda_cmx.load_symbol_approx` works).  When `symbol = None` we degrade
  (callback returns `None` -> `Unknown Flambda_kind.value`).
- `external reify_approx : 'a = "%reify_approx"` typechecks: arity-0 externals
  are permitted when the primitive name starts with `%` (`typedecl.ml:4760`).
- With `prim_arity = 0`, `translcore`/`translprim` compile an application
  `reify_approx foo` to `Lapply { ap_func = Lprim (Preify_approx, [], _);
  ap_args = [foo] }` (all user args become "extra args" around the 0-ary
  primitive; see `translcore.ml` `cut_args` and `lambda_of_prim`
  `Primitive (prim, arity)` case).

## Design

### `Value_approximation.Standalone` (in classic_mode_types/value_approximation.ml{,i})

**Decision (from Mark, 2026-07-21)**: only the genuinely hash-consed types
need converting (Symbol, Reg_width_const, Code_id, Function_slot).  Ordinary
immutable data structures — `Flambda_kind.t`,
`Flambda_kind.Scannable_block_shape.t`, `Tag.Scannable.t` (an int),
`Alloc_mode.For_types.t`, and `Reg_width_const.Descr.t` (a plain variant of
plain scalar types; verified: `Target_ocaml_int.t`, `Targetint_32_64.t`,
float/vector bit patterns are all plain data) — are kept as-is and marshal
fine.

- `compilation_unit = { pack_prefix : string list; name : string }`
  - decompose: `Compilation_unit.for_pack_prefix` + `Prefix.to_list` +
    `Name.to_string`; `Compilation_unit.name_as_string`.
  - rebuild: `Prefix.parse_for_pack (String.concat "." ...)` (or
    `Prefix.empty` when the list is empty) + `Name.of_string` +
    `Compilation_unit.create`.
  - Limitation: instance arguments of parameterised units are not preserved.
- `symbol = { compilation_unit; linkage_name : string }`
  - rebuild with `Symbol.unsafe_create cu (Linkage_name.of_string ...)`.
    We use `unsafe_create` because `Symbol.create` treats the linkage name as
    a raw name to be prefixed with the compilation unit (see the CR in
    `int_ids.mli`), and we saved the already-mangled linkage name.
- `code_id = { compilation_unit; name : string }` (stamp and debuginfo
  dropped); rebuild with `Code_id.create ~name ~debug:Debuginfo.none cu`
  (fresh stamp — code ids do not round-trip exactly).
- function slot: just `string` (the name; stamp ignored, per spec).  Rebuilt
  with `Function_slot.create <code_id's comp unit> ~name
  ~is_always_immediate:false Flambda_kind.value` (the slot's own compilation
  unit is not saved; we reuse the code id's).
- consts: stored as `Reg_width_const.Descr.t` directly (decompose with
  `Reg_width_const.descr`, rebuild with `Reg_width_const.of_descr`; the
  private `Descr` constructors are no obstacle to marshalling).
- `t` mirrors the five constructors of `Value_approximation.t`, keeping
  `Tag.Scannable.t`, `Scannable_block_shape.t` and `Alloc_mode.For_types.t`
  unchanged in `Block_approximation`.

API:

```ocaml
module Standalone : sig
  type t  (* concrete, all constructors exposed in the mli *)
  val create : 'code Value_approximation.t -> t          (* drops 'code *)
  val to_approximation :
    t ->
    find_code:(code_id:Code_id.t ->
               function_slot:Function_slot.t ->
               symbol:Symbol.t option ->
               'code option) ->
    'code Value_approximation.t
  val to_marshalled_string : t -> string                 (* Marshal.to_string *)
  val of_marshalled_string : string -> t                 (* Marshal.from_string *)
end
```

`find_code` returning `None` yields `Unknown Flambda_kind.value` for that
closure approximation.  The classic-mode instantiation (`'code =
Code_or_metadata.t`) is left to future callers (e.g. looking the code up from
the symbol via the `.cmx` loading machinery); nothing in `classic_mode_types`
mentions `Code_or_metadata`.

### `%reify_approx` primitive

- `lambda/lambda.ml`: new constructor `Preify_approx` in `type primitive`
  (0-ary at the Lambda level; the "argument" only ever appears via the
  `Lapply` wrapping described above, and is rewritten during CPS conversion).
  - `primitive_can_raise`: false; may-allocate: no (result is a statically
    allocated string); `primitive_result_layout`: `Pvalue Pgenval`-style
    (string layout).
- `lambda/translprim.ml`: new `prim` variant `Reify_approx`;
  `lookup_primitive`: `| "%reify_approx" -> Reify_approx`.
  `lambda_of_prim`:
  - native: `Lprim (Preify_approx, [], loc)`
  - bytecode (`not !Clflags.native_code`): a stub function
    `fun _ -> ""` (approximations don't exist in bytecode; the empty string
    will fail `of_marshalled_string`, which is honest).  This keeps
    `Preify_approx` out of the bytecode pipeline entirely.
- `printlambda.ml` and any other exhaustive matches over `primitive`: add
  cases as the compiler errors direct.

### Lambda_to_flambda / Closure_conversion

- `lambda_to_flambda.ml`, `Lapply` case of `cps`: when
  `ap_func = Lprim (Preify_approx, [], _)` rewrite to
  `Lprim (Preify_approx, ap_args, ap_loc)` and recurse.  Anything other than
  exactly one argument is a fatal error ("%reify_approx must be applied to
  exactly one argument").
- `closure_conversion.ml`, `close_primitive`: new case
  `| Preify_approx, [[arg]] ->`
  1. `find_value_approximation_through_symbol acc env arg` — we resolve
     through symbols so that e.g. a top-level block reifies as its block
     approximation rather than just `Value_symbol`.  (Revisit if the raw
     symbol is preferred.)
  2. `Standalone.create` -> `Standalone.to_marshalled_string`.
  3. `register_const0 acc (Static_const.immutable_string s) "reified_approx"`
     -> symbol.
  4. Result: just the string constant.  **Decision (from Mark, 2026-07-21)**:
     no `Opaque_identity foo` binding — `reify_approx foo` is replaced by the
     constant string literal alone.  (The argument, being a [Simple] by this
     point, was still evaluated by the CPS conversion; only its value goes
     unused.)
  - `| Preify_approx, ([] | _)` -> fatal error (bare/partial use, e.g.
    `let f = reify_approx in ...` cannot be supported since the argument's
    approximation must be known at the primitive site).

### Demarshal helper

`Standalone.of_marshalled_string` + `Standalone.to_approximation` compose to
give the required function.  A convenience wrapper instantiated at
`Code_or_metadata.t` can be added where first needed (candidate:
`flambda_cmx.ml` or `closure_conversion_aux.ml`).

## Open questions / notes

- Should reification look *through* symbols (current choice) or preserve
  `Value_symbol`?  Current: through.
- `Levent` wrappers around the `Lapply` (bytecode debug events) are not
  expected on the native path; not handled.
- Instance arguments (parameterised modules) in compilation units are dropped.
- Vec256/512 consts are mirrored but rebuilding uses the corresponding
  `Bit_pattern.of_bits`; no special target checks.
- `Block_approximation` field arrays can be shared/cyclic?  No — they are
  trees; Marshal without `Closures` flag is fine.

## Progress log

- [x] Explored types; recorded key facts above.
- [x] `Standalone` module in value_approximation.ml{,i} (simplified per
      2026-07-21 decision: only hash-consed types are converted).
- [x] `Preify_approx` in lambda.ml + translprim + printlambda + all
      exhaustive matches (slambda_fracture, slambdaeval, tmc,
      value_rec_compiler, blambda_of_lambda, lambda_to_flambda_primitives,
      closure_conversion inner list).  Bytecode: translprim emits a stub
      function returning [""] instead of the primitive.
- [x] CPS rewrite of the `Lapply` form (lambda_to_flambda.ml) +
      `close_primitive` case producing the constant string (no
      Opaque_identity, per 2026-07-21 decision).
- [x] Build (`make -s boot-compiler`), format (`make -s fmt`).
- [x] Test `testsuite/tests/flambda2/reify_approx.ml` (-Oclassic, native):
      constant -> Value_const, toplevel pair -> Block_approximation,
      toplevel function -> Closure_approximation, dynamic -> Unknown; all
      four strings demarshal.  Note: applications of [reify_approx] trigger
      warning 20 (ignored-extra-argument) since its type is a bare variable;
      the test silences it.
- [x] Full `make -s test`: 2356 passed, 0 failed (2026-07-21).
- [ ] Future: convenience demarshal wrapper instantiated at
      [Code_or_metadata.t] (e.g. in flambda_cmx), looking code up from the
      symbol; exercise [Standalone.to_approximation] round-trip.
