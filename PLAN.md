# Layout-polymorphism templates as true closures

This describes the representation of layout-polymorphism (`let poly_`)
templates from slambda evaluation onwards, as implemented on this branch.

## Motivation

Layout polymorphism closure-converts at the lambda level: a `let poly_`
binding becomes a compile-time *template* (specialized per layout
instantiation by slambda evaluation) plus a runtime *environment block*
holding the captured values. Previously the environment was an ordinary
mixed block (`Pmakeblock`), the specialized function took it as a prepended
parameter and destructured it with `Pmixedfield`, and each instantiation
site was a partial application of the (n+1)-ary specialized function to the
environment.

Flambda2 therefore saw an opaque block and a generic partial application,
and could apply none of its closure optimizations: no dead value-slot
elimination, no closure typing/aliasing through projections, no lifting of
environments to statics, no reaper environment unboxing, and calls went
through a currying wrapper.

The environment and the instantiated functions are now presented to
flambda2 as genuine sets of closures.

## The representation

After slambda evaluation, a template gives rise to three kinds of term
(`Slambda_fracture` writes them at template-definition time; evaluation
splices and specializes them):

```
(* definition site: the runtime half of the template value *)
let env = Pset_of_closures { template; layouts; mode = <env mode> }
            (capture_1, ..., capture_n)

(* hoisted to toplevel, once per (unit x template x layout arguments) *)
let name =
  Lcode { code_fun = <function of the original arity whose body starts with
                        let cap_i = Pproject_value_slot { index = i; layout_i }
                                      (Lvar closure_var) in ...>;
          code_closure_var = closure_var;
          code_slots = [layout_1; ...; layout_n];
          code_alloc_mode = <env mode> }

(* each instantiation site *)
Pclose_template { template } (Lvar name, env)
```

### `Lcode`

`Lcode` is the per-unit shared specialized code of a template
instantiation. It is deliberately a distinct lambda constructor, not a
variant of `Lfunction`: it is not an ordinary closure expression — no
closure exists for it by itself — and making it a constructor forces every
lambda pass to handle it explicitly rather than silently treating it as a
normal function (which would materialize an unsound closure with unfilled
slots). It is stage-restricted, like `Lkindtemplate`/`Lsplice`: it only
exists after slambda evaluation, and only as the right-hand side of the
`Llet` binding a hoisted instantiation.

- `code_fun` keeps the original arity; its `mode` is ignored.
- `code_closure_var` binds the function's own closure within the body
  (flambda2's `my_closure`); the body reads capture `i` with
  `Pproject_value_slot { index = i; layout }` applied to it. It is not a
  free variable of the `Lcode`.
- `code_slots` are the capture layouts, in the order of the corresponding
  `Pset_of_closures` arguments.
- `code_alloc_mode` is the allocation mode of the closures built by
  `Pclose_template`: the template environment's mode. Closures hold
  *copies* of the captures, so a local environment bounds them to its
  region; a heap environment permits (and requires, since instantiations
  may escape) heap closures. Note that `Lkindinstantiate.kinst_mode` is
  always `maybe_alloc_stack` and says nothing about escape, so it must not
  be used for this.

The identifier bound to an `Lcode` is not an ordinary variable: it may only
be referenced by `Pclose_template`. In from_lambda it is excluded from
free-variable sets (`Lambda_to_flambda_env.register_lcode_ident`, filtered
in `cps_function`) so that references inside nested functions do not become
value slots, and it is never CPS-converted (the `Pclose_template` argument
is passed through as a raw `IR.Var`).

### The primitives

- `Pset_of_closures { template; layouts; mode }`, applied to the captured
  values, allocates the environment block. In flambda2 it becomes a
  `Set_of_closures` with *no functions*: a single synthetic
  `Function_declarations.Deleted` slot (a null code pointer plus
  closure-info word, two words) satisfies the pervasive invariant that
  every set of closures has a function slot at offset 0 (GC `startenv`,
  `Slot_offsets` layout, and `Project_value_slot`'s function-slot-relative
  addressing). Each capture contributes one value slot per non-void
  unarized component of its layout, so unboxed-product captures are stored
  flat.
- `Pproject_value_slot { index; layout }` reads capture `index` from the
  enclosing `Lcode`'s closure; it expands to one flambda2
  `Project_value_slot` per unarized component.
- `Pclose_template { template }`, applied to `[code; env]`, builds a real
  set of closures pairing the shared code with fresh value slots whose
  contents are flat-copied (projected) out of the environment block, and is
  bound with `Bound_pattern.set_of_closures`.

`Lambda.primitive_may_allocate` reports `Pclose_template` as a
(conservative) local allocation: the true mode lives on the `Lcode`, which
may belong to another compilation unit.

### Slot identity across compilation units (`Template_id`, `Lpoly_slots`)

The environment block is allocated in the unit that evaluates the template
definition, but its slots are projected in any unit that instantiates the
template — the projections live inside the slambda template marshalled into
the `.cmx`. Flambda2 slot identity is (compilation unit, integer stamp), so
every unit must reconstruct *exactly* the same slots.

Identity derives from `Template_id` (extracted from `Slambdaeval` into
`lambda/template_id.ml`): owner unit plus a globally unique stamp. It has
precisely the needed semantics for free: templates re-register per
evaluation, so a template nested inside another gets a fresh id per outer
instantiation (its environment set is then a distinct set per duplication,
avoiding any cross-instantiation slot clash); and template ids marshal
through the cmx template store (`CU_data`). The primitives carry a
`Lambda.template_ref` — `Template_var` (a compile-time binding, resolved by
evaluation to the template's value) before evaluation, `Template_id` after.

`middle_end/flambda2/from_lambda/lpoly_slots.ml` maps a template id to
slots deterministically, in a reserved *negative* stamp space
(`Slot.create_deterministic`; ordinary `Slot.create` stamps are
non-negative, so the two can never collide): the environment's function
slot is `-stamp - 1` and capture `i`'s leaf `j` is
`-cantor(cantor(stamp, i), j) - 1`. Both the defining and instantiating
units compute identical slots from identical concrete layouts.

Only environment-set slots use deterministic stamps. The sets built by
`Pclose_template` are entirely local to the instantiating unit and use
ordinary fresh slots, so they keep full dead-slot elimination.

### Pinning environment slots

An environment slot's only uses may live inside the marshalled template,
invisible to the defining unit's flambda2. Deterministic-stamp value slots
are therefore treated as always-used by every removal path:

- `Name_occurrences.value_slot_is_used_or_imported` (dead-slot removal in
  `Expr_builder` and `Data_flow_graph`);
- both used-slot predicates in `Slot_offsets` (so the slots are assigned
  and exported offsets);
- the cmx import map (`Renaming.Import_map.value_slot_is_used`);
- the *type* export filters (`Type_grammar.remove_unused_value_slots_...`
  and `Set_of_closures_contents.remove_unused_value_slots`) — without
  these, the exported closure types lose the slots and importing units
  simplify the projections to `Invalid`, an abort at runtime;
- the reaper keeps them and refuses (fatal error) to change the
  representation of a set containing them.

### from_lambda

`Llet (name, Lcode ...)` is handled by `Closure_conversion.close_lcode`:
the code is compiled via the ordinary `close_one_function` machinery with
`code_closure_var` mapped to `my_closure` and the per-capture slots
recorded in the environment (`Env.lcode_context`) for translating
`Pproject_value_slot`; only the *code* is emitted (`Acc.add_code`) — no
prototype set is created (a set with unfilled slots would give the code's
projections an `exactly_this_closure` type that simplifies them to
`Invalid`). The binding is registered (`Env.add_lcode_binding`) rather than
bound to a variable.

Each `Pclose_template` looks up the registration, projects every
environment leaf slot out of the environment value (using the
template-derived function slot as `project_from`), and creates a
`Set_of_closures` reusing the registered code id and function slot —
exactly the shape inlining-duplicated sets already have. Flambda2
re-simplifies code per referencing set, so each site gets its own
specialized copy: dead captures are eliminated per site (unused projection
=> slot dropped from that site's set), at some code-size cost relative to
the old shared-body-plus-currying-stub scheme when one instantiation has
many sites.

In the common same-unit case the machinery disappears entirely: the
environment block, the per-site closures, and the calls all inline and
simplify away. Cross-unit, projections resolve directly against the
defining unit's statically-allocated environment symbol via imported
offsets.

### Bytecode

Bytecode shares the same lambda; `Blambda_of_lambda` lowers the constructs
to the legacy behaviour (bytecode boxes all unboxed values, so there are no
value slots):

- `Lcode` compiles as a function with `code_closure_var` prepended as an
  extra first parameter (`Lambda.free_variables` of an `Lcode` is already
  exactly this wrapper's free-variable set);
- `Pset_of_closures` builds a block, one field per capture, with the same
  `Makeblock`/`Make_faux_mixedblock` dispatch as `Pmakeblock` (preserving
  the faux-mixed sentinel that makes polymorphic compare/hash/marshal
  reject environments holding boxed-in-bytecode non-values);
- `Pproject_value_slot` is a field read plus the usual deep copy of
  unboxed-product elements;
- `Pclose_template` is a one-argument application, i.e. the same
  `Kgrab`-based partial application as before.

`Simplif.simplify_local_functions` treats `Lcode` like `Lfunction`
(a function-scope barrier for its body); other lambda passes handle it
conservatively (no motion, no duplication, no arity fusion).

### fexpr

The fexpr language supports function slots whose code is deleted:
`Fexpr.fun_decl.code_id` is optional, printed and parsed as
`closure deleted @slot` (a deleted closure requires an explicit function
slot), so environment sets round-trip through the fexpr printer/parser.

## Testing

`testsuite/tests/layout_poly/fexpr/` contains short programs checked
against their simplified-flambda dumps (`dump-simplify` +
`check-fexpr-dump`): a fully static template; a static template
instantiated in the same unit; a dynamic-capture template that optimizes
away completely; an escaping instantiation whose closure and copied
captures must survive; nested templates; and cross-unit instantiation.
Most templates are `[@inline never]` so the dumps pin the compiled
artifacts (specialized code, per-site sets, direct calls) — this also
checks that function attributes survive templating; `dynamic_eliminated.ml`
deliberately allows inlining to document full elimination.
`testsuite/tests/layout_poly/escaping_instantiation.ml` is a runtime
regression test for the `code_alloc_mode` escape bug.

## Known limitations

- Cross-module static evaluation remains unimplemented in bytecode
  (pre-existing).
- The reaper (off by default) fatals rather than re-representing
  environment sets; teaching its analysis to treat them as escaping would
  lift this.
- Per-site code specialization trades code size for optimization when a
  single (template x layouts) instantiation has very many sites.
- Environment-set slots are pinned even when the defining unit could prove
  them dead everywhere; a finer analysis could walk the marshalled
  templates for the slots that actually escape into the cmx.
