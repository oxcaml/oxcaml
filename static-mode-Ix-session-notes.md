# Static mode / `-Ix` — session context

Working branch: `static-mode-Ix`. Draft PR: oxcaml/oxcaml#6226.

## Goal

A compilation unit can only be used at `@ static` (compile-time evaluation) if
a `.cmx` is guaranteed to be available for it. `-Ix <dir>` marks a directory as
one where `.cmx` files are guaranteed (unlike plain `-I`). So a module loaded
from a `-I` directory must be treated as `dynamic` regardless of what its `.cmi`
records.

## Behavioral change (the core fix)

In `Persistent_env.acknowledge_new_pers_name`, when building the loaded unit's
mode:

- `Visible { cmx_guaranteed = true }` → keep the staticity recorded in the cmi.
- `Visible { cmx_guaranteed = false } | Hidden` → force `Dynamic`, and attach
  the `Cmx_not_guaranteed` hint so the error explains why.

Error message (see `Mode.Report.print_const`, `Cmx_not_guaranteed`):

```
The module is "dynamic"
  because only modules from core libraries and the current library can
  currently be static.
```

There is a `CR-someday zqian` on that message: it is specific to the Jane
Street build setup and should be generalized.

## The `persistent_signature` refactor (enables carrying the hint)

`persistent_signature` now carries the unit's full mode, indexed by allowance:

```ocaml
(* types.ml / types.mli, inside Make_wrapped *)
and 'd persistent_signature = signature * 'd Mode.Value.t
```

Threading of the allowance index ("different `'d` at each site"):

- **cmi on disk** records only `Mode.Staticity.Const.t`.
  - `Cmi_format.cmi_infos` / `cmi_infos_lazy` are decoupled from
    `persistent_signature`: they hold `signature * Mode.Staticity.Const.t`.
  - `Signature_with_global_bindings.t` likewise carries
    `Subst.Lazy.signature * Mode.Staticity.Const.t` (no `'d`).
- **import** (`Persistent_env.pn_sign`, `sig_reader`, `Persistent_env.read`) is
  `Allowance.left_only persistent_signature` — the unit's actual mode (a lower
  bound), where the `Cmx_not_guaranteed` hint lives.
- **save** (`Env.save_signature`, `Persistent_env.make_cmi`) takes
  `Allowance.both persistent_signature`.

## `mode_unit` moved to `Persistent_env`

`mode_unit` ("the mode of a compilation unit": legacy on every axis, with a
given staticity) moved from `Env` to `Persistent_env`. `Env`/`Typemod`/
`Compile_common` call `Persistent_env.mode_unit`. Reason: the unit `Value` must
be built where `cmx_guaranteed` is known (in `Persistent_env`), which sits below
`Env`.

- `Persistent_env.acknowledge_new_pers_name` builds `pn_sign`'s `Value`:
  - cmx guaranteed → `mode_unit ~staticity:(cmi const)` then `disallow_right`;
  - otherwise → `mode_unit_with_staticity` injecting
    `Staticity.of_const ~hint:Cmx_not_guaranteed Dynamic` (legacy axes + the
    hinted staticity, via `Monadic.proj`/`min_with`/`join`).
- `Env.read_sign_of_cmi` now uses the stored `Value` directly as `mda_mode`.
- `Persistent_env.make_cmi` extracts the const via
  `(Mode.Value.to_const_exn mode).staticity`.

## Decisions made this session

- **mli-less `.ml` stays `Dynamic`.** The no-`.mli` save keeps the hardcoded
  `Staticity.Dynamic`; we did *not* switch it to the inferred structure mode's
  staticity (a structure is always static-capable, so its floor would be
  `Static`, which is not what we want for the cmi).
- **`Env.read_signature` returns `signature * Mode.Staticity.Const.t`** (not the
  full `Value`), so consumers in `Typemod` get the const directly.
- **`to_const_exn` added to the axis `Common` signature** (`mode_intf.mli`); the
  implementation already existed in the generic functors.

## Tests

- `testsuite/tests/dash-Ix/staticity.ml` — a static module `S` (file-level
  `@@ static`) can be rebound at `@ static` when loaded via `-Ix`, but not via
  plain `-I` (forced dynamic; reference shows the hint message).
- `testsuite/tests/layout_poly/use_mli_top_modality.ml` — loads its deps via
  `-Ix .` so its static rebinds keep working (cwd is `cmx_guaranteed = false`).

`make -s boot-compiler`, `make -s fmt`, and `make -s test` (6794 passed, 0
failed) are all green.

## Possible follow-ups

- Generalize the `Cmx_not_guaranteed` message (the `CR-someday zqian`).
- `CR-soon zqian`: all persistent modules should always be `Static`, at which
  point `mode_unit`'s `staticity` parameter can be removed.
