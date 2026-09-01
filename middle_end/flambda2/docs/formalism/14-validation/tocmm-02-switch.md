# tocmm-02: switch → Cswitch, untagged scrutinee, range check

Targets `TC.Switch` (ch. [16](../16-to-cmm-control.md)) and `CM.Switch`
(ch. [15](../15-cmm.md)): a multi-arm `match` on an `int` becoming a Cmm
`(switch …)` over an *untagged* scrutinee, with the out-of-range arm guarded.

**Testsuite:** [`testsuite/tests/flambda2/examples/tocmm/switch.ml`](../../../../../testsuite/tests/flambda2/examples/tocmm/switch.ml) (reference: `tocmm/switch.compilers.reference`, capturing `-dcmm -dcanonical-ids`) — run with `make -s test-one TEST=flambda2/examples/tocmm/switch.ml`.

## Source

```ocaml
external ext : int -> unit = "ext_fn"
let[@inline never] dispatch n =
  match n with 0 -> ext 100 | 1 -> ext 200 | 2 -> ext 300 | 3 -> ext 400 | _ -> ext 0
```

(The arms have distinct effects, so Simplify cannot collapse the match to an
affine formula or a data table; a genuine `Cswitch` survives.)

## Prediction (before reading `-dcmm`)

1. The scrutinee `n` is a tagged `int`; `TC.Switch` switches on the *untagged*
   form, so expect an untag `(>>s n 1)` (`asr 1`, `TC.Prim.TagUntag`) feeding the
   switch.
2. The `_ ->` default covers everything outside `{0,1,2,3}`; `to_cmm`'s
   `transl_switch_clambda` builds an index over `[0, max]`, so out-of-range values
   are separated by a range check before the table (`CM.Switch` NOTES) — expect a
   `(<u k n)` unsigned compare selecting the default.
3. The four arms become `(switch … case 0: … case 3: …)` (`CM.Switch`).
4. `ext 100` → `(extcall "ext_fn" 201 …)` (`100` tagged = `201`; `R.Val.Imm`).

## Actual `-dcmm -dcanonical-ids`

```
(function camlSwitch__dispatch_0_1_code (n/0: int) : int
 (if (<u 7 n/0) (extcall "ext_fn" 1 int->val)
   (let switcher/0 (>>s n/0 1)
     (switch switcher/0
     case 0: (extcall "ext_fn" 201 int->val)
     case 1: (extcall "ext_fn" 401 int->val)
     case 2: (extcall "ext_fn" 601 int->val)
     case 3: (extcall "ext_fn" 801 int->val)))) )
```

## Verdict

MATCH.

- `(>>s n 1)` — untag of the scrutinee before switching (`TC.Prim.TagUntag`;
  `TC.Switch` uses the untagged scrutinee).
- `(switch switcher case 0: … case 3: …)` — a genuine `Cswitch` node
  (`CM.Switch`, `transl_switch_clambda`).
- `(<u 7 n)` — the range check separating the `_` default (an unsigned compare on
  the still-tagged `n`; `7 = 2·3+1` is the tagged max arm), matching `CM.Switch`'s
  "values outside `[0,max]` go to the default" note.
- Arm bodies `(extcall "ext_fn" 201 …)` etc. — `100 ↦ 201`, `R.Val.Imm`.

## Diagnosis

No discrepancy. This exercises the general (`n > 2` arms) path of `TC.Switch`
(the `Cswitch`, not the two-arm `C.ite`), including the untagged scrutinee and the
default-range guard. (Small dense `int → value` matches are instead compiled by
Simplify/to_cmm to an affine formula or a data-table load — see tocmm-05 — so a
literal `Cswitch` requires arms with genuinely distinct control, as here.)
