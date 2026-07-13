# new-06: trap actions preserved around an opaque call

Targets `OS.ApplyCont.TrapPush` / `OS.ApplyCont.TrapPop` (ch. 04): a `try…with`
around a call to an unknown function keeps its push/pop trap actions — they must
NOT be optimized away.

## Source

`t06.ml`:

```ocaml
let f g = try g () with _ -> 0
```

`g` is a parameter (an unknown, opaque function), so its call may raise and the
handler is reachable.

Flags: `ocamlopt.opt -O3 -c -drawfexpr-to 06.raw.fl -dfexpr-to 06.final.fl t06.ml`.

## Raw IR (body of `f`)

```
let try_region/36 = %begin_try_region () in
let try_ghost_region/37 = %begin_try_ghost_region () in
cont k/20 push(k/18)
  where k/20 = apply g/34 (0) -> k/19 * k/18
  where k/19 (body_result/41 : imm tagged) = cont k/17 pop(k/18) (body_result/41)
  where k/18 exn (exn/38 : val) =
    let unit/39 = %end_try_region (try_region/36) in
    let unit/40 = %end_try_ghost_region (try_ghost_region/37) in
    cont k/17 (0)
```

## Prediction (before running)

The callee `g` is a parameter of kind `val` with unknown type: the call cannot
be resolved to a direct call, cannot be proven non-raising, and cannot be
inlined. Therefore the trap frame must survive: `S.Rewrite.LetCont.Inline` and
`.DeadHandler` do not apply (the exn handler `k/18` carries trap actions and is
reachable), and the `push(k/18)` / `pop(k/18)` trap actions
(`OS.ApplyCont.TrapPush` / `.TrapPop`) are preserved verbatim around the apply.

Expected final body: structurally identical to the raw — `begin_try_region`,
`cont … push(k_exn)`, `apply g (0) -> k_ret * k_exn`, `pop(k_exn)` on the return
edge, and the exn handler ending the try regions.

## Actual IR (final)

```
let try_region/73 = %begin_try_region () in
let try_ghost_region/74 = %begin_try_ghost_region () in
cont k/41 push(k/39)
  where k/41 = apply g/72 (0) -> k/40 * k/39
  where k/40 (body_result/78 : imm tagged) = cont k/38 pop(k/39) (body_result/78)
  where k/39 exn (exn/75 : val) =
    let unit/76 = %end_try_region (try_region/73) in
    let unit/77 = %end_try_ghost_region (try_ghost_region/74) in
    cont k/38 (0)
```

## Verdict

MATCH. The trap push/pop and both try regions are preserved; the opaque `apply`
is retained. Nothing in the frame was eliminated.

## Diagnosis

No discrepancy. This is the negative control the target rules demand: because
the callee is opaque, none of the trap-eliminating rewrites has its side
condition satisfied, so `OS.ApplyCont.TrapPush`/`.TrapPop` semantics are left
intact. (Had `g` been a known non-raising function, the handler would be dead
and the frame collapsible — that is a separate scenario.)
