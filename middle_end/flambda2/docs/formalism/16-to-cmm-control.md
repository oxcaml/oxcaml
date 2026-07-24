# to_cmm, Stage 1: control-flow translation

*Part of the Flambda 2 formalism; see [README.md](README.md).*

This chapter formalizes the **control-flow** half of the `to_cmm` translation:
how Flambda's continuations, trap actions, switches, applications, and `Invalid`
become the Cmm control constructs of [`15`](15-cmm.md). It is *target-independent*
— none of these rules touch word size, endianness, or memory layout; the
representation relation `≈` ([`17`](17-representation.md)) and the data/primitive
lowering ([`18`](18-to-cmm-data.md)) are Stage 2 and are where representation
bugs live. Control is the structural scaffold Stage 2 hangs on.

The translation is a single recursive pass, `to_cmm_expr.ml#expr`, over a
well-formed *optimized* Flambda unit (the output of Simplify), carrying a
translation environment. Variable/simple translation and the delayed-binding
("let-substitution") machinery are owned by [`18`](18-to-cmm-data.md); here they
appear only as the judgment `Θ ⊢ s ⤳ᵥ ce` and as *flush* side conditions.

## 1. The translation judgment and environment

```
Θ ⊢ e ⤳ e_c            -- Flambda expr e translates to Cmm expr e_c under Θ
Θ ⊢ s ⤳ᵥ ce            -- simple s translates to a Cmm value-expr (owned by [18])
```

```
Θ = ⟨V, Φ, D⟩
  V : (Variable ⊎ Symbol) ⇀ (Cmm value-expr)     -- simple translation ([18])
  Φ : Continuation ⇀ realization                 -- how each continuation is realized
  D : delayed-binding state                       -- let-substitution ([18])

realization ::= Return ⟨param_types⟩              -- the current function's return cont
              | Jump ⟨param_types, lbl⟩            -- a Ccatch handler reached by Cexit lbl
              | Inline ⟨params, e_h, occ⟩          -- inlined at the (unique) use site
              | Exn ⟨lbl⟩                          -- an exception handler (Ctrywith label)
```

`Φ` mirrors `To_cmm_env`'s continuation table (`to_cmm_env.mli`; the
`Return`/`Jump`/`Inline` cases returned by `get_continuation`, plus the
exn-handler set queried by `is_exn_handler`). A continuation is realized exactly
one of these four ways, decided when its binder is translated (§3). The
declaration is recorded here; [`18`](18-to-cmm-data.md) owns `V` and `D`.

```rule
RULE TC.Expr.Dispatch
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr
CAVEAT disclosure: model narrows code's quadruple return (Cmm expr, free vars, symbol inits, result) to the Cmm expr alone, treating the other components as semantically irrelevant.
---
Θ ⊢ e ⤳ e_c dispatches on Expr.descr e:
  Let → §TC.Let* (data: [18]) ;  Let_cont → §3 ;  Apply → §5 ;
  Apply_cont → §4 ;  Switch → §6 ;  Invalid → §7
--------------------------------------------------
The translation is structural on the Flambda expression grammar (02-syntax.md).
NOTES: `to_cmm_expr.ml#expr` returns a quadruple (Cmm expr, free vars, symbol
inits, result); only the Cmm expr is relevant to the semantics, so `⤳` records
just it. `Let` of a primitive/simple/set-of-closures/static is data and lives in
[18]; the control forms are below.
```

## 2. Continuation realization: choosing the Cmm form

Non-recursive continuations are classified by
`to_cmm_effects.ml#classify_continuation_handler` into `May_inline` (realized
`Inline`) or `Regular` (realized `Jump`, or `Exn` if it is an exception
handler). Recursive groups are always `Jump` (a recursive `Ccatch`).

```rule
RULE TC.LetCont.Classify
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont
CODE middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_continuation_handler
---
For a Non_recursive Let_cont binding k with handler h, num_free_occurrences and
is_applied_with_traps:
  classify_continuation_handler k h … = May_inline   ⟹  k realized Inline (TC.LetCont.Inline)
  classify_continuation_handler k h … = Regular, h not is_exn_handler
                                                     ⟹  k realized Jump   (TC.LetCont.Jump)
  classify_continuation_handler k h … = Regular, h is_exn_handler
                                                     ⟹  k realized Exn    (TC.LetCont.Exn)
A Recursive group is realized Jump (recursive Ccatch, TC.LetCont.Rec).
--------------------------------------------------
Θ's Φ is extended accordingly before translating the sub-expressions.
NOTES: `May_inline` holds for a non-recursive, non-exn, non-cold handler used
exactly once and not applied with traps (classify_continuation_handler; this is
the source-verified claim of 13 §4.6). Everything else is a real Cmm catch.
```

## 3. Translating continuation binders (`Let_cont`)

```rule
RULE TC.LetCont.Inline
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_inlined
CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#add_inline_cont
---
e = Let_cont (Non_recursive { handler = h over k, body = e_body });  k realized Inline
Continuation_handler.pattern_match h = (params x̄, handler body e_h)
Θ′ = Θ with Φ[k ↦ Inline ⟨x̄, e_h, occ⟩]
Θ′ ⊢ e_body ⤳ e_c
--------------------------------------------------
Θ ⊢ e ⤳ e_c
NOTES: NO Cmm is emitted for the binder; the handler is recorded and spliced in
at k's unique use site (TC.ApplyCont.Inline / the Apply return-cont Inline case,
§4/§5). Emits nothing itself — this is the single-use-continuation inlining that
makes to_cmm output sequential code instead of a Ccatch (to_cmm.md; 13 §4.6).
```

```rule
RULE TC.LetCont.Jump
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_not_inlined
CODE middle_end/flambda2/to_cmm/to_cmm_env.ml#add_jump_cont
---
e = Let_cont (Non_recursive { handler = h over k, body = e_body });  k realized Jump
Continuation_handler.pattern_match h = (params x̄, handler body e_h),  not is_exn_handler
lbl fresh;  Θ_j = Θ with Φ[k ↦ Jump ⟨param_types x̄, lbl⟩]
flush(Θ) at a Branching_point  (delayed bindings materialized; [18])
Θ_j ⊢ e_body ⤳ e_body_c ;  Θ_j ⊢ e_h ⤳ e_h_c   (x̄ bound to fresh Cmm params)
--------------------------------------------------
Θ ⊢ e ⤳ Ccatch(Normal, [⟨lbl, x̄_c, e_h_c, is_cold⟩], e_body_c)
NOTES: A non-inlined ordinary continuation becomes a non-recursive Ccatch
(create_ccatch ~rec_flag:false). The Branching_point flush (let_cont_not_inlined)
prevents a delayed binding from being duplicated into both body and handler.
Steps to CM.Catch.NonRec / CM.Exit. `is_cold` carries the handler's coldness.
```

```rule
RULE TC.LetCont.Exn
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_exn_handler
CODE backend/cmm_helpers.ml#trywith
---
e = Let_cont (Non_recursive { handler = h over k, body = e_body });  k realized Exn
Continuation_handler.pattern_match h = (params (x_exn :: x̄_extra), handler body e_h),
  is_exn_handler
lbl fresh;  Θ_x = Θ with Φ[k ↦ Exn ⟨lbl⟩]
Θ_x ⊢ e_body ⤳ e_body_c ;  Θ ⊢ e_h ⤳ e_h_c
--------------------------------------------------
Θ ⊢ e ⤳ Ccatch(Exn_handler, [⟨lbl, x_exn :: x̄_extra, e_h_c, is_cold⟩], e_body_c)   -- via trywith
NOTES: An exception-handler continuation becomes a `Ccatch` with flag `Exn_handler`,
built by the `trywith` smart constructor (let_cont_exn_handler) — there is NO distinct
`Ctrywith` machine node (CM.Catch.Exn). The first parameter is the exception value,
the rest are extra args; at raise sites they are passed as the remaining `Craise`
operands and threaded through distinguished per-handler registers by cfg_selectgen
(env_find_regs_for_exception_extra_args), not mutable slots. Steps to
CM.Catch.Exn / CM.Raise. The trap-stack PUSH is not part of this catch: it is a
`Push lbl` trap action on the `Cexit` entering the try body (translated from the
Flambda Push trap action; TC.ApplyCont.Jump), with a matching `Pop` on the normal exit.
```

```rule
RULE TC.LetCont.Rec
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#let_cont_rec
CAVEAT disclosure: invariant_params branch is code-accurate but rarely exercised — Simplify/loopify carry loop invariants as free variables/aliases, so the invariant prefix is usually empty.
---
e = Let_cont (Recursive handlers);  no handler is_exn_handler (checked)
pattern_match handlers = (invariant_params z̄, body e_body, (kᵢ ↦ hᵢ)ᵢ)
lblᵢ fresh;  Θ_r = Θ with Φ[kᵢ ↦ Jump ⟨(z̄ ++ x̄ᵢ) types, lblᵢ⟩]ᵢ
flush(Θ) at Entering_loop
Θ_r ⊢ e_body ⤳ e_body_c ;  Θ_r ⊢ e_iⁱ ⤳ e_i_cⁱ   (invariant_vars ++ x̄ᵢ bound)
--------------------------------------------------
Θ ⊢ e ⤳ Ccatch(Recursive, [⟨lblᵢ, z̄_c ++ x̄ᵢ_c, e_i_cⁱ, false⟩]ᵢ, e_body_c)
NOTES: A recursive continuation group is a recursive Ccatch (create_ccatch
~rec_flag:true). The Entering_loop flush (let_cont_rec) is essential: it forbids
inlining a binding into the loop body, which would move a computation inside the
loop and change how often it runs. Recursive groups cannot contain exn handlers
(checked; contains_exn_handler). invariant_params are a leading prefix of each
handler's params, matching OS.LetCont.Rec. Steps to CM.Catch.Rec. Caveat: in
practice Simplify/loopify carry loop-invariant values as free variables / aliases
rather than as `invariant_params`, so the `invariant_vars ++ x̄ᵢ` prefix
(let_cont_rec) is code-accurate but rarely exercised — the invariant prefix is
usually empty.
```

## 4. Translating continuation calls (`Apply_cont`)

Dispatch mirrors `to_cmm_expr.ml#apply_cont`: exn handler → raise; else on `Φ(k)`.

```rule
RULE TC.ApplyCont.Jump
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_continuation
---
e = Apply_cont k (s̄);  Φ(k) = Jump ⟨param_types, lbl⟩
trap actions translated:  None ↦ [] ;  Some (Push {exn_handler = k_h}) ↦ [Push lbl_h] ;
  Some (Pop {exn_handler = k_h; …}) ↦ [Pop lbl_h]   where lbl_h is k_h's Cmm label
Θ ⊢ s̄ ⤳ᵥ v̄   (skipped args removed; [18])
flush(Θ) Flush_everything
--------------------------------------------------
Θ ⊢ e ⤳ Cexit(lbl, v̄, translated-trap-actions)
NOTES: A jump becomes `Cexit` carrying the translated trap actions (Push/Pop map
one-to-one; get_cmm_continuation resolves k_h to its Cmm label). This is the Cmm
image of OS.ApplyCont / OS.ApplyCont.TrapPush / TrapPop; steps to CM.Exit /
CM.Exit.Trap. `remove_skipped_args` drops arguments whose parameter was proven
unused (unarized-void / skipped params).
```

```rule
RULE TC.ApplyCont.Return
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_jump_to_return_continuation
CODE backend/cmm_helpers.ml#trap_return
---
e = Apply_cont k (s̄);  Φ(k) = Return ⟨param_types⟩
Θ ⊢ s̄ ⤳ᵥ v̄ ;  flush(Θ) Flush_everything
trap action None:                     e_c = make_tuple v̄
trap action Some (Pop {exn_handler = k_h}):  e_c = trap_return (make_tuple v̄) [Pop lbl_h]
trap action Some (Push _):            ill-formed (fatal)
--------------------------------------------------
Θ ⊢ e ⤳ e_c        -- e_c becomes the value/return of the current Cmm function body
NOTES: A call to the current function's return continuation is not a jump: the
argument tuple simply *is* the Cmm function body's result
(translate_jump_to_return_continuation), reaching CM.Unit.Final /
CM.Exit(Return_lbl). A `Pop` trap action becomes a `trap_return` (pop-then-return).
A `Push` on the return continuation is rejected.
```

```rule
RULE TC.ApplyCont.Inline
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#apply_cont
---
e = Apply_cont k (s̄);  Φ(k) = Inline ⟨params x̄, e_h, occ⟩;  trap_action(e) = None
|s̄| = |x̄|;   Θ′ = Θ binding each non-Rec_info xᵢ ⤳ᵥ (translation of sᵢ) ([18])
Θ′ ⊢ e_h ⤳ e_h_c
--------------------------------------------------
Θ ⊢ e ⤳ e_h_c
NOTES: Calling an inlined continuation splices its handler in, binding the params
to the argument simples (via bind_var_to_simple; Rec_info/depth params skipped).
Emits no Cmm control construct — this is where TC.LetCont.Inline pays off. An
inlined continuation must not carry a trap action (checked; fatal otherwise).
```

```rule
RULE TC.ApplyCont.Raise
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_raise
CODE backend/cmm_helpers.ml#raise_prim
---
e = Apply_cont k (s̄);  k is an exn handler (Env.is_exn_handler);  s̄ = s_exn :: s̄_extra
trap_action(e) = Some (Pop { raise_kind; … })   (else fatal)
Θ ⊢ s_exn ⤳ᵥ v_exn ;  Θ ⊢ s̄_extra ⤳ᵥ v̄_extra ;  flush(Θ) Flush_everything
--------------------------------------------------
Θ ⊢ e ⤳ Cop(Craise (if !Clflags.debug then raise_kind else Raise_notrace), v_exn :: v̄_extra, dbg)
NOTES: A raise (an Apply_cont to an exn handler carrying a Pop) becomes
`raise_prim` (translate_raise); the exception value is the first `Cop` operand
and the extra handler args are the remaining operands, later threaded through
distinguished per-handler registers by cfg_selectgen (emit_expr_raise), not
mutable slots. `raise_prim` preserves the source `raise_kind` only under
`!Clflags.debug` (-g); in the default build the operator is `Raise_notrace`.
This affects only backtrace recording, not control flow. Cmm image of
OS.ApplyCont.Raise; steps to CM.Raise. Reaching an exn handler *without* a Pop
is a fatal to_cmm error.
```

## 5. Translating applications (`Apply`)

The call itself (`translate_apply0`) selects the Cmm call form by call kind; the
*return continuation* handling (`apply_expr`) then places the result. Only the
control aspect is here; the calling convention detail is in
[`15`](15-cmm.md) (`CM.Apply`/`CM.Extcall`).

```rule
RULE TC.Apply.Call
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_apply0
CAVEAT disclosure: Method and Effect call translations are noted only; their semantics are out of scope (ch. 01, OS.Apply.Method/Effect) and carry no ≈-correctness obligation.
---
For Apply { callee; args = s̄; call_kind; alloc_mode; … }:
  Function (Direct cid)            ⤳ C.direct_call code_sym(cid) v̄   (my_closure appended
                                     if is_my_closure_used)
  Function Indirect_unknown_arity  ⤳ C.indirect_call callee v̄        (generic caml_apply)
  Function (Indirect_known_arity)  ⤳ C.indirect_full_call callee v̄
  C_call { … }                     ⤳ C.extcall … (TC via translate_external_call)
where Θ ⊢ callee ⤳ᵥ callee_c and Θ ⊢ s̄ ⤳ᵥ v̄.
--------------------------------------------------
The resulting Cmm call expression `call`, consumed by TC.Apply.Return.
NOTES: Cmm image of OS.Apply.Direct / .Indirect* / .CCall; steps to CM.Apply /
CM.Extcall. `Method` calls ⤳ C.send and `Effect` ops ⤳ perform/resume/… are
emitted by translate_apply0 but their SEMANTICS are out of scope (ch. 01;
OS.Apply.Method/Effect); their translation is noted, not given a `≈`-correctness
obligation.
```

```rule
RULE TC.Apply.Return
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#apply_expr
---
Let `call` = TC.Apply.Call for the application, with return continuation dst:
  dst = Never_returns                    ⤳ flush; e_c = call
  dst = Return k, Φ(k) = Return _        ⤳ flush; e_c = call        (tail: block result)
  dst = Return k, Φ(k) = Jump ⟨_, lbl⟩   ⤳ flush; e_c = Cexit(lbl, [call], [])
  dst = Return k, Φ(k) = Inline ⟨[x], e_h, occ⟩  ⤳ bind x ⤳ᵥ call; Θ′ ⊢ e_h ⤳ e_h_c
  dst = Return k, Φ(k) = Inline ⟨x̄, e_h, occ⟩ (|x̄|>1) ⤳ flush(Θ) Branching_point;
                                     Ccatch(Normal,[⟨lbl,x̄_c,e_h_c,false⟩],
                                     Cexit(lbl,[call],[]))   (destructure multi-value result)
--------------------------------------------------
Θ ⊢ Apply … ⤳ e_c
NOTES: apply_expr's three cases. Never_returns and a Return-cont that is the
function's own return flush and leave the call as the tail expression. A Jump
target wraps the call in a Cexit to the join point. A single-param Inline binds
the call result and splices the handler (no Cmm control). A multi-param Inline
uses a Ccatch/Cexit pair to destructure the unboxed multi-value result. If the
Apply's exn continuation carries extra args, translate_apply additionally wraps
the call in a Ctrywith that reraises with the extras (TC.Apply.ExnWrapper).
```

```rule
RULE TC.Apply.ExnWrapper
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#translate_apply
CODE backend/cmm_helpers.ml#trywith
CODE backend/cmm_helpers.ml#raise_prim
CAVEAT disclosure: rule closes fidelity finding KF-033 (Cmm-side twin of KF-019); omitting the wrapper leaves the composed model stuck at a defined program point.
---
Let `call` = TC.Apply.Call for the application, with exn continuation k_exn:
  extra_args(k_exn) = []  ⟹  call is used unchanged (the common case)
  extra_args(k_exn) = s̄_extra ≠ [],  Θ ⊢ s̄_extra ⤳v v̄_extra  (translated at the CALL site):
    call′ = Ccatch(Exn_handler, [⟨lbl_w, [x_exn],
                      Cop(Craise (if !Clflags.debug then Raise_reraise else Raise_notrace),
                          x_exn :: v̄_extra)⟩],   -- reraise with the extras
              Ccatch(Normal, [⟨lbl_pop, [(x_res : return machtype)], x_res⟩],
                Ccatch(Normal, [⟨lbl_push, [], Cexit(lbl_pop, [call], [Pop lbl_w])⟩],
                  Cexit(lbl_push, [], [Push lbl_w]))))
    with x_exn, x_res, lbl_pop, lbl_w, lbl_push fresh
--------------------------------------------------
TC.Apply.Return consumes call′ in place of call.
NOTES: The arity bridge for callee exceptions. A callee's uncaught exception
arrives bucket-only (CM.Apply.Raise), but an extras-carrying source exn handler
compiles to an Exn_handler expecting the extras as additional Craise operands
(TC.LetCont.Exn, TC.ApplyCont.Raise) — without this wrapper the composed model
is stuck at a defined program point (fidelity finding KF-033, the Cmm-side twin
of KF-019). translate_apply catches the bucket in a fresh extras-FREE
Exn_handler and reraises (debug-gated as in TC.ApplyCont.Raise: Raise_reraise
under -g, else Raise_notrace) with the extras, which are translated
at the call site, where they are in scope — the same device used when inlining
a function into a context whose exn continuation takes extra args (code
comment). The Push/Pop trap actions are explicit on the Cexits: the push
scaffold enters the guarded region, and the return path rides
Cexit(lbl_pop, [call], [Pop lbl_w]) so the trap stack is balanced on both the
return and raise paths. The Exn_handler is outermost: C.trywith is applied
last, wrapping the pop/push scaffold. Steps to CM.Catch.Exn / CM.Raise.
```

## 6. Translating switches (`Switch`)

```rule
RULE TC.Switch
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#switch
CODE backend/cmm_helpers.ml#transl_switch_clambda
CODE backend/cmm_helpers.ml#ite
---
e = Switch { scrutinee = s; arms };   Θ ⊢ s ⤳ᵥ sc   (untagged, or tagged when smaller)
each arm action aⱼ (an Apply_cont) translated by §4:  Θ ⊢ aⱼ ⤳ e_cⱼ
flush(Θ) Branching_point
|arms| = 2  ⟹  a two-way branch. to_cmm first RE-TAGS the scrutinee when the tagged
   form is smaller (must_tag_discriminant), otherwise uses the untagged sc. The
   emitted form is normally an EQUALITY TEST against one arm's discriminant d,
   evaluated AFTER any re-tagging:
     e_c = C.ite (C.eq (C.int d) sc) if_d if_other
   so for a source bool/int switch d is a *tagged* value, e.g. `(if (== 1 b) …)` or
   `(if (!= x 1) …)`. Only when a (post-tagging) discriminant is literally 0 does it
   collapse to the bare  e_c = C.ite sc e_c⁰ e_c¹  with arm 0 as the else branch (the
   must_tag = false, genuinely-untagged case, rarely reached from source).
|arms| = n > 2 ⟹ e_c = transl_switch_clambda sc index [cases],
                index over [0, max_d]; a gap ↦ a synthesized Cinvalid "unreachable
                switch case" case (needs_unreachable)
--------------------------------------------------
Θ ⊢ e ⤳ e_c
NOTES: Cmm image of OS.Switch / OS.Switch.Undef. Two-armed switches become an
if-then-else (CM.If) — kept as `C.ite` rather than a real Cswitch so Selectgen can
fuse the comparison. Larger switches become a `Cswitch` with an index table
(CM.Switch); missing discriminants below the max map to an explicit `Cinvalid`,
values above the max fall out of the table (genuine UB). The scrutinee is
untagged unless a 2-arm switch's tagged form is strictly smaller
(Env.extra_info / cmm_arith_size). The Branching_point flush keeps bindings from
being duplicated across arms.
```

## 7. Invalid

```rule
RULE TC.Invalid
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#invalid
CODE middle_end/flambda2/to_cmm/to_cmm_shared.ml#invalid
---
e = Invalid { message };  flush(Θ) Flush_everything
--------------------------------------------------
Θ ⊢ e ⤳ Cinvalid { message }
NOTES: Flambda `Invalid` (OS.Invalid) becomes Cmm `Cinvalid` (CM.Invalid) via
C.invalid, allocating a message symbol. A correct pipeline never reaches it
(ch. 20 composes with 13's "reachable ⇏ Invalid").
```

## 8. The control simulation lemma

The control rules preserve behaviour: a Flambda control step is matched by
Cmm steps that keep the control relation — corresponding continuation
realizations, and trap/region stacks in lockstep. This is the target-independent
part of the full `to_cmm` simulation ([`20`](20-to-cmm-soundness.md),
`INV.ToCmm.Simulates`); it needs only the trivial part of `≈` (control values,
labels, and the trap/region-stack correspondence), not the memory layout.

```rule
RULE INV.ToCmm.Control
CLAIM normative
CODE middle_end/flambda2/to_cmm/to_cmm_expr.ml#expr
CODE middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_continuation_handler
---
Assume Θ ⊢ e ⤳ e_c and a control relation ≈_ctrl linking: the Flambda continuation
env K with χ via Φ (Jump↦CHandler at the same label; Inline↦spliced; Return↦the
function boundary; Exn↦Exn_handler); the trap stacks T ≈ TT (same handler
identities, same depth); the region stacks R ≈ RR.
Then for every Flambda control transition
  ⟨e, ρ, K, H, T, R⟩ ⟶ ⟨e′, ρ′, K′, H′, T′, R′⟩         (an OS.LetCont*/ApplyCont*/Switch/Apply/Invalid step)
there is a matching Cmm run
  ⟨e_c, ce, χ, M, TT, RR⟩ ⟶c* ⟨e_c′, ce′, χ′, M′, TT′, RR′⟩
with Θ′ ⊢ e′ ⤳ e_c′ and the relation re-established (T′ ≈ TT′, R′ ≈ RR′, K′ ≈ Φ′),
where H ≈ M is preserved unchanged by control steps.
--------------------------------------------------
Control transfers correspond step-for-step; trap Push/Pop and region push/pop
stay in lockstep (TC.ApplyCont.Jump/Return/Raise, TC.LetCont.*, TC.Switch,
TC.Apply.Return). Reaching OS.Invalid corresponds to reaching CM.Invalid, both
excluded on reachable states by soundness (ch. 20).
NOTES: Empirically validated by the control case studies
(tocmm-switch, tocmm-trywith, tocmm-inline-cont; 14-validation). Target-
independent: no rule here mentions word size, endianness, or layout. It is a
specialization of INV.ToCmm.Simulates (ch. 20) to the control fragment, proved
by cases on the Flambda control transition and the ⤳ rules above. The trap-depth
balance invariant (|T| constant per program point, 04 §1.7) transfers to |TT|
because Push/Pop translate one-to-one (TC.ApplyCont.Jump).
```

## 9. Summary of rules

Dispatch: `TC.Expr.Dispatch`, `TC.LetCont.Classify`.

Continuation binders: `TC.LetCont.Inline`, `TC.LetCont.Jump`, `TC.LetCont.Exn`,
`TC.LetCont.Rec`.

Continuation calls: `TC.ApplyCont.Jump`, `TC.ApplyCont.Return`,
`TC.ApplyCont.Inline`, `TC.ApplyCont.Raise`.

Applications / switch / invalid: `TC.Apply.Call`, `TC.Apply.Return`, `TC.Switch`,
`TC.Invalid`.

Simulation: `INV.ToCmm.Control`.

The `Let`-form (data) translation rules `TC.Let*`, the simple/variable judgment
`⤳ᵥ`, and the flush/let-substitution correctness are owned by
[`18`](18-to-cmm-data.md).
