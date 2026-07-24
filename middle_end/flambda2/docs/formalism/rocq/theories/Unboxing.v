(* Unboxing.v -- ch. 12 (12-unboxing.md): unboxing.  All 18
   S.Unbox.* rules.
   Owner: Girard (wave 5).
   Imports: Base, Syntax, TypeGrammar, MeetJoin.

   Contents:
   - Section 1: where continuation-parameter unboxing hooks in
     (anchor).
   - Section 2: the decision language (Unboxing_types) and the
     Unbox? oracle Parameter (sanctioned oracle).
   - Section 3: the oracle's three stages -- optimistic decision,
     use-site refinement, benefit filter (anchors) -- and the
     loopified boxed-accumulator conjecture.
   - Section 4: variants (anchors).
   - Section 5: [@unboxable] wrappers (anchor; from_lambda, out of
     Simplify's scope).
   - Section 6: the handler-side typing-environment equation
     (S.Unbox.Denv.Equation, normative) and the term rewrite
     (rw_unbox, single constructor for S.Unbox.ContParam.Rewrite).
   - Section 7: mutable unboxing / ref-to-var (anchors).

   File-wide ENCODING NOTE: most of ch. 12 documents the unboxing
   pass's internal algorithm (optimistic decisions, per-use
   refinement, benefit filtering, flow-based mutable unboxing) --
   machinery quantified over dacc / flow-analysis internals that the
   model does not carry.  Those rules are documented anchors (the
   sanctioned exception in the CORRESPONDENCE catalog, as for the
   ch. 09 anchor set).  What IS modeled: the decision language, the
   Unbox? oracle, the normative handler-side equation, and the
   headline Let_cont rewrite shape. *)

From Stdlib Require Import ZArith List Bool.
From Flambda2 Require Import Base Syntax TypeGrammar PrimMemoryA.
From Flambda2 Require Import MeetJoin.
Import ListNotations.

Local Open Scope Z_scope.

(* ================================================================== *)
(* 1. Where continuation-parameter unboxing hooks in                  *)
(* ================================================================== *)

(** RULE S.Unbox.ContParam.Hook (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#prepare_dacc_for_handlers
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler
    CODE middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#make_decisions

    Unbox decisions for a handler's params are made on the downwards
    pass, from the types joined over the (known) use sites; the extra
    params and args are emitted on the upwards pass.  Return,
    Toplevel_return, exn handlers, and inlinable single-use
    continuations are never unboxed.  Recursive handlers decide
    optimistically, from the parameter type alone, without use-site
    refinement or benefit filtering. *)
Definition S_Unbox_ContParam_Hook_documented : Prop := True.

(* ================================================================== *)
(* 2. The decision language (Unboxing_types) and the oracle           *)
(* ================================================================== *)

(* Apply_cont_rewrite_id.t: the identity of one use site of the
   continuation.  ENCODING NOTE: an abstract counter in the code,
   encoded as Z. *)
Definition rewrite_id := Z.

(* Extra_arg.t: what to pass for one component at one use site
   (12-unboxing.md section 3.2).  The wrapper case carries the
   named-argument builder as a function from the (named) argument
   simples to the projection primitive. *)
Inductive extra_arg :=
  EA_already_in_scope (s : simple)
| EA_new_let_binding (v : variable) (p : prim)
| EA_new_let_binding_with_named_args
    (v : variable) (mk : list simple -> prim).

(* Extra_param_and_args.t: the unit of the transformation -- a fresh
   component parameter together with one Extra_arg per use site
   (keyed by Apply_cont_rewrite_id). *)
Record epa := Mk_epa
  { epa_param : variable;
    epa_args : fmap rewrite_id extra_arg }.

(* Unboxing_types.do_not_unbox_reason. *)
Inductive do_not_unbox_reason :=
  UR_not_beneficial
| UR_max_depth_exceeded
| UR_incomplete_parameter_type
| UR_not_enough_information_at_use
| UR_not_of_kind_value
| UR_unboxing_not_requested
| UR_all_fields_invalid.

(* Unboxing_types.decision / unboxing_decision / field_decision /
   const_ctors (12-unboxing.md section 2): how one box decomposes
   into components; each field carries its own nested decision, so
   unboxing recurses into fields. *)
Inductive decision : Type :=
  Do_not_unbox (r : do_not_unbox_reason)
| Unbox (u : unboxing_decision)

with unboxing_decision : Type :=
  UD_unique_tag_and_size
    (t : tag) (shape : block_shape) (fields : list field_decision)
| UD_variant
    (tag_epa : epa) (cc : const_ctors)
    (fields_by_tag : list (tag * list field_decision))
| UD_closure_single_entry
    (fs : function_slot)
    (vars_within_closure : list (value_slot * field_decision))
| UD_number (nnk : naked_number_kind) (e : epa)

with field_decision : Type :=
  Mk_field_decision (e : epa) (d : decision) (k : kind_ws)

with const_ctors : Type :=
  CC_zero
| CC_at_least_one (is_int : epa) (ctor : decision).

(* The use-site information handed to the oracle: make_decisions'
   Non_recursive arg_types_by_use_id (the argument type at each
   known use), or Recursive (back-edge uses not known yet: decide
   from the parameter type alone; refinement and the benefit filter
   are skipped). *)
Inductive unbox_uses :=
  UU_non_recursive (arg_types_by_use : list (rewrite_id * ftype))
| UU_recursive.

(* The doc's Unbox?(param, uses) (README; 12-unboxing.md section 0).
   Sanctioned oracle Parameter (CORRESPONDENCE encoding table):
   sections 3-4 of the chapter document the decision procedure
   descriptively (anchors below); the model treats the verdict as
   opaque.  The ftype argument is the parameter's joined type in the
   given environment. *)
Parameter unbox_dec :
  tenv -> variable -> ftype -> unbox_uses -> decision.

(* ================================================================== *)
(* 3. The oracle's stages (anchors) and the loopify conjecture        *)
(* ================================================================== *)

(** RULE S.Unbox.Optimistic.Number (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_number_decision
    CODE middle_end/flambda2/simplify/unboxing/unboxers.ml
    VERIFIED 14-validation/new-07-float-unbox.md

    If E proves prove_is_a_boxed_<nnk>(param_type) for some
    naked-number kind nnk, the optimistic decision is
    Unbox (Number (nnk, fresh epa)).  Deciders tried in order:
    immediate (untagging), float, float32, int32, int64, nativeint,
    vec128, vec256, vec512.  Tagged immediates are a degenerate
    "boxed number" whose unboxing is untagging. *)
Definition S_Unbox_Optimistic_Number_documented : Prop := True.

(** RULE S.Unbox.Optimistic.Block (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision
    CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_fields

    If E proves prove_unique_tag_and_size(param_type) = (tag, shape,
    size), below max_unboxing_depth: a fresh epa is made per field
    (kind from the shape), and param_type is met against the
    reconstructed immutable block over the epas' alias types.  The
    meet both checks feasibility (nonbottom) and produces the field
    types that drive the recursive per-field decisions at depth+1.
    Meet bottom for all fields => Do_not_unbox All_fields_invalid. *)
Definition S_Unbox_Optimistic_Block_documented : Prop := True.

(** RULE S.Unbox.Optimistic.Closure (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_vars_within_closure

    If E proves prove_single_closures_entry(param_type), the
    continuation is not recursive, and depth < max_unboxing_depth:
    each value slot of the entry becomes a recursively-decided fresh
    epa component of Closure_single_entry. *)
Definition S_Unbox_Optimistic_Closure_documented : Prop := True.

(** RULE S.Unbox.Depth (CLAIM descriptive) -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision
    CODE driver/oxcaml_flags.ml#Flambda2.Expert

    At recursion depth >= max_unboxing_depth (default 3,
    -flambda2-expert-max-unboxing-depth), a non-number param type
    gets Do_not_unbox Max_depth_exceeded.  Boxed numbers are decided
    before the depth check, so a number at any depth still unboxes;
    the bound only limits nesting of blocks/variants/closures. *)
Definition S_Unbox_Depth_documented : Prop := True.

(** RULE S.Unbox.ExtraArg.Available (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg
    CODE middle_end/flambda2/simplify/unboxing/unboxers.ml

    If the use's argument is a Simple s available in the use's
    typing env, and the unboxer's prove_simple on alias_type_of s
    returns Known_result s', the extra arg at this use is
    Already_in_scope s': the component is already a Simple in scope,
    no projection is emitted -- what makes the unboxing beneficial
    (section 3.3). *)
Definition S_Unbox_ExtraArg_Available_documented : Prop := True.

(** RULE S.Unbox.ExtraArg.Project (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg

    If prove_simple returns Need_meet, the use site gains a
    New_let_binding (fresh var, unboxer.unboxing_prim s) performing
    the projection (Untag_immediate / Unbox_number / Block_load /
    Project_value_slot).  This projection is a cost weighed by the
    benefit filter. *)
Definition S_Unbox_ExtraArg_Project_documented : Prop := True.

(** RULE S.Unbox.ExtraArg.Invalid (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#unbox_arg

    If prove_simple returns Invalid, the argument's type proves the
    box cannot have this shape here, so control cannot reach this
    Apply_cont: raise Invalid_apply_cont, and record the use in
    rewrites_ids_known_as_invalid -- unreachable rather than
    blocking the unboxing. *)
Definition S_Unbox_ExtraArg_Invalid_documented : Prop := True.

(** RULE S.Unbox.Refine.Pass (CLAIM descriptive) -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#refine_decision_based_on_arg_types_at_uses
    CODE middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#compute_extra_params_and_args
    CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_one_decision_and_use

    Two passes: pass = Filter (downwards) folds over the known use
    sites; a Prevent_current_unboxing demotes the decision to
    Do_not_unbox Not_enough_information_at_use.  pass =
    Compute_all_extra_args (upwards, once every use incl. recursive
    back-edges is known) fills the remaining extra args and must not
    encounter a Prevent_current_unboxing.  Each component epa gains
    one Extra_arg per use site, or the use is marked invalid. *)
Definition S_Unbox_Refine_Pass_documented : Prop := True.

(** RULE S.Unbox.Beneficial (CLAIM descriptive) -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/is_unboxing_beneficial.ml#is_unboxing_beneficial_for_epa
    CODE middle_end/flambda2/simplify/unboxing/is_unboxing_beneficial.ml#filter_non_beneficial_decisions

    An Unbox U none of whose (transitively reachable) components has
    an Already_in_scope non-poison extra arg at any use site is
    demoted to Do_not_unbox Not_beneficial.  Exception: Number
    Naked_immediate is always kept (at worst it untags an integer,
    cheap); other boxed numbers and every block/variant/closure must
    show a beneficial component. *)
Definition S_Unbox_Beneficial_documented : Prop := True.

(* ------------------------------------------------------------------ *)
(* 3.4 Loopified boxed accumulators                                   *)
(* ------------------------------------------------------------------ *)

(* The non-null head of the unknown_with_subkind type of a
   loop-carried accumulator's boxed-number declared subkind (boxed
   float / float32 / int32 / int64 / nativeint / vec128/256/512):
   the boxed head whose payload is the UNKNOWN type of the
   corresponding naked kind.  The alloc mode is a parameter:
   unknown_with_subkind's mode is heap-or-local or heap depending on
   the stack-allocation config, and the boxed-number provers never
   consult it.  Naked_immediate is excluded on purpose: the rule is
   about genuinely boxed accumulators; int8/int16 have no boxed
   form. *)
Definition accum_unknown_head (nnk : naked_number_kind)
    (am : alloc_mode_types) : option head_value_non_null :=
  match nnk with
  | NN_naked_float =>
      Some (HV_boxed_float (unknown_of_kind K_naked_float) am)
  | NN_naked_float32 =>
      Some (HV_boxed_float32 (unknown_of_kind K_naked_float32) am)
  | NN_naked_int32 =>
      Some (HV_boxed_int32 (unknown_of_kind K_naked_int32) am)
  | NN_naked_int64 =>
      Some (HV_boxed_int64 (unknown_of_kind K_naked_int64) am)
  | NN_naked_nativeint =>
      Some (HV_boxed_nativeint (unknown_of_kind K_naked_nativeint) am)
  | NN_naked_vec128 =>
      Some (HV_boxed_vec128 (unknown_of_kind K_naked_vec128) am)
  | NN_naked_vec256 =>
      Some (HV_boxed_vec256 (unknown_of_kind K_naked_vec256) am)
  | NN_naked_vec512 =>
      Some (HV_boxed_vec512 (unknown_of_kind K_naked_vec512) am)
  | NN_naked_immediate | NN_naked_int8 | NN_naked_int16 => None
  end.

(** RULE S.Unbox.Loopify.AccumBoxElim (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#make_decisions
    CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_number_decision
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler

    Doc statement, two clauses, for code c loopified
    (S.Rewrite.Loopify.Body) with self continuation k and a
    LOOP-VARYING param p_j of boxed-number declared subkind (an
    invariant param is instead removed wholesale by
    S.Rewrite.Loopify.InvariantArgElim -- no thread, no allocation):
    (1) the unboxing DECISION for p_j always fires, from the
        declared subkind alone: the recursive path gives the param
        unknown_with_subkind (S.Struct.Rec.InvariantVsVariant), on
        which prove_is_a_boxed_<nnk> is Proved
        (S.Unbox.Optimistic.Number); refinement and the benefit
        filter are skipped for recursive continuations;
    (2) the BOXED param p_j' -- and the box allocation on every
        back-edge -- is eliminated IFF the handler, rewritten under
        the param = box(component) equation (S.Unbox.Denv.Equation),
        does not consume the boxed value as a box on any path;
        returning a varying accumulator at a loop exit keeps p_j' in
        required_names, so the residual loop threads BOTH values and
        allocates a fresh box each iteration.  No exit-edge
        re-boxing is ever introduced.

    ENCODING NOTE: clause (1) is stated below on exactly the
    unknown_with_subkind type of the declared subkind: Not_null,
    unknown boxed payload (accum_unknown_head above), No_alias form
    (so the alias-to-symbol guard cannot fire) -- on it the oracle
    returns Unbox (Number (nnk, e)).  Not_null is load-bearing: on
    any Maybe_null type gen_value_to_proof returns Unknown, so
    prove_is_a_boxed_<nnk> never proves and the decision does NOT
    fire; only the alloc mode stays quantified (config-dependent,
    never consulted by the provers).  Clause (2) quantifies over
    required_names -- whole-body flow facts of the unmodeled pass
    internals -- and stays documented here per the HYBRID variant of
    the catalog-37 exception (CORRESPONDENCE "HYBRID variant", which
    names this rule); its composition chain is
    S.Rewrite.Loopify.Body / .InvariantArgElim,
    S.Unbox.Optimistic.Number, S.Unbox.Denv.Equation,
    S.Unbox.ContParam.Rewrite, S.Struct.Rec.InvariantVsVariant. *)
Theorem S_Unbox_Loopify_AccumBoxElim :
  forall (E : tenv) (p : variable) (nnk : naked_number_kind)
         (am : alloc_mode_types) (h : head_value_non_null),
    accum_unknown_head nnk am = Some h ->
    exists e,
      unbox_dec E p
        (FT_value
           (Oub_ok (No_alias (Mk_head_value (Oub_ok h) Not_null))))
        UU_recursive
      = Unbox (UD_number nnk e).
Admitted.

(* ================================================================== *)
(* 4. Variants (anchors)                                              *)
(* ================================================================== *)

(** RULE S.Unbox.Optimistic.Variant (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/optimistic_unboxing_decision.ml#make_optimistic_decision
    CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_variant

    If E proves prove_variant_like(param_type) (not recursive, below
    max depth, and at least one non-constant constructor's fields
    survive the meet), the decision is Unbox (Variant { tag;
    const_ctors; fields_by_tag }), collapsing to Unique_tag_and_size
    when const_ctors = Zero and fields_by_tag is a singleton. *)
Definition S_Unbox_Optimistic_Variant_documented : Prop := True.

(** RULE S.Unbox.Variant.Discriminator (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#compute_extra_args_for_variant
    CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#extra_args_for_const_ctor_of_variant

    Per use, from the use's meet_variant_like: only non-const ctors
    possible => a single tag t is required; tag <- t, is_int <-
    false, tag t's fields projected, the other tags' fields <-
    poison.  Only const ctors possible => is_int <- true, ctor <-
    the immediate recovered by meet_tagging_of_simple, all block
    fields <- poison.  Both possible => Prevent_current_unboxing.
    Poison arguments never count as beneficial. *)
Definition S_Unbox_Variant_Discriminator_documented : Prop := True.

(* ================================================================== *)
(* 5. Function parameter/result unboxing (wrappers; anchor)           *)
(* ================================================================== *)

(** RULE S.Unbox.FunParam.Wrapper (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda.ml#cps_function
    CODE middle_end/flambda2/from_lambda/closure_conversion.ml#compute_body_of_unboxed_function
    CODE middle_end/flambda2/from_lambda/closure_conversion.ml#make_unboxed_function_wrapper

    A function with [@unbox_return] and/or an [@unbox_param]
    parameter (and not a stub) is CPS-converted into an
    Unboxed_calling_convention: a boxed wrapper (keeping the
    original function slot and calling convention) that unboxes the
    params, calls the unboxed main function (fresh "_unboxed"
    function slot), and re-boxes the return.  The actual removal of
    the boxing is left to Simplify (wrapper inlining + this
    chapter's unboxing + constant folding).  Entirely in
    from_lambda, out of Simplify's scope; recorded for
    traceability.  Cross-function unboxing by the Reaper pass is out
    of scope for the chapter. *)
Definition S_Unbox_FunParam_Wrapper_documented : Prop := True.

(* ================================================================== *)
(* 6. The handler-side equation and the term rewrite                  *)
(* ================================================================== *)

(* ------------------------------------------------------------------ *)
(* 6.1 Flattened component parameters                                 *)
(* ------------------------------------------------------------------ *)

(* The component parameters x_1 ... x_n of a decision, flattened in
   tree order (12-unboxing.md section 6: "component parameters
   x_1...x_n (flattened from U)").  Inner fixpoints for guardedness
   over the nested lists, as in Syntax.v's replace_apply_cont. *)
Fixpoint ud_vars (u : unboxing_decision) : list variable :=
  match u with
  | UD_unique_tag_and_size _ _ fields =>
      (fix go (l : list field_decision) : list variable :=
         match l with
         | [] => []
         | fd :: tl => fd_vars fd ++ go tl
         end) fields
  | UD_variant te cc fbt =>
      epa_param te
      :: cc_vars cc
         ++ (fix go (l : list (tag * list field_decision))
               : list variable :=
               match l with
               | [] => []
               | (_, fds) :: tl =>
                   (fix go2 (l2 : list field_decision)
                      : list variable :=
                      match l2 with
                      | [] => []
                      | fd :: tl2 => fd_vars fd ++ go2 tl2
                      end) fds ++ go tl
               end) fbt
  | UD_closure_single_entry _ vws =>
      (fix go (l : list (value_slot * field_decision))
         : list variable :=
         match l with
         | [] => []
         | (_, fd) :: tl => fd_vars fd ++ go tl
         end) vws
  | UD_number _ e => [epa_param e]
  end

with fd_vars (fd : field_decision) : list variable :=
  match fd with
  | Mk_field_decision e d _ => epa_param e :: dec_vars d
  end

with dec_vars (d : decision) : list variable :=
  match d with
  | Do_not_unbox _ => []
  | Unbox u => ud_vars u
  end

with cc_vars (cc : const_ctors) : list variable :=
  match cc with
  | CC_zero => []
  | CC_at_least_one ie ctor => epa_param ie :: dec_vars ctor
  end.

(* ------------------------------------------------------------------ *)
(* 6.2 The reconstructed-box shape of a decision                      *)
(* ------------------------------------------------------------------ *)

Definition alias_of_var (k : kind) (x : variable) : ftype :=
  alias_type_of k (Simple_name (Name_var x) Coercion_id).

(* A non-null Value type with the given head. *)
Definition value_ty (h : head_value_non_null) : ftype :=
  FT_value (Oub_ok (No_alias (Mk_head_value (Oub_ok h) Not_null))).

(* boxed_<nnk>_alias_to: the head of a boxed number whose contents
   is the given payload type.  The Naked_immediate case is the
   degenerate tagged immediate: a variant whose immediates arm is
   the payload alias and whose blocks arm is bottom (no block
   form).  int8/int16 have no boxed form. *)
Definition boxed_number_shape (nnk : naked_number_kind)
    (payload : ftype) (am : alloc_mode_types)
  : option head_value_non_null :=
  match nnk with
  | NN_naked_float => Some (HV_boxed_float payload am)
  | NN_naked_float32 => Some (HV_boxed_float32 payload am)
  | NN_naked_int32 => Some (HV_boxed_int32 payload am)
  | NN_naked_int64 => Some (HV_boxed_int64 payload am)
  | NN_naked_nativeint => Some (HV_boxed_nativeint payload am)
  | NN_naked_vec128 => Some (HV_boxed_vec128 payload am)
  | NN_naked_vec256 => Some (HV_boxed_vec256 payload am)
  | NN_naked_vec512 => Some (HV_boxed_vec512 payload am)
  | NN_naked_immediate =>
      Some (HV_variant None (Ou_known payload) None
              (Ou_known (Mk_row_like_for_blocks fempty Ob_bottom am))
              No_extensions false)
  | NN_naked_int8 | NN_naked_int16 => None
  end.

(* The alias type of a field's component parameter, at the field's
   kind. *)
Definition fd_alias_ty (fd : field_decision) : ftype :=
  match fd with
  | Mk_field_decision e _ kws => alias_of_var (ws_kind kws) (epa_param e)
  end.

(* The shape T built from a decision U (the premise table of
   S.Unbox.Denv.Equation): Number -> boxed_<nnk>_alias_to epa;
   Unique_tag_and_size -> immutable_block tag shape (alias-types of
   the field epas); Closure_single_entry ->
   closure_with_at_least_these_value_slots; Variant -> variant with
   const_ctors / non_const_ctors from the field epas.
   ENCODING NOTE: relational, with the creator-internal slack
   existential (alloc modes; the variant's immediates arm beyond the
   const_ctors = Zero case, its extensions/uniqueness; the closure
   entry's function/closure type maps).  The load-bearing content --
   which head each shape produces, the per-field/per-slot component
   aliases, the get_tag/is_int discriminator naming -- is pinned;
   the value-slot map and the by-tag case map are pinned in BOTH
   directions (no extra slots or tags beyond the decision's), and
   the variant's unlisted-tags arm is Ob_bottom, as the pass
   constructs them. *)
Inductive decision_shape : unboxing_decision -> ftype -> Prop :=
| DS_number : forall nnk e am h,
    boxed_number_shape nnk
      (alias_of_var (K_naked_number nnk) (epa_param e)) am = Some h ->
    decision_shape (UD_number nnk e) (value_ty h)
| DS_block : forall t shape fields am,
    decision_shape (UD_unique_tag_and_size t shape fields)
      (value_ty (HV_variant None
        (Ou_known (FT_naked_immediate Oub_bottom)) None
        (Ou_known (Mk_row_like_for_blocks
          (fupd tag_eqb fempty t (Ou_known (Mk_row_like_block_case
            (map fd_alias_ty fields)
            (Mk_row_like_index (Rl_known (Z.of_nat (length fields)))
               shape)
            fempty)))
          Ob_bottom am))
        No_extensions false))
| DS_closure : forall fs vws fts cts vst socc eext am,
    (forall w fd, In (w, fd) vws -> vst w = Some (fd_alias_ty fd)) ->
    (forall w ty, vst w = Some ty ->
       exists fd, In (w, fd) vws /\ ty = fd_alias_ty fd) ->
    decision_shape (UD_closure_single_entry fs vws)
      (value_ty (HV_closures
        (Mk_row_like_for_closures
          (fupd function_slot_eqb fempty fs
            (Mk_row_like_closures_case
              (Mk_closures_entry fts cts vst)
              (Mk_row_like_index (Rl_at_least socc) tt)
              eext))
          Ob_bottom)
        am))
| DS_variant : forall te cc fbt is_int_v imms known am exts uniq,
    is_int_v = (match cc with
                | CC_zero => None
                | CC_at_least_one ie _ => Some (epa_param ie)
                end) ->
    (cc = CC_zero -> imms = Ou_known (FT_naked_immediate Oub_bottom)) ->
    (forall t fds, In (t, fds) fbt ->
       exists idx eext,
         known t = Some (Ou_known (Mk_row_like_block_case
           (map fd_alias_ty fds) idx eext))) ->
    (forall t c, known t = Some c ->
       exists fds idx eext,
         In (t, fds) fbt /\
         c = Ou_known (Mk_row_like_block_case
               (map fd_alias_ty fds) idx eext)) ->
    decision_shape (UD_variant te cc fbt)
      (value_ty (HV_variant is_int_v imms (Some (epa_param te))
        (Ou_known (Mk_row_like_for_blocks known Ob_bottom am))
        exts uniq)).

(* ------------------------------------------------------------------ *)
(* 6.3 The handler-side typing-environment equation                   *)
(* ------------------------------------------------------------------ *)

(** RULE S.Unbox.Denv.Equation (CLAIM normative) -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/build_unboxing_denv.ml#denv_of_decision
    CODE middle_end/flambda2/simplify/unboxing/build_unboxing_denv.ml#add_equation_on_var

    Premises in doc order: the component parameters are defined (as
    extra variables) in the handler env; the shape T is built from U
    (decision_shape above); the conclusion equation E_handler |-
    param : T is added by meeting param's alias type with T.  This
    equation is what makes the handler's Block_load / Unbox_number /
    Project_value_slot on param reduce, via prove/meet, to the
    component parameters, so the box becomes dead.  For variants the
    denv also records Get_tag and Is_int relations (captured here by
    the variant head's discriminator-variable fields) and the
    corresponding CSE bindings.
    ENCODING NOTE: "defined as extra variables of their kinds" is
    encoded as name_bound_in for each flattened component (the
    per-component kinds live in the decision's field_decision /
    naked-number data).  The meet runs in an intermediate env E_mid,
    not in E: denv_of_decision defines the extra variables FIRST and
    meets in that extended denv (T's alias types mention the
    component variables, which E does not bind).  E_mid is pinned
    relationally: it binds the flattened components and gives param
    the same type it has in E; E_handler is E_mid after the
    equation, so it binds the components too. *)
Definition S_Unbox_Denv_Equation
    (E E_handler : tenv) (param : variable) (U : unboxing_decision)
  : Prop :=
  exists E_mid,
    (forall x, In x (ud_vars U) -> name_bound_in E_mid (Name_var x))
    /\ tenv_find E_mid (Name_var param) K_value
         = tenv_find E (Name_var param) K_value
    /\ (forall x, In x (ud_vars U) ->
          name_bound_in E_handler (Name_var x))
    /\ (exists T Tm eps,
          decision_shape U T /\
          meet E_mid (tenv_find E_mid (Name_var param) K_value) T
            (Meet_ok Tm eps) /\
          tenv_find E_handler (Name_var param) K_value = Tm).

(* ------------------------------------------------------------------ *)
(* 6.4 The term rewrite                                               *)
(* ------------------------------------------------------------------ *)

(** RULE S.Unbox.ContParam.Rewrite (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/unboxing/unboxing_epa.ml#add_extra_params_and_args
    CODE middle_end/flambda2/simplify/apply_cont_rewrite.ml#create
    CODE middle_end/flambda2/simplify/expr_builder.ml#rewrite_apply_cont
    CODE middle_end/flambda2/simplify/unboxing/unbox_continuation_params.ml#compute_extra_params_and_args
    VERIFIED 14-validation/new-07-float-unbox.md

    Premises in doc order: Unbox?(x, uses) = Unbox U with component
    parameters x_1...x_n flattened from U; the handler env carries
    the equation x : box(x_1...x_n) (S.Unbox.Denv.Equation); each
    use site r has extra args for x_1...x_n (section 3.2).
    Conclusion: Let_cont k(xbar) = e_handler in e_body rewrites to
    Let_cont k(xbar, x_1...x_n) = e_handler' in e_body', where each
    Apply_cont k(..., s, ...) at r becomes L_r[Apply_cont k(...,
    s, abar_r)] (L_r the New_let_binding projections), e_handler' is
    e_handler simplified under E_handler, and x (and any x_i) found
    unused is dropped by decide_param_usage.
    ENCODING NOTE (the decided fidelity caveat): the doc marks this
    rule descriptive because the exact term shape (extra-param
    threading, projection ordering, unused-param dropping) is
    algorithm-dependent.  Accordingly the constructors below pin the
    headline shape -- the same continuation, the extra component
    params appended (their variables exactly the flattened ud_vars),
    the exn-handler exclusion, and the Denv equation -- and leave
    e_handler' / e_body' (and the per-use L_r rewrites they embed)
    unconstrained.  Two further disclosed slacks: [uses] is
    existentially free rather than tied to k's use sites in [body]
    (use-site enumeration is pass bookkeeping); and the conclusion
    pins the PRE-DROPPING canonical form params ++ extra --
    unused-param dropping (decide_param_usage) happens downstream
    and is one of the algorithm-dependent shapes this caveat
    covers.  The separately-anchored ingredients
    (S.Unbox.Denv.Equation, S.Unbox.ExtraArg.*, S.Unbox.Beneficial)
    plus the chapter's section 8 worked example carry the rest.  The
    recursive-handler analogue is the second constructor: identical
    per-handler shape on one handler of an E_let_cont_rec group,
    with [uses] pinned to UU_recursive (recursive handlers decide
    from the parameter type alone, skipping refinement and the
    benefit filter -- S.Unbox.ContParam.Hook); the sibling handlers
    keep their continuations and order but their bodies are
    unconstrained, like e_handler' (their Apply_cont k sites gain
    the extra args too).  These constructors exist so the rewrites
    union (Simplify.v) has rw_unbox inhabited with the documented
    shape rather than an opaque Parameter.
    ENCODING NOTE: per the fifth frozen-interface amendment all rw_*
    relations share the closed type
    eff_flags -> code_env -> tenv -> expr -> expr -> Prop (the
    ambient effect-flag setting the pass runs under). No ch. 12 rule
    reads the flags, so [flags] is bound as a direct (unused)
    inductive parameter rather than a Section Variable: Rocq prunes
    section variables that do not occur in an Inductive, which would
    silently drop the argument from the closed type here. *)
Inductive rw_unbox (flags : eff_flags) (C : code_env) (E : tenv)
    : expr -> expr -> Prop :=
| RW_Unbox_ContParam :
    forall k params handler is_cold body x kws T uses U
           E_handler extra handler' body',
      In (x, kws) params ->
      tenv_find E (Name_var x) K_value = T ->
      unbox_dec E x T uses = Unbox U ->
      S_Unbox_Denv_Equation E E_handler x U ->
      map fst extra = ud_vars U ->
      rw_unbox flags C E
        (E_let_cont_nonrec k
           (Mk_cont_handler params handler false is_cold) body)
        (E_let_cont_nonrec k
           (Mk_cont_handler (params ++ extra) handler' false is_cold)
           body')
| RW_Unbox_ContParam_Rec :
    forall inv hs1 hs2 hs1' hs2' k params handler is_cold body
           x kws T U E_handler extra handler' body',
      In (x, kws) params ->
      tenv_find E (Name_var x) K_value = T ->
      unbox_dec E x T UU_recursive = Unbox U ->
      S_Unbox_Denv_Equation E E_handler x U ->
      map fst extra = ud_vars U ->
      map fst hs1' = map fst hs1 ->
      map fst hs2' = map fst hs2 ->
      rw_unbox flags C E
        (E_let_cont_rec inv
           (hs1 ++ (k, Mk_cont_handler params handler false is_cold)
              :: hs2)
           body)
        (E_let_cont_rec inv
           (hs1'
              ++ (k, Mk_cont_handler (params ++ extra) handler'
                       false is_cold)
              :: hs2')
           body').

(* ================================================================== *)
(* 7. Mutable unboxing (ref-to-var; anchors)                          *)
(* ================================================================== *)

(** RULE S.Unbox.Mutable.Candidate (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/flow/mutable_unboxing.ml#blocks_to_unbox
    CODE middle_end/flambda2/simplify/flow/mutable_unboxing.ml#escaping

    At the dataflow turn: prim = Make_block { tag; mut; fields }
    binding var, reachable in the flow graph, required (live), and
    not escaping (the union of escaping_by_alias, escaping_by_use,
    escaping_by_return) => var is recorded in blocks_to_unbox with
    its tag and field kinds.  Requiring non-escaping AND required is
    a correctness condition, not merely an optimization: a
    non-required block does not mark its fields as escaping. *)
Definition S_Unbox_Mutable_Candidate_documented : Prop := True.

(** RULE S.Unbox.Mutable.Rewrite (CLAIM descriptive)
    -- 12-unboxing.md
    CODE middle_end/flambda2/simplify/flow/mutable_unboxing.ml#Fold_prims.apply_prim
    CODE middle_end/flambda2/simplify/flow/mutable_unboxing.ml#compute_rewrites
    VERIFIED 14-validation/new-07-float-unbox.md

    For b in blocks_to_unbox, with current field bindings threaded
    forward (SSA-style, as extra continuation params where b is live
    across a continuation): Make_block b is removed (fields <- the
    makeblock arguments); Block_load b field i is replaced by a
    binding to fields[i] (invalid if absent); Block_set b field i v
    is removed (fields[i] <- v); Is_int b -> false; Get_tag b -> b's
    static tag.  A store followed by a load thus reads the stored
    value directly -- the ref-to-var effect.  Block references are
    canonicalized through the dominator alias map before lookup;
    the extra params/args merge into the normal EPA machinery. *)
Definition S_Unbox_Mutable_Rewrite_documented : Prop := True.
