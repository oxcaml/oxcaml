(* ==================================================================
   WellFormed.v -- 03-kinds.md: kinding and well-formedness judgments
   (sections 2-6).  WF.Subkind.Erasable and WF.Arity.Unarize live in
   Base.v (the kind/subkind/arity apparatus is Base's); everything
   else from ch. 03 is here.

   ENCODING NOTE: the per-primitive result-kind and argument-kind
   tables belong to chapters 05/06 (PrimScalar.v / PrimMemory*.v,
   wave 3); this wave-2 file abstracts them as Section Variables
   (result_kind_table, prim_arg_kinds), together with the code
   metadata arities (code_params_arity, code_result_arity) read by
   the WF.Apply.* rules.  Same pattern as Opsem.v's
   pure_prim/effectful_prim (CORRESPONDENCE catalog entry 21).
   ================================================================== *)

From Stdlib Require Import ZArith Bool List.
From Flambda2 Require Import Base Syntax.
Import ListNotations.

(* ================================================================== *)
(* 1. GC-scannability (03-kinds.md section 2)                         *)
(* ================================================================== *)

(** RULE WF.Subkind.Scannable (STATUS descriptive) -- 03-kinds.md
    CODE kinds/flambda_kind.ml#With_subkind.must_be_gc_scannable
    must_be_gc_scannable(kw) = true iff kind(kw) = Value and
    non_null_value_subkind(kw) <> Tagged_immediate: only Value-kinded
    values not known to be tagged immediates must be treated as GC
    roots.  Descriptive: documents the current classification used by
    block shapes to decide which fields need scanning, not a semantic
    invariant. *)
Definition WF_Subkind_Scannable_documented : Prop := True.

(* ================================================================== *)
(* 2. Apply arity flavours (03-kinds.md section 3)                    *)
(* ================================================================== *)

Definition arity_is_unarized (a : arity) : Prop :=
  Forall
    (fun c => match c with
              | Arity_singleton _ => True
              | Arity_unboxed_product _ => False
              end)
    a.

(** RULE WF.Arity.ApplyFlavours (STATUS normative) -- 03-kinds.md
    CODE terms/apply_expr.mli#create
    CODE terms/apply_expr.mli#args_arity
    CODE terms/apply_expr.mli#return_arity
    args_arity(apply) is [`Complex] (unboxed-product structure
    retained); return_arity(apply) is [`Unarized] (already flat).
    ENCODING NOTE: the phantom Complex/Unarized tags are not modeled
    (arity is one type in Base.v); the [`Complex] half is by
    construction (ap_args_arity may contain products), so the modeled
    content is that the return arity contains only singletons. *)
Definition WF_Arity_ApplyFlavours (ap : apply_expr) : Prop :=
  arity_is_unarized (ap_return_arity ap).

(* ================================================================== *)
(* 3. Kinding judgment Gamma |- s : kappa (03-kinds.md section 4)     *)
(* ================================================================== *)

(* Gamma maps variables to (bare) kinds; in the implementation this
   is the kind stored on each Variable.t, not a separate structure. *)
Definition kctx := fmap variable kind.

(* Case table of WF.Kind.Const (cases in the rule's order); the rule
   comment sits on the WF_Kind_Const constructor below. *)
Definition const_kind (c : const) : kind :=
  match c with
  | Const_tagged_immediate _ => K_value
  | Const_null => K_value
  | Const_naked_immediate _ => K_naked_immediate
  | Const_naked_float _ => K_naked_float
  | Const_naked_float32 _ => K_naked_float32
  | Const_naked_int8 _ => K_naked_int8
  | Const_naked_int16 _ => K_naked_int16
  | Const_naked_int32 _ => K_naked_int32
  | Const_naked_int64 _ => K_naked_int64
  | Const_naked_nativeint _ => K_naked_nativeint
  | Const_naked_vec128 _ => K_naked_vec128
  | Const_naked_vec256 _ => K_naked_vec256
  | Const_naked_vec512 _ => K_naked_vec512
  | Const_poison k _ => k
  end.

(* ENCODING NOTE: coercions are embedded in Simple_name (a simple is
   a name *at* a coercion, not a coercion wrapper around a simple),
   so the doc's "s @ co" is the same name carrying a different
   coercion.  The Var/Symbol base rules are stated at Coercion_id and
   WF.Kind.Coerce transports a kinding at Coercion_id to the same
   name at any coercion. *)
Inductive kinding (Gamma : kctx) : simple -> kind -> Prop :=

(** RULE WF.Kind.Var (STATUS normative) -- 03-kinds.md
    CODE term_basics/simple.ml#kind
    CODE identifiers/int_ids.ml#Variable.kind *)
| WF_Kind_Var : forall x k,
    Gamma x = Some k ->
    kinding Gamma (Simple_name (Name_var x) Coercion_id) k

(** RULE WF.Kind.Symbol (STATUS normative) -- 03-kinds.md
    CODE term_basics/simple.ml#kind
    Symbols name statically-allocated data and are always of kind
    Value (Symbol.t carries no kind field). *)
| WF_Kind_Symbol : forall s,
    kinding Gamma (Simple_name (Name_sym s) Coercion_id) K_value

(** RULE WF.Kind.Const (STATUS normative) -- 03-kinds.md
    CODE identifiers/reg_width_const.ml#kind
    CODE identifiers/int_ids.mli#Const.Descr
    Each Const case has a fixed kind (const_kind above, cases in the
    rule's order); Poison carries an explicit kind (any kind,
    including Region/Rec_info). *)
| WF_Kind_Const : forall c,
    kinding Gamma (Simple_const c) (const_kind c)

(** RULE WF.Kind.Coerce (STATUS normative) -- 03-kinds.md
    CODE identifiers/coercion0.mli#S
    CODE term_basics/simple.ml#kind
    A coercion does not alter the run-time value of its argument;
    Simple.kind discards the coercion, so kind is invariant under
    coercion. *)
| WF_Kind_Coerce : forall n co k,
    kinding Gamma (Simple_name n Coercion_id) k ->
    kinding Gamma (Simple_name n co) k.

(* ================================================================== *)
(* 4. Chapter 05/06 kind tables, abstracted (see header note)         *)
(* ================================================================== *)

(* Flambda_primitive.result_kind: Singleton kappa or Unit. *)
Inductive prim_result_kind :=
  PRK_singleton (k : kind)
| PRK_unit.

Section WithKindTables.

(* result_kind table (chapters 05/06 give the per-primitive rows). *)
Variable result_kind_table : prim_op -> prim_result_kind.

(* arg-kind tables: arg_kind_of_unary_primitive,
   args_kind_of_binary/ternary/quaternary/variadic_primitive.  A
   relation, since the variadic results (Variadic_all_of_kind,
   Variadic_zero_or_one, Variadic_mixed, Variadic_unboxed_product)
   admit several concrete kind lists. *)
Variable prim_arg_kinds : prim_op -> list kind -> Prop.

(* Code_metadata.params_arity / result_arity of a code id, read by
   the WF.Apply.* rules. *)
Variable code_params_arity : code_id -> option arity.
Variable code_result_arity : code_id -> option arity.

(* result_kind'(p), the where-clause of WF.Named.Prim: a
   Unit-returning primitive has result kind Value (it produces the
   unit immediate). *)
Definition result_kind' (op : prim_op) : kind :=
  match result_kind_table op with
  | PRK_singleton k => k
  | PRK_unit => K_value
  end.

(* ================================================================== *)
(* 5. Named kinding Gamma |- n : kappa (03-kinds.md section 5.1)      *)
(* ================================================================== *)

(* Named.kind is defined only for singleton-bindable defining
   expressions; there is deliberately no clause for
   N_set_of_closures or N_static_consts (fatal in the OCaml; they
   are bound by the other patterns -- WF.Let.SetOfClosures /
   WF.Let.Static below). *)
Inductive named_kinding (Gamma : kctx) : named -> kind -> Prop :=

(** RULE WF.Named.Simple (STATUS normative) -- 03-kinds.md
    CODE terms/flambda.ml#Named.kind *)
| WF_Named_Simple : forall s k,
    kinding Gamma s k ->
    named_kinding Gamma (N_simple s) k

(** RULE WF.Named.Prim (STATUS normative) -- 03-kinds.md
    CODE terms/flambda.ml#Named.kind
    CODE terms/flambda_primitive.mli#result_kind
    CODE terms/flambda_primitive.ml#result_kind'
    Named.kind(Prim (p, dbg)) = result_kind'(p); the per-primitive
    rows live in chapters 05/06 (result_kind_table above). *)
| WF_Named_Prim : forall p,
    named_kinding Gamma (N_prim p) (result_kind' (prim_op_of p))

(** RULE WF.Named.RecInfo (STATUS normative) -- 03-kinds.md
    CODE terms/flambda.ml#Named.kind *)
| WF_Named_RecInfo : forall ri,
    named_kinding Gamma (N_rec_info ri) K_rec_info.

(* ================================================================== *)
(* 6. Primitive argument kinds (03-kinds.md section 5.2)              *)
(* ================================================================== *)

(** RULE WF.Prim.ArgKinds (STATUS normative) -- 03-kinds.md
    CODE terms/flambda_primitive.mli#arg_kind_of_unary_primitive
    CODE terms/flambda_primitive.mli#args_kind_of_binary_primitive
    CODE terms/flambda_primitive.mli#args_kind_of_variadic_primitive
    p applied to s1 ... sn; the expected argument kinds of p are
    k1 ... kn (from the arg-kind tables); Gamma |- si : ki for each
    i.  Conclusion: the primitive application is kind-correct. *)
Definition WF_Prim_ArgKinds (Gamma : kctx) (p : prim) : Prop :=
  exists ks,
    prim_arg_kinds (prim_op_of p) ks
    /\ Forall2 (kinding Gamma) (prim_args p) ks.

(** RULE WF.Prim.MakeBlockMixed (STATUS normative) -- 03-kinds.md
    CODE terms/flambda_primitive.ml#args_kind_of_variadic_primitive
    CODE middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive
    CODE kinds/flambda_kind.mli#Mixed_block_shape.field_kinds
    VERIFIED 14-validation/mixed-01-record.md
    The specialisation of WF.Prim.ArgKinds for
    p = P_variadic (VP_make_block (BK_mixed t sigma) mu am) args,
    sigma = <p0, ebar>: Variadic_mixed sigma expands pointwise to
    field_kinds(sigma) (mixed_block_field_kinds in Base.v); n = p0 +
    |ebar| and each argument is checked at its position's kind.
    (mu and am are carried only to mirror the rule's p.) *)
Definition WF_Prim_MakeBlockMixed (Gamma : kctx)
    (t : tag) (sigma : mixed_block_shape) (mu : mutability)
    (am : alloc_mode_alloc) (args : list simple) : Prop :=
  length args
    = (value_prefix_size sigma + length (flat_suffix sigma))%nat
  /\ Forall2 (kinding Gamma) args (mixed_block_field_kinds sigma).

(* ================================================================== *)
(* 7. Switch non-emptiness (03-kinds.md section 5.3)                  *)
(* ================================================================== *)

(** RULE WF.Switch.NonEmpty (STATUS normative) -- 03-kinds.md
    CODE simplify/simplify_switch_expr.ml
    CODE simplify/expr_builder.ml
    CODE terms/flambda.mli#Invalid
    A Switch with zero arms is not well-formed (Simplify turns it
    into Invalid Zero_switch_arms).  ENCODING NOTE: the doc states
    this as a separate negative rule (Gamma |/- Switch ok); it is
    encoded as this named predicate, referenced as a premise of the
    (only) switch constructor WF_Switch_Scrutinee of expr_wf, which
    makes the non-derivability literal.  The stronger structural
    claim (>= 2 arms on surviving switches) is
    WF_Syntax_SwitchMinArms in Syntax.v. *)
Definition WF_Switch_NonEmpty (sw : switch_expr) : Prop :=
  sw_arms sw <> [].

(* ================================================================== *)
(* 8. Apply_cont (03-kinds.md section 5.4)                            *)
(* ================================================================== *)

(* Delta maps each in-scope continuation to its unarized parameter
   kinds (Bound_parameter.kind of each parameter). *)
Definition cctx := fmap continuation unarized_arity.

(** RULE WF.ApplyCont.Arity (STATUS normative) -- 03-kinds.md
    CODE terms/apply_cont_expr.mli#create
    CODE bound_identifiers/bound_parameters.mli#arity
    CODE bound_identifiers/bound_parameter.mli#kind
    CODE simplify/env/continuation_uses.ml#add_use
    CODE simplify/simplify_apply_cont_expr.ml#inline_linearly_used_continuation
    Apply_cont k s1 ... sn; k has parameters with kinds kw1 ... kwn;
    n = num params of k (after unarization); Gamma |- sj : kind(kwj)
    (ignoring subkinds) for each j.  Trap actions are covered by
    chapters 04 / 02. *)
Definition WF_ApplyCont_Arity (Gamma : kctx) (Delta : cctx)
    (ac : apply_cont_expr) : Prop :=
  exists ks,
    Delta (ac_continuation ac) = Some ks
    /\ length (ac_args ac) = length ks
    /\ Forall2 (fun s kw => kinding Gamma s (ws_kind kw))
         (ac_args ac) ks.

(* ================================================================== *)
(* 9. Apply (03-kinds.md section 5.5)                                 *)
(* ================================================================== *)

(* The direct-callee code id of an application, if any. *)
Definition apply_direct_code (ap : apply_expr) : option code_id :=
  match ap_call_kind ap with
  | CK_function (FC_direct cid) => Some cid
  | _ => None
  end.

(** RULE WF.Apply.ArgKinds (STATUS normative) -- 03-kinds.md
    CODE simplify/simplify_apply_expr.ml#simplify_apply_shared
    CODE terms/apply_expr.mli#args_arity
    apply with args s1 ... sm and args_arity a;
    unarize(a) = kw1 ... kwm; Gamma |- si : ki with kind(kwi) = ki
    for each i. *)
Definition WF_Apply_ArgKinds (Gamma : kctx) (ap : apply_expr)
    : Prop :=
  Forall2 (fun s kw => kinding Gamma s (ws_kind kw))
    (ap_args ap) (unarize (ap_args_arity ap)).

(* Componentwise subkind erasure (inner fixpoint for guardedness,
   as unarize_component in Base.v). *)
Fixpoint erase_component (c : arity_component) : arity_component :=
  match c with
  | Arity_singleton kw => Arity_singleton (erase_subkind kw)
  | Arity_unboxed_product cs =>
      Arity_unboxed_product
        ((fix go (l : list arity_component) : list arity_component :=
            match l with
            | [] => []
            | c' :: tl => erase_component c' :: go tl
            end) cs)
  end.

(* "agrees on their common prefix": the shorter arity against the
   equal-length prefix of the other, ignoring subkinds. *)
Definition arity_agree_prefix (a b : arity) : Prop :=
  let n := Nat.min (length a) (length b) in
  map erase_component (firstn n a) = map erase_component (firstn n b).

(** RULE WF.Apply.DirectArity (STATUS normative) -- 03-kinds.md
    CODE simplify/simplify_apply_expr.ml#arity_mismatch
    CODE terms/flambda.mli#Invalid
    Direct call of code cid with params_arity from its code
    metadata: the apply's args_arity agrees with params_arity on
    their common prefix (partial and over-application prefixes
    allowed). *)
Definition WF_Apply_DirectArity (ap : apply_expr) : Prop :=
  forall cid,
    apply_direct_code ap = Some cid ->
    exists pa,
      code_params_arity cid = Some pa
      /\ arity_agree_prefix (ap_args_arity ap) pa.

(** RULE WF.Apply.DirectResultArity (STATUS normative) -- 03-kinds.md
    CODE simplify/simplify_apply_expr.ml
    CODE terms/flambda.mli#Invalid
    Exact direct call (provided_num_args = num_params, i.e. equal
    top-level component counts): result_arity from the code metadata
    is equal_ignoring_subkinds to the apply's return_arity.  Checked
    only for exact applications; partial/over cases are
    WF.Apply.Partial / WF.Apply.Over. *)
Definition WF_Apply_DirectResultArity (ap : apply_expr) : Prop :=
  forall cid pa,
    apply_direct_code ap = Some cid ->
    code_params_arity cid = Some pa ->
    length (ap_args_arity ap) = length pa ->
    exists ra,
      code_result_arity cid = Some ra
      /\ equal_ignoring_subkinds (unarize ra)
           (unarize (ap_return_arity ap)).

(* "a single Value": one register-width result of kind Value. *)
Definition arity_is_one_value (a : arity) : Prop :=
  map ws_kind (unarize a) = [K_value].

(** RULE WF.Apply.Over (STATUS normative) -- 03-kinds.md
    CODE simplify/simplify_apply_expr.ml
    CODE simplify/simplify_common.ml
    CODE terms/flambda.mli#Invalid
    Direct call with provided_num_args > num_params:
    over-application is well-formed; the overapplied function's
    return arity must be a single Value
    (is_one_param_of_kind_value).  ENCODING NOTE: the conclusion's
    compilation scheme (full application whose result is applied to
    the remaining arguments) is prose about ch. 16's translation;
    the modeled content is the return-arity constraint. *)
Definition WF_Apply_Over (ap : apply_expr) : Prop :=
  forall cid pa,
    apply_direct_code ap = Some cid ->
    code_params_arity cid = Some pa ->
    (length pa < length (ap_args_arity ap))%nat ->
    exists ra,
      code_result_arity cid = Some ra /\ arity_is_one_value ra.

(** RULE WF.Apply.Partial (STATUS normative) -- 03-kinds.md
    CODE simplify/simplify_apply_expr.ml
    CODE terms/flambda.mli#Invalid
    Direct call with 0 < provided_num_args < num_params: partial
    application is well-formed; the apply's return arity must be a
    single Value.  ENCODING NOTE: as for WF.Apply.Over, the
    closure-capturing compilation scheme is prose; the mode-mismatch
    Invalids are named by WF.Check.Gated's consequence list. *)
Definition WF_Apply_Partial (ap : apply_expr) : Prop :=
  forall cid pa,
    apply_direct_code ap = Some cid ->
    code_params_arity cid = Some pa ->
    (0 < length (ap_args_arity ap))%nat ->
    (length (ap_args_arity ap) < length pa)%nat ->
    arity_is_one_value (ap_return_arity ap).

(* ================================================================== *)
(* 10. Regions and recursion depths (03-kinds.md section 5.6)         *)
(* ================================================================== *)

(** RULE WF.Region.Var (STATUS normative) -- 03-kinds.md
    CODE from_lambda/closure_conversion.ml
    CODE terms/flambda_primitive.ml#result_kind_of_variadic_primitive
    Region tokens are of kind Region.  This predicate is the
    per-variable statement; its intended instantiations are the
    region-introducing variables (my_region, over_app_region, the
    unit's toplevel regions, Alloc_local/App_local payloads).  The
    Begin/End region result/argument kinds are rows of the ch. 06
    tables (result_kind_table / prim_arg_kinds). *)
Definition WF_Region_Var (Gamma : kctx) (x : variable) : Prop :=
  Gamma x = Some K_region.

(** RULE WF.RecInfo.MyDepth (STATUS normative) -- 03-kinds.md
    CODE simplify/simplify_apply_expr.ml
    CODE simplify/simplify_set_of_closures.ml
    CODE terms/flambda.mli#Function_params_and_body.create
    The my_depth variable bound by a function body has kind
    Rec_info. *)
Definition WF_RecInfo_MyDepth (Gamma : kctx) (c : code0) : Prop :=
  Gamma (c0_my_depth c) = Some K_rec_info.

(* ================================================================== *)
(* 11. The judgment Gamma |- e ok (03-kinds.md section 5)             *)
(* ================================================================== *)

(* "bound_static and g have matching shapes (symbols for data, code
   ids for code)" -- WF.Let.Static's shape premise. *)
Definition bsp_matches_scc (bsp : bound_static_pattern)
    (scc : static_const_or_code) : Prop :=
  match bsp, scc with
  | BSP_code _, SCC_code _ => True
  | BSP_code _, SCC_deleted_code => True
  | BSP_set_of_closures _, SCC_static_const (SC_set_of_closures _) =>
      True
  | BSP_block_like _, SCC_static_const sc =>
      match sc with
      | SC_set_of_closures _ => False
      | _ => True
      end
  | _, _ => False
  end.

(* ENCODING NOTE: ch. 03 section 5 gives rules only for the forms
   below (Let bindings, Switch, Apply_cont); the judgment is
   declarative and the doc does not state congruence rules for
   Let_cont, Apply or Invalid.  The wf_expr_* constructors carrying
   no rule ids are plumbing that closes the judgment over those
   forms: Let_cont extends Delta with the handlers' unarized
   parameter kinds (invariant params prepended for recursive
   groups) and Gamma with the parameters over each handler body;
   Apply conjoins the named WF.Apply.* / WF.Arity.ApplyFlavours
   predicates; Invalid is vacuously ok. *)
Inductive expr_wf : kctx -> cctx -> expr -> Prop :=

(** RULE WF.Let.Singleton (STATUS normative) -- 03-kinds.md
    CODE terms/flambda.ml#Named.kind
    CODE terms/flambda.mli#Named.kind
    CODE bound_identifiers/bound_pattern.mli#t
    Gamma |- n : kn (Named.kind); pattern = Singleton x with
    Gamma(x) = kx; kx = kn; Gamma, x:kn |- body ok. *)
| WF_Let_Singleton : forall Gamma Delta bv n kn kx body,
    named_kinding Gamma n kn ->
    Gamma (bv_var bv) = Some kx ->
    kx = kn ->
    expr_wf (fupd variable_eqb Gamma (bv_var bv) kn) Delta body ->
    expr_wf Gamma Delta (E_let (BPat_singleton bv) n body)

(** RULE WF.Let.SetOfClosures (STATUS normative) -- 03-kinds.md
    CODE terms/flambda.mli#Named
    CODE bound_identifiers/bound_pattern.mli#t
    CODE simplify/simplify_set_of_closures.ml
    n = Set_of_closures _; pattern = Set_of_closures [x1 ... xn]
    with Gamma(xi) = Value for each i; Gamma, xbar:Value |- body
    ok.  Each closure variable is of kind Value (a closure is an
    OCaml heap value). *)
| WF_Let_SetOfClosures : forall Gamma Delta bvs soc am body,
    Forall (fun bv => Gamma (bv_var bv) = Some K_value) bvs ->
    expr_wf
      (fupd_list variable_eqb Gamma
         (map (fun bv => (bv_var bv, K_value)) bvs))
      Delta body ->
    expr_wf Gamma Delta
      (E_let (BPat_set_of_closures bvs) (N_set_of_closures soc am)
         body)

(** RULE WF.Let.Static (STATUS normative) -- 03-kinds.md
    CODE terms/flambda.mli#Named
    CODE bound_identifiers/bound_pattern.mli#t
    n = Static_consts g; pattern = Static bound_static; matching
    shapes; body ok under the extended context.  ENCODING NOTE: the
    doc extends Gamma with the bound symbols (each : Value) and code
    ids, but Gamma here is variable-keyed and symbols are
    unconditionally of kind Value (WF.Kind.Symbol), so the extension
    is a no-op in this encoding; code ids are not kinded. *)
| WF_Let_Static : forall Gamma Delta bst scg body,
    Forall2 bsp_matches_scc bst scg ->
    expr_wf Gamma Delta body ->
    expr_wf Gamma Delta
      (E_let (BPat_static bst) (N_static_consts scg) body)

(** RULE WF.Switch.Scrutinee (STATUS normative) -- 03-kinds.md
    CODE terms/switch_expr.ml#t
    CODE simplify/simplify_switch_expr.ml#simplify_arm
    Gamma |- scrutinee : Naked_immediate (a Switch scrutinee is a
    naked immediate, untagged before the switch); each arm action ok
    (WF.ApplyCont.Arity).  The final premise references
    WF_Switch_NonEmpty (see that rule's ENCODING NOTE).  The
    structural form of the scrutinee constraint is
    WF_Syntax_SwitchScrutinee in Syntax.v. *)
| WF_Switch_Scrutinee : forall Gamma Delta sw,
    kinding Gamma (sw_scrutinee sw) K_naked_immediate ->
    Forall (fun arm => WF_ApplyCont_Arity Gamma Delta (snd arm))
      (sw_arms sw) ->
    WF_Switch_NonEmpty sw ->
    expr_wf Gamma Delta (E_switch sw)

(* ------ plumbing constructors (no rule ids; see note above) ------ *)

| wf_expr_apply_cont : forall Gamma Delta ac,
    WF_ApplyCont_Arity Gamma Delta ac ->
    expr_wf Gamma Delta (E_apply_cont ac)

| wf_expr_apply : forall Gamma Delta ap,
    WF_Apply_ArgKinds Gamma ap ->
    WF_Apply_DirectArity ap ->
    WF_Apply_DirectResultArity ap ->
    WF_Apply_Over ap ->
    WF_Apply_Partial ap ->
    WF_Arity_ApplyFlavours ap ->
    expr_wf Gamma Delta (E_apply ap)

| wf_expr_let_cont_nonrec : forall Gamma Delta k h body,
    expr_wf
      (fupd_list variable_eqb Gamma
         (map (fun p => (fst p, ws_kind (snd p))) (ch_params h)))
      Delta (ch_handler h) ->
    expr_wf Gamma
      (fupd continuation_eqb Delta k (map snd (ch_params h)))
      body ->
    expr_wf Gamma Delta (E_let_cont_nonrec k h body)

| wf_expr_let_cont_rec : forall Gamma Delta inv handlers body,
    (forall k h,
        In (k, h) handlers ->
        expr_wf
          (fupd_list variable_eqb Gamma
             (map (fun p => (fst p, ws_kind (snd p)))
                (inv ++ ch_params h)))
          (fupd_list continuation_eqb Delta
             (map (fun kh =>
                     (fst kh, map snd (inv ++ ch_params (snd kh))))
                handlers))
          (ch_handler h)) ->
    expr_wf Gamma
      (fupd_list continuation_eqb Delta
         (map (fun kh =>
                 (fst kh, map snd (inv ++ ch_params (snd kh))))
            handlers))
      body ->
    expr_wf Gamma Delta (E_let_cont_rec inv handlers body)

| wf_expr_invalid : forall Gamma Delta msg,
    expr_wf Gamma Delta (E_invalid msg).

End WithKindTables.

(* ================================================================== *)
(* 12. Where kinds are checked in practice (03-kinds.md section 6)    *)
(* ================================================================== *)

(** RULE WF.Check.Gated (STATUS descriptive) -- 03-kinds.md
    CODE ui/flambda_features.ml#kind_checks
    CODE driver/oxcaml_flags.ml
    CODE driver/oxcaml_args.ml
    CODE simplify/simplify_apply_expr.ml#simplify_apply_shared
    Kind checking is controlled by -flambda2-kind-checks (default
    off: the checks are conservative and can reject legitimate
    layout-polymorphic code).  When on, a kind/arity mismatch at an
    Apply is fatal; when off, the mismatch rewrites the Apply to an
    Invalid node.  Flambda 2 has no standalone kind-checking pass:
    the Let / Apply_cont / Switch-scrutinee agreements are
    maintained by construction, while the Apply and
    primitive-argument agreements are re-checked during Simplify
    through this gate. *)
Definition WF_Check_Gated_documented : Prop := True.

(* ================================================================== *)
(* 13. code0 binder discipline (encoding support)                     *)
(* ================================================================== *)

(* code0_wf: the binder disciplines the doc assumes by alpha-fiat for
   every code item and never states as rules (Knuth's W-23, from the
   ch. 11 inlining review).  On paper, alpha-convention lets a code
   item's binders be read as fresh and pairwise distinct; mechanized
   binders are concrete names, so the discipline is assumed
   explicitly -- solely by ch. 13's code-tying WF hypothesis
   (coordinator ruling: no rewrite-rule use-site premises):
   - the return and exn continuations are distinct;
   - parameter names are distinct;
   - the body's free continuations are at most the return and exn
     continuations (free_conts, Syntax.v);
   - the region binders are distinct, stated on the alloc-mode
     field, where code0 carries the region names (KF-023: code0
     elides the region binders as fields, but a Local my_alloc_mode
     names both, and with r = g a Local-to-Local region_rename
     (Inlining.v) would send the callee's ghost-region uses to the
     caller's ordinary region).
   No rule id: encoding-support predicate, not a documented
   rule. *)
Definition code0_wf (c : code0) : Prop :=
  c0_return_continuation c <> c0_exn_continuation c
  /\ NoDup (map fst (c0_params c))
  /\ (forall k,
        free_conts (c0_body c) k ->
        k = c0_return_continuation c \/ k = c0_exn_continuation c)
  /\ match c0_my_alloc_mode c with
     | App_local r g => r <> g
     | _ => True
     end.
