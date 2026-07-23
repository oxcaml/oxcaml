(* RewritesPrim.v -- 10-simplify-rewrites.md, prim-side rewrites
   (owner: Curry).

   The prim-side half of ch. 10: the rw_prim relation over eff_flags,
   code_env and tenv covering primitive simplification (Transfer /
   ArgKindMismatch / Relational), constant folding (+ Float /
   PartialUndef), reification, the algebraic identities (IntIdentity /
   FloatIdentity / UntagTag / Projection / PhysEqual / ObjDupElide),
   alias canonicalization (rw_simple), and the CSE family (Eligible /
   Replace / Extend) plus the Share.StaticDynamicSplit invariant --
   19 rules.  The control-side half (the Switch, Let, LetCont,
   Apply, Loopify and Invalid rule families) is RewritesControl.v
   (owner: Church).

   Frozen interface (main-approved, fifth amendment + pruning
   correction): rw_prim : eff_flags -> code_env -> tenv -> expr ->
   expr -> Prop, with flags and C bound as DIRECT inductive
   parameters (a Section Variable no constructor reads would be
   pruned and close the type at four arguments; see the DRAFT NOTE
   in Simplify.v).  Ambient-flags semantics per main's ruling: the
   relation is parameterized by the flag setting the pass runs
   under; the float folds are gated on ef_float_const_prop.

   Const-fold premises invoke the actual denotations through
   Machine.v's denot_prim (the union of denot_scalar / denot_mem_a /
   denot_mem_b), keying the soundness coupling to chapters 05-06. *)

(* String before List: List's length must win (Hopper). *)
From Stdlib Require Import ZArith Bool String List.
From Flambda2 Require Import Base Syntax Values Opsem TypeGrammar
  PrimScalar PrimMemoryA PrimMemoryB Machine MeetJoin.
Import ListNotations.

Local Open Scope Z_scope.

(* ================================================================== *)
(* 1. The abstract transfer function (sanctioned oracles)             *)
(* ================================================================== *)

(* [[p]]#(Tbar) = T |> eps: the abstract counterpart of the primitive
   denotation -- given the types of p's arguments it returns a type
   for the result and an environment extension of relational facts.
   Ch. 10 defines it only through the rewrites that consult it (the
   per-primitive computations live in the simplify_*_primitive.ml
   files); it is on the sanctioned oracle list (CORRESPONDENCE.md
   encoding table, "oracles" row). *)
Parameter prim_transfer
  : prim_op -> list ftype -> ftype -> env_extension -> Prop.

(* The expected-argument-kind table of ch. 03 (WF.Prim.ArgKinds;
   arg_kind_of_unary_primitive and friends).  ENCODING NOTE:
   WellFormed.v defers this same table as a Section Variable; here it
   must be a top-level Parameter because rw_prim's frozen closed type
   cannot carry it as an argument.  A relation, not a function, for
   the same reason as in WellFormed.v (the variadic rows admit
   several concrete kind lists).  Sanctioned by main alongside
   prim_transfer (CORRESPONDENCE.md entry 56); rule premises quote
   it (S.Rewrite.Prim.Transfer's kind check). *)
Parameter prim_arg_kinds : prim_op -> list kind -> Prop.

(* ================================================================== *)
(* 2. Helpers                                                         *)
(* ================================================================== *)

(* Rebuild a primitive with its arguments transformed. *)
Definition map_prim_args (f : simple -> simple) (p : prim) : prim :=
  match p with
  | P_nullary op => P_nullary op
  | P_unary op s => P_unary op (f s)
  | P_binary op s1 s2 => P_binary op (f s1) (f s2)
  | P_ternary op s1 s2 s3 => P_ternary op (f s1) (f s2) (f s3)
  | P_quaternary op s1 s2 s3 s4 =>
      P_quaternary op (f s1) (f s2) (f s3) (f s4)
  | P_variadic op args => P_variadic op (map f args)
  end.

(* Argument canonicalization, applied by every rewrite before
   anything else looks at a Simple (ch. 10, "Canonicalization is
   pervasive"; the per-Simple statement is rw_simple below). *)
Definition canonicalize_prim (E : tenv) (p : prim) : prim :=
  map_prim_args (canonical E) p.

(* The kind a constant carries (int_ids / reg_width_const). *)
Definition const_kind (c : const) : kind :=
  match c with
  | Const_naked_immediate _ => K_naked_immediate
  | Const_tagged_immediate _ => K_value
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
  | Const_null => K_value
  | Const_poison k _ => k
  end.

(* The intrinsic kind of s's stored type in E: a constant carries its
   own kind; a name's kind is that of its stored entry's type.  None
   when E has no entry for the name (the kind is then unknowable
   in-model). *)
Definition simple_kind_in (E : tenv) (s : simple) : option kind :=
  match s with
  | Simple_const c => Some (const_kind c)
  | Simple_name n _ =>
      match te_types E n with
      | Some ne => Some (kind_of_ftype (ne_type ne))
      | None => None
      end
  end.

(* Kind and constant construction per standard_int (the payload kind
   of the integer arithmetic/shift primitives). *)
Definition kind_si (k : standard_int) : kind :=
  match k with
  | SI_tagged_immediate => K_value
  | SI_naked_immediate => K_naked_immediate
  | SI_naked_int8 => K_naked_int8
  | SI_naked_int16 => K_naked_int16
  | SI_naked_int32 => K_naked_int32
  | SI_naked_int64 => K_naked_int64
  | SI_naked_nativeint => K_naked_nativeint
  end.

Definition const_si (k : standard_int) (i : Z) : const :=
  match k with
  | SI_tagged_immediate => Const_tagged_immediate i
  | SI_naked_immediate => Const_naked_immediate i
  | SI_naked_int8 => Const_naked_int8 i
  | SI_naked_int16 => Const_naked_int16 i
  | SI_naked_int32 => Const_naked_int32 i
  | SI_naked_int64 => Const_naked_int64 i
  | SI_naked_nativeint => Const_naked_nativeint i
  end.

Definition kind_fbw (w : float_bitwidth) : kind :=
  match w with
  | FBW_float64 => K_naked_float
  | FBW_float32 => K_naked_float32
  end.

(* Float arithmetic operators (the family the propagating_float_consts
   flag gates; comparisons return integers and are not in it). *)
Definition is_float_arith (op : prim_op) : bool :=
  match op with
  | Op_unary (UP_float_arith _ _) => true
  | Op_binary (BP_float_arith _ _) => true
  | _ => false
  end.

(* "the prover proves s equals the constant c": stated through the
   on-disk equals-to-simple prover (MeetJoin.v) applied to s's
   in-term type (the alias_type_of view of T.Env.Find.Canonical).
   ENCODING NOTE: the implementation consults the per-kind meet_*
   shortcuts (Known_result flavour: meet_naked_immediates,
   meet_equals_tagged_immediates, ...); on the alias view both roads
   reduce to "s's canonical is the constant c", which is what the
   equals prover derives.  The two on-disk meet shortcuts witness
   the per-kind family. *)
Definition proven_const (E : tenv) (k : kind) (s : simple) (c : const)
  : Prop :=
  prove_equals_to_simple_of_kind E k
    (fst (type_simple_in_term E k s)) (Proved (Simple_const c)).

(* The environment extension recording an inverse relation on a
   result variable r: r gets the Naked_immediate type "= ir(n)"
   (T.Grammar.NakedNumber.Immediate's Inverse_relations form). *)
Definition irel_ext (r : variable) (ir : irelation) (n : name)
  : env_extension :=
  fun n' =>
    if name_eqb n' (Name_var r)
    then Some (FT_naked_immediate (Oub_ok (No_alias
           (Inverse_relations
              (fun ir' =>
                 if irelation_eqb ir' ir then Some [n] else None)))))
    else None.

(* View of a boxed-number head: which boxable kind, the payload type,
   and the allocation mode. *)
Definition boxed_payload_view (h : head_value_non_null)
  : option (boxable_number * ftype * alloc_mode_types) :=
  match h with
  | HV_boxed_float32 t am => Some (BN_naked_float32, t, am)
  | HV_boxed_float t am => Some (BN_naked_float, t, am)
  | HV_boxed_int32 t am => Some (BN_naked_int32, t, am)
  | HV_boxed_int64 t am => Some (BN_naked_int64, t, am)
  | HV_boxed_nativeint t am => Some (BN_naked_nativeint, t, am)
  | HV_boxed_vec128 t am => Some (BN_naked_vec128, t, am)
  | HV_boxed_vec256 t am => Some (BN_naked_vec256, t, am)
  | HV_boxed_vec512 t am => Some (BN_naked_vec512, t, am)
  | _ => None
  end.

(* ================================================================== *)
(* 3. The integer/float identity tables                               *)
(* ================================================================== *)

(* Which operand of a binary primitive is the proven-constant one. *)
Inductive operand_side := Side_left | Side_right.

(* Outcome column of the S.Rewrite.Prim.IntIdentity table. *)
Inductive int_id_result :=
  | IIR_other            (* the other operand *)
  | IIR_const (i : Z)    (* the constant i *)
  | IIR_neg.             (* Sub (0, other operand) *)

(* The arithmetic rows of the table (the named constant is the KNOWN
   operand; symmetric operations admit either side, sub/div/mod only
   the stated side).  Realized by The_other_side / Exactly /
   Negation_of_the_other_side in op_lhs_unknown / op_rhs_unknown /
   symmetric_op_one_side_unknown. *)
Inductive int_arith_identity
  : binary_int_arith_op -> operand_side -> Z -> int_id_result -> Prop :=
  | IAI_add_0 : forall sd, int_arith_identity BIA_add sd 0 IIR_other
  | IAI_sub_r0 : int_arith_identity BIA_sub Side_right 0 IIR_other
  | IAI_sub_l0 : int_arith_identity BIA_sub Side_left 0 IIR_neg
  | IAI_or_0 : forall sd, int_arith_identity BIA_or sd 0 IIR_other
  | IAI_or_m1 : forall sd,
      int_arith_identity BIA_or sd (-1) (IIR_const (-1))
  | IAI_xor_0 : forall sd, int_arith_identity BIA_xor sd 0 IIR_other
  | IAI_mul_1 : forall sd, int_arith_identity BIA_mul sd 1 IIR_other
  | IAI_mul_0 : forall sd,
      int_arith_identity BIA_mul sd 0 (IIR_const 0)
  | IAI_mul_m1 : forall sd, int_arith_identity BIA_mul sd (-1) IIR_neg
  | IAI_and_m1 : forall sd,
      int_arith_identity BIA_and sd (-1) IIR_other
  | IAI_and_0 : forall sd,
      int_arith_identity BIA_and sd 0 (IIR_const 0)
  | IAI_div_1 : int_arith_identity BIA_div Side_right 1 IIR_other
  | IAI_div_m1 : int_arith_identity BIA_div Side_right (-1) IIR_neg
  | IAI_mod_1 :
      int_arith_identity BIA_mod Side_right 1 (IIR_const 0)
  | IAI_mod_m1 :
      int_arith_identity BIA_mod Side_right (-1) (IIR_const 0).

(* The shift rows: the left operand is the value, the right the shift
   amount.  Out-of-range shift amounts are NOT simplified (ch. 10
   NOTES; they cannot be made Invalid, the IR being type-safe). *)
Inductive int_shift_identity
  : int_shift_op -> operand_side -> Z -> int_id_result -> Prop :=
  | ISI_amount_0 : forall op,
      int_shift_identity op Side_right 0 IIR_other
  | ISI_value_0 : forall op,
      int_shift_identity op Side_left 0 (IIR_const 0)
  | ISI_asr_m1 :
      int_shift_identity IS_asr Side_left (-1) (IIR_const (-1)).

(* The replacement named per outcome. *)
Definition int_id_named (k : standard_int) (other : simple)
  (res : int_id_result) : named :=
  match res with
  | IIR_other => N_simple other
  | IIR_const i => N_simple (Simple_const (const_si k i))
  | IIR_neg =>
      N_prim (P_binary (BP_int_arith k BIA_sub)
        (Simple_const (const_si k 0)) other)
  end.

(* The float constants of S.Rewrite.Prim.FloatIdentity: the
   bit-pattern-preserving neutral elements +-1.0 at each bitwidth
   (the bool is "negated": true selects the Neg(other) column). *)
Inductive float_pm_one : float_bitwidth -> const -> bool -> Prop :=
  | FPO_f64_pos :
      float_pm_one FBW_float64 (Const_naked_float (f64_of_Z 1)) false
  | FPO_f64_neg :
      float_pm_one FBW_float64
        (Const_naked_float (f64_of_Z (-1))) true
  | FPO_f32_pos :
      float_pm_one FBW_float32
        (Const_naked_float32 (f32_of_f64 (f64_of_Z 1))) false
  | FPO_f32_neg :
      float_pm_one FBW_float32
        (Const_naked_float32 (f32_of_f64 (f64_of_Z (-1)))) true.

(* ================================================================== *)
(* 4. Alias canonicalization                                          *)
(* ================================================================== *)

(** RULE S.Rewrite.Alias.Canonicalize (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_simple.ml#simplify_simple0
    CODE middle_end/flambda2/types/env/typing_env.ml#type_simple_in_term_exn *)
(* Premises in doc order: s occurs at name mode >= m; E gives s the
   type T and canonical simple s_can at mode m.  Conclusion: s ~>
   s_can.  ENCODING NOTE: the name-mode floor on canonical lookups
   and the re-simplification/composition of a carried coercion are
   not modeled (te_canonical is the unconstrained, coercion-less
   canonical map; T.Env.Find.Canonical's note).  Applied to every
   Simple in every rewrite: the rw_prim rules below take the
   canonical image directly (canonicalize_prim / canonical E), as
   rw_control does. *)
Inductive rw_simple (E : tenv) : simple -> simple -> Prop :=
  | S_Rewrite_Alias_Canonicalize : forall s k T c,
      type_simple_in_term E k s = (T, c) ->
      rw_simple E s c.

(* ================================================================== *)
(* 5. Common subexpression elimination                                *)
(* ================================================================== *)

(* The per-primitive CSE predicate (the shape clause of
   S.Rewrite.CSE.Eligible): transcribed on the families the doc NOTES
   enumerate as INCLUDED -- immutable and immutable_unique
   Array_load, the header-read family (Is_int, Is_null, Get_tag,
   Get_header, Array_length, String_length, Bigarray_get_alignment),
   arithmetic/comparison primitives, and construction of immutable
   blocks.  Primitives the NOTES exclude (Block_load, Project_*,
   Duplicate_*, mutable or raw loads including the String case of
   String_or_bigstring_load, Bigarray_load) and primitives the NOTES
   do not name have no constructor: the envelope claims nothing for
   them, and the exclusions are provable by inversion
   (S.Rewrite.Share.StaticDynamicSplit below). *)
Inductive cse_prim_pred : prim_op -> Prop :=
  | CPP_array_load_imm : forall ak alk mu,
      mu = Immutable \/ mu = Immutable_unique ->
      cse_prim_pred (Op_binary (BP_array_load ak alk mu))
  | CPP_is_int : forall vo, cse_prim_pred (Op_unary (UP_is_int vo))
  | CPP_is_null : cse_prim_pred (Op_unary UP_is_null)
  | CPP_get_tag : cse_prim_pred (Op_unary UP_get_tag)
  | CPP_get_header : cse_prim_pred (Op_unary UP_get_header)
  | CPP_array_length : forall akl,
      cse_prim_pred (Op_unary (UP_array_length akl))
  | CPP_string_length : forall sb,
      cse_prim_pred (Op_unary (UP_string_length sb))
  | CPP_bigarray_get_alignment : forall al,
      cse_prim_pred (Op_binary (BP_bigarray_get_alignment al))
  | CPP_unary_int_arith : forall k op,
      cse_prim_pred (Op_unary (UP_int_arith k op))
  | CPP_unary_float_arith : forall w op,
      cse_prim_pred (Op_unary (UP_float_arith w op))
  | CPP_boolean_not : cse_prim_pred (Op_unary UP_boolean_not)
  | CPP_int_arith : forall k op,
      cse_prim_pred (Op_binary (BP_int_arith k op))
  | CPP_int_shift : forall k op,
      cse_prim_pred (Op_binary (BP_int_shift k op))
  | CPP_int_comp : forall k cb,
      cse_prim_pred (Op_binary (BP_int_comp k cb))
  | CPP_float_arith : forall w op,
      cse_prim_pred (Op_binary (BP_float_arith w op))
  | CPP_float_comp : forall w cb,
      cse_prim_pred (Op_binary (BP_float_comp w cb))
  | CPP_phys_equal : forall ec,
      cse_prim_pred (Op_binary (BP_phys_equal ec))
  | CPP_make_block_immutable : forall bk am,
      cse_prim_pred (Op_variadic (VP_make_block bk Immutable am)).

(** RULE S.Rewrite.CSE.Eligible (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/terms/flambda_primitive.ml#Eligible_for_cse.create
    CODE middle_end/flambda2/terms/flambda_primitive.ml#unary_primitive_eligible_for_cse
    CODE middle_end/flambda2/terms/flambda_primitive.ml#binary_primitive_eligible_for_cse
    VERIFIED 14-validation/cse_immutable_array_load_var_index.md
    VERIFIED 14-validation/cse_immutable_array_load.md *)
(* Premises in doc order: the per-primitive eligibility predicate
   holds; at least one argument is a variable; the effects/coeffects
   pair is (No_effects, No_coeffects) or (Only_generative_effects
   Immutable, No_coeffects).  No rewrite by itself: this is the side
   condition for S.Rewrite.CSE.Replace / S.Rewrite.CSE.Extend.
   FINDING #16 (doc-level design conflict; RESOLVED 2026-07-22 as
   13 s4 item 8 -- entry 75): the immutable-block-construction arm
   (CPP_make_block_immutable, via the Only_generative_effects
   Immutable clause) licenses sharing two distinct allocations, and
   P.Binary.PhysEqual (PrimMemoryB.v) is now RELATIONAL on exactly
   that iota-class, with INV.Simplify.Preserves restated as
   refinement (Soundness.v): the former in-model refutation
   composition -- a context applying phys_equal to both copies and
   observing the merge -- is now a licensed RESOLUTION of the loose
   result.  Both rules remain transcribed faithfully; nothing is
   narrowed. *)
Definition cse_eligible (fl : eff_flags) (p : prim) : Prop :=
  cse_prim_pred (prim_op_of p)
  /\ (exists x co, In (Simple_name (Name_var x) co) (prim_args p))
  /\ (let e := effects_of fl (prim_op_of p) in
      (ece_effects e = No_effects /\ ece_coeffects e = No_coeffects)
      \/ (ece_effects e = Only_generative_effects Immutable
          /\ ece_coeffects e = No_coeffects)).

(* The CSE table: prior primitive applications (with canonicalized
   arguments) mapped to the simple they were bound to.  ENCODING
   NOTE: in the pass this is denv state
   (common_subexpression_elimination.ml, scoped by_scope/combined so
   equations survive join points only where valid in every
   predecessor -- that scoping is dacc machinery outside the model).
   rw_prim's frozen closed type cannot carry the table, so the two
   table-consuming/producing rules live in the side judgments
   rw_cse / cse_extend below, with the table an explicit argument;
   like rw_code_loopify / rw_self_tail_call (RewritesControl.v) they
   are not arms of Simplify.v's rewrites union; Simplify.v consumes
   them via its cse_deep traversal (the S_cse closure step, which
   threads the table down from fempty). *)
Definition cse_table : Type := fmap prim simple.

(** RULE S.Rewrite.CSE.Replace (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_primitive.ml#try_cse
    CODE middle_end/flambda2/simplify/simplify_primitive.ml#apply_cse
    VERIFIED 14-validation/cse_immutable_array_load.md
    VERIFIED 14-validation/cse_immutable_array_load_var_index.md
    VERIFIED 14-validation/issue5721.md
    VERIFIED 14-validation/new-04-cse.md *)
(* Premises in doc order: p eligible; min name mode = normal (the
   NM_normal binding below); the CSE table maps p with canonicalized
   arguments to a simple s in scope.  Conclusion: the binding's
   defining expression becomes an alias to s ("with x : alias-of s"
   is dacc type recording, in prose).  Reuse is sound per the
   revised NOTES (item 8): p is pure, or an immutable allocation
   where sharing changes only the physical identity of an immutable
   value -- identity MAY be observed through phys_equal; the change
   is licensed because (==) on non-mutable values is
   implementation-dependent, and P.Binary.PhysEqual together with
   INV.Simplify.Preserves' refinement reading grants exactly this
   license.  CSE runs BEFORE the per-primitive analysis in
   simplify_primitive. *)
Inductive rw_cse (fl : eff_flags) (C : code_env) (tbl : cse_table)
    (E : tenv) : expr -> expr -> Prop :=
  | S_Rewrite_CSE_Replace : forall x p s body,
      cse_eligible fl p ->
      tbl (canonicalize_prim E p) = Some s ->
      (forall n, simple_name s = Some n -> name_bound_in E n) ->
      rw_cse fl C tbl E
        (E_let (BPat_singleton (Mk_bound_var x NM_normal))
           (N_prim p) body)
        (E_let (BPat_singleton (Mk_bound_var x NM_normal))
           (N_simple s) body).

(** RULE S.Rewrite.CSE.Extend (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_primitive.ml#try_cse
    CODE middle_end/flambda2/simplify/common_subexpression_elimination.ml#T0.add
    VERIFIED 14-validation/new-04-cse.md *)
(* Premises in doc order: p eligible; no prior equal application in
   scope.  Conclusion: record p |-> x in the CSE table, keep the
   binding (no source-to-source change: this judgment relates the
   table before and after, the bookkeeping half of
   S.Rewrite.CSE.Replace).  The table update is stated pointwise to
   avoid needing decidable equality on prim. *)
Inductive cse_extend (fl : eff_flags) (E : tenv)
    : cse_table -> expr -> cse_table -> Prop :=
  | S_Rewrite_CSE_Extend : forall tbl tbl' x m p body,
      cse_eligible fl p ->
      tbl (canonicalize_prim E p) = None ->
      tbl' (canonicalize_prim E p)
        = Some (simple_of_name (Name_var x)) ->
      (forall q, q <> canonicalize_prim E p -> tbl' q = tbl q) ->
      cse_extend fl E tbl
        (E_let (BPat_singleton (Mk_bound_var x m)) (N_prim p) body)
        tbl'.

(* ================================================================== *)
(* 6. The prim-side rewrite relation                                  *)
(* ================================================================== *)

(* fl; C; E |- e ~> e'.  One constructor per rule (or per stated
   case), premises in doc order.  flags and C are direct inductive
   parameters (pruning-proof; see the header comment); C is unread
   by the prim-side rules but keeps the closed type at Simplify.v's
   frozen five arguments.  The conclusions' "with x : T" / "E
   extended by eps" clauses are dacc type recording performed by the
   ch. 09 traversal; the T and eps concerned appear in premises so
   the recorded objects are pinned, but the source-to-source relation
   does not carry them (same treatment as rw_control). *)
Inductive rw_prim (flags : eff_flags) (C : code_env)
    : tenv -> expr -> expr -> Prop :=

(** RULE S.Rewrite.Prim.Transfer (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive
    CODE middle_end/flambda2/simplify/simplify_named.ml#simplify_named0 *)
(* The base case: arguments are canonicalized and the primitive kept;
   the result variable gains the type computed by [[p]]#.  Premises
   in doc order: arguments canonicalized (the conclusion applies
   canonicalize_prim; per-Simple statement rw_simple); each
   argument's kind matches the expected argument kind (the kind
   check, simplify_primitive.ml#arg_kind_mismatch); [[p]]# on the
   argument types yields T |> eps.  ENCODING NOTE: the "no more
   specific rewrite below applies" priority premise is pass
   machinery; the relational envelope derives the refinements
   alongside the base case. *)
| S_Rewrite_Prim_Transfer : forall E bp p ks Ts T eps body,
    length ks = length (prim_args p) ->
    prim_arg_kinds (prim_op_of p) ks ->
    Forall2 (fun s k => simple_kind_in E s = Some k)
      (prim_args p) ks ->
    Forall2 (fun sk T' =>
        T' = fst (type_simple_in_term E (snd sk) (fst sk)))
      (combine (prim_args p) ks) Ts ->
    prim_transfer (prim_op_of p) Ts T eps ->
    rw_prim flags C E
      (E_let bp (N_prim p) body)
      (E_let bp (N_prim (canonicalize_prim E p)) body)

(** RULE S.Rewrite.Prim.ArgKindMismatch (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_primitive.ml#arg_kind_mismatch
    CODE middle_end/flambda2/simplify/simplify_primitive.ml#simplify_primitive *)
(* Premises in doc order: p expects argument kinds kbar; some
   argument's type has a kind other than the corresponding one;
   kind checks are off.  Sound because the binding is unreachable
   (the argument's type would be Bottom at the expected kind).
   ENCODING NOTE: the Flambda_features.kind_checks() = false premise
   is pass plumbing outside eff_flags; the statement is the flag-off
   behavior (with the flag on this is a fatal compiler error) --
   same treatment as join_points() in T.Join.ConstAgreement. *)
| S_Rewrite_Prim_ArgKindMismatch : forall E bp p ks s k k' body,
    prim_arg_kinds (prim_op_of p) ks ->
    In (s, k) (combine (prim_args p) ks) ->
    simple_kind_in E s = Some k' ->
    k' <> k ->
    rw_prim flags C E
      (E_let bp (N_prim p) body)
      (E_invalid "Prim_arg_kind_mismatch"%string)

(** RULE S.Rewrite.Prim.Relational (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_relational_primitive
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_get_tag *)
(* Premises in doc order: the primitive is is_int(x) [resp.
   get_tag(x), is_null(x)]; x is not resolved to a constant.
   Conclusion: the primitive is kept and the extension relates the
   result to the scrutinee (add_is_int_relation and friends; here
   the extension eps is pinned by the irel_ext premise and recorded
   by dacc).  Four constructors: the three relational primitives,
   plus the fold that only the general is_int (variant_only = false)
   performs when the scrutinee's type already determines the answer
   (the doc's NOTES cross-reference to S.Rewrite.Prim.ConstFold;
   stated here on provers.ml#prove_is_int since the argument itself
   is not a constant).  get_tag never folds directly; the constant
   is recovered downstream by meet/reify through the relation. *)
| S_Rewrite_Prim_Relational_IsInt : forall E r m vo x n eps body,
    simple_is_const (canonical E x) = false ->
    simple_name (canonical E x) = Some n ->
    eps = irel_ext r Rel_is_int n ->
    rw_prim flags C E
      (E_let (BPat_singleton (Mk_bound_var r m))
         (N_prim (P_unary (UP_is_int vo) x)) body)
      (E_let (BPat_singleton (Mk_bound_var r m))
         (N_prim (P_unary (UP_is_int vo) x)) body)
| S_Rewrite_Prim_Relational_GetTag : forall E r m x n eps body,
    simple_is_const (canonical E x) = false ->
    simple_name (canonical E x) = Some n ->
    eps = irel_ext r Rel_get_tag n ->
    rw_prim flags C E
      (E_let (BPat_singleton (Mk_bound_var r m))
         (N_prim (P_unary UP_get_tag x)) body)
      (E_let (BPat_singleton (Mk_bound_var r m))
         (N_prim (P_unary UP_get_tag x)) body)
| S_Rewrite_Prim_Relational_IsNull : forall E r m x n eps body,
    simple_is_const (canonical E x) = false ->
    simple_name (canonical E x) = Some n ->
    eps = irel_ext r Rel_is_null n ->
    rw_prim flags C E
      (E_let (BPat_singleton (Mk_bound_var r m))
         (N_prim (P_unary UP_is_null x)) body)
      (E_let (BPat_singleton (Mk_bound_var r m))
         (N_prim (P_unary UP_is_null x)) body)
| S_Rewrite_Prim_Relational_IsIntFold : forall E bp x b body,
    prove_is_int E (stored_type E K_value (canonical E x))
      (Proved b) ->
    rw_prim flags C E
      (E_let bp (N_prim (P_unary (UP_is_int false) x)) body)
      (E_let bp (N_simple (Simple_const
         (Const_naked_immediate (if b then 1 else 0)))) body)

(** RULE S.Rewrite.Prim.ConstFold (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Binary_arith_like
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unary_primitive
    VERIFIED 14-validation/new-01-constfold.md
    VERIFIED 14-validation/new-05-inline-fold.md *)
(* Premises in doc order: each argument proven equal to a constant
   (Known_result; proven_const above); the denotation is defined on
   those constants with no heap effect.  The folded constant is
   exactly [[p]] applied to the argument constants -- the premise
   invokes Machine.v's denot_prim (the union of denot_scalar /
   denot_mem_a / denot_mem_b), which is the soundness coupling to
   chapters 05-06.  "with x : {c}" is dacc type recording.
   ENCODING NOTE: float arithmetic is excluded here and stated in
   the flag-gated S.Rewrite.Prim.ConstFold.Float constructor below
   (its NOTES make the propagating_float_consts gate part of the
   fold's applicability; an ungated float clause would be unsound
   under the ambient-flags reading). *)
| S_Rewrite_Prim_ConstFold : forall E bp p ks cs vs v c body,
    is_float_arith (prim_op_of p) = false ->
    length ks = length (prim_args p) ->
    prim_arg_kinds (prim_op_of p) ks ->
    Forall2 (fun sk cst => proven_const E (snd sk) (fst sk) cst)
      (combine (prim_args p) ks) cs ->
    Forall2 (fun cst v' => const_value cst = Some v') cs vs ->
    (forall H, denot_prim (prim_op_of p) vs H (PR_ok v H)) ->
    const_value c = Some v ->
    rw_prim flags C E
      (E_let bp (N_prim p) body)
      (E_let bp (N_simple (Simple_const c)) body)

(** RULE S.Rewrite.Prim.ConstFold.Float (CLAIM descriptive)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_float_arith_op
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen *)
(* Premises in doc order: p is a float arithmetic primitive with all
   arguments proven constant; DE.propagating_float_consts = true
   (the ef_float_const_prop flag).  The computed value, when folding
   occurs, is normative (same denot_prim premise as ConstFold, under
   the IEEE semantics of the ch. 05 float rules).  Descriptive
   because the flag can change; encoded as a defining clause since
   the flag is in eff_flags and the fold's content is exact. *)
| S_Rewrite_Prim_ConstFold_Float : forall E bp p ks cs vs v c body,
    ef_float_const_prop flags = true ->
    is_float_arith (prim_op_of p) = true ->
    length ks = length (prim_args p) ->
    prim_arg_kinds (prim_op_of p) ks ->
    Forall2 (fun sk cst => proven_const E (snd sk) (fst sk) cst)
      (combine (prim_args p) ks) cs ->
    Forall2 (fun cst v' => const_value cst = Some v') cs vs ->
    (forall H, denot_prim (prim_op_of p) vs H (PR_ok v H)) ->
    const_value c = Some v ->
    rw_prim flags C E
      (E_let bp (N_prim p) body)
      (E_let bp (N_simple (Simple_const c)) body)

(** RULE S.Rewrite.Prim.ConstFold.PartialUndef (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Binary_arith_like *)
(* Premise: integer division/remainder by a proven-zero divisor.
   [[div]](_, 0) = undef (ch. 05), reachable only in dead code, so
   the binding becomes Invalid.  Realized as the Invalid outcome of
   op_lhs_unknown and of op in Int_ops_for_binary_arith, which
   check_possible_results turns into SPR.create_invalid. *)
| S_Rewrite_Prim_ConstFold_PartialUndef :
    forall E bp k op s1 s2 body,
    op = BIA_div \/ op = BIA_mod ->
    proven_const E (kind_si k) s2 (const_si k 0) ->
    rw_prim flags C E
      (E_let bp (N_prim (P_binary (BP_int_arith k op) s1 s2)) body)
      (E_invalid "Division_by_zero"%string)

(** RULE S.Rewrite.Prim.Reify (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_named.ml#simplify_named0
    CODE middle_end/flambda2/simplify/lifting/reification.ml#try_to_reify *)
(* Premises in doc order: the binding was simplified with try_reify
   set (SPR plumbing, in prose); x's type T (computed by [[p]]#)
   reifies to a single value.  Reification reads the TYPE, not the
   primitive.  ENCODING NOTE: the constant path (reify returning
   Simple) is transcribed; the lifting path (a fresh symbol, gated
   by allow_lifting on at-most-generative effects and normal mode)
   rests on reify's R_lift clauses, which are underivable in the
   MeetJoin.v envelope (see the note on reify there), so it is
   documented here and vacuous in-model. *)
| S_Rewrite_Prim_Reify : forall E bp p ks Ts T eps s body,
    length ks = length (prim_args p) ->
    prim_arg_kinds (prim_op_of p) ks ->
    Forall2 (fun sk T' =>
        T' = fst (type_simple_in_term E (snd sk) (fst sk)))
      (combine (prim_args p) ks) Ts ->
    prim_transfer (prim_op_of p) Ts T eps ->
    reify E T (R_simple s) ->
    rw_prim flags C E
      (E_let bp (N_prim p) body)
      (E_let bp (N_simple s) body)

(** RULE S.Rewrite.Prim.IntIdentity (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift *)
(* Premises in doc order: an integer binary arithmetic/logical/shift
   primitive; one operand proven a neutral/absorbing constant for
   the operation (tables int_arith_identity / int_shift_identity
   above; the named constant is the KNOWN operand).  The primitive
   is replaced per the outcome column (int_id_named).  Sound by the
   ring/lattice identities of fixed-width two's-complement
   arithmetic under wraparound.  div/mod by 0 is
   S.Rewrite.Prim.ConstFold.PartialUndef; there is deliberately NO
   power-of-two multiply-to-shift rewrite.  For shifts the left
   operand is the value (kind of k) and the right the shift amount
   (a naked immediate). *)
| S_Rewrite_Prim_IntIdentity_Arith :
    forall E bp k op s1 s2 sd zc res s_known s_other body,
    int_arith_identity op sd zc res ->
    (sd = Side_left /\ s_known = s1 /\ s_other = s2)
    \/ (sd = Side_right /\ s_known = s2 /\ s_other = s1) ->
    proven_const E (kind_si k) s_known (const_si k zc) ->
    rw_prim flags C E
      (E_let bp (N_prim (P_binary (BP_int_arith k op) s1 s2)) body)
      (E_let bp (int_id_named k s_other res) body)
| S_Rewrite_Prim_IntIdentity_Shift :
    forall E bp k op s1 s2 sd zc res body,
    int_shift_identity op sd zc res ->
    (sd = Side_left ->
       proven_const E (kind_si k) s1 (const_si k zc)) ->
    (sd = Side_right ->
       proven_const E K_naked_immediate s2
         (Const_naked_immediate zc)) ->
    rw_prim flags C E
      (E_let bp (N_prim (P_binary (BP_int_shift k op) s1 s2)) body)
      (E_let bp (int_id_named k s1 res) body)

(** RULE S.Rewrite.Prim.FloatIdentity (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen *)
(* Premises in doc order: a float binary arithmetic primitive under
   propagating_float_consts; one operand proven a bit-pattern-
   preserving neutral element.  Only multiply/divide by +-1.0 --
   these preserve every bit of the other operand; x + 0.0 and
   x - 0.0 are deliberately NOT simplified (signed zeros).  The
   constant may sit on either side of mul, only on the divisor side
   of div. *)
| S_Rewrite_Prim_FloatIdentity :
    forall E bp w op s1 s2 sd c neg s_known s_other body,
    ef_float_const_prop flags = true ->
    op = BFA_mul \/ (op = BFA_div /\ sd = Side_right) ->
    (sd = Side_left /\ s_known = s1 /\ s_other = s2)
    \/ (sd = Side_right /\ s_known = s2 /\ s_other = s1) ->
    float_pm_one w c neg ->
    proven_const E (kind_fbw w) s_known c ->
    rw_prim flags C E
      (E_let bp (N_prim (P_binary (BP_float_arith w op) s1 s2))
         body)
      (E_let bp
         (if neg
          then N_prim (P_unary (UP_float_arith w UFA_neg) s_other)
          else N_simple s_other)
         body)

(** RULE S.Rewrite.Prim.UntagTag (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_untag_immediate
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unbox_number
    VERIFIED 14-validation/code_size_of_boolean_not_switch.md *)
(* Premises in doc order: untag_imm(x) [resp. unbox_number(x)]; x's
   type carries an alias to a naked immediate [resp. naked number] s
   (the projection shape tagged_immediate_alias_to /
   boxed_*_alias_to: the expanded head's immediates arm / payload is
   an Equals-s type).  Untag(Tag s) folds to s structurally: the
   forward tag/box set x's type to record the payload alias.  The
   inverse direction (Tag(Untag), Box(Unbox)) is recovered by CSE
   equations installed at the untag/unbox (box only at proven-Heap
   mode) -- see S.Rewrite.Share.StaticDynamicSplit.  The nullability
   and is-null wildcard follows gen_value_to_meet
   (T.Prove.MeetShortcut.NullPremise): the answer is computed from
   the non-null head. *)
| S_Rewrite_Prim_UntagTag_Untag :
    forall E bp x U ii timms gt bl ext u isnull s body,
    expand_head E (fst (type_simple_in_term E K_value x)) U ->
    U = FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok (HV_variant ii (Ou_known timms) gt bl ext u))
          isnull))) ->
    ftype_alias_simple timms = Some s ->
    rw_prim flags C E
      (E_let bp (N_prim (P_unary UP_untag_immediate x)) body)
      (E_let bp (N_simple s) body)
| S_Rewrite_Prim_UntagTag_Unbox :
    forall E bp bn x U hnn am t s isnull body,
    expand_head E (fst (type_simple_in_term E K_value x)) U ->
    U = FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok hnn) isnull))) ->
    boxed_payload_view hnn = Some (bn, t, am) ->
    ftype_alias_simple t = Some s ->
    rw_prim flags C E
      (E_let bp (N_prim (P_unary (UP_unbox_number bn) x)) body)
      (E_let bp (N_simple s) body)

(** RULE S.Rewrite.Prim.Projection (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_immutable_block_load0
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_value_slot
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_function_slot
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_array_length
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_string_length
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load *)
(* Reading a component of a provably immutable aggregate at a known
   position.  Only the fast-path group (a) literally binds
   let r = s and is transcribed: Block_load (immutable, constant
   field), Project_value_slot, Project_function_slot.  Group (b)
   (array_load / array_length / string_length) KEEPS the primitive,
   refines r's type and folds only later through
   S.Rewrite.Prim.Reify (the doc's refine-and-reify mechanism) --
   those instances are the Transfer + Reify composition and get no
   constructor here.  Premises per constructor, in doc order: the
   aggregate's type records the field / value slot / sibling closure
   as a Simple (meet_block_field_simple /
   meet_project_value_slot_simple / meet_project_function_slot_simple,
   stated structurally on the expanded head; the closures cases ride
   the on-disk prove_single_closures_entry).  Immutability is
   essential: mutable loads never fold; closure slots are always
   immutable.  The single-known-tag premise on Block_load transcribes
   the meet's requirement that the loaded field is determined across
   the row-like (other_tags Bottom, one known tag). *)
| S_Rewrite_Prim_Projection_BlockLoad :
    forall E bp bak i x U ii imms gt kt am ext u isnull
           t fields idx ceps Tf s body,
    expand_head E (fst (type_simple_in_term E K_value x)) U ->
    U = FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok (HV_variant ii imms gt
             (Ou_known (Mk_row_like_for_blocks kt Ob_bottom am))
             ext u))
          isnull))) ->
    kt t = Some (Ou_known (Mk_row_like_block_case fields idx ceps)) ->
    (forall t', (exists c', kt t' = Some c') -> t' = t) ->
    (0 <= i)%Z ->
    nth_error fields (Z.to_nat i) = Some Tf ->
    ftype_alias_simple Tf = Some s ->
    rw_prim flags C E
      (E_let bp (N_prim (P_unary (UP_block_load bak Immutable i) x))
         body)
      (E_let bp (N_simple s) body)
| S_Rewrite_Prim_Projection_ValueSlot :
    forall E bp fs w x fts cts vts Tw s body,
    prove_single_closures_entry E x fs
      (Mk_closures_entry fts cts vts) ->
    vts w = Some Tw ->
    ftype_alias_simple Tw = Some s ->
    rw_prim flags C E
      (E_let bp (N_prim (P_unary (UP_project_value_slot fs w) x))
         body)
      (E_let bp (N_simple s) body)
| S_Rewrite_Prim_Projection_FunctionSlot :
    forall E bp f f' x fts cts vts Tf s body,
    prove_single_closures_entry E x f
      (Mk_closures_entry fts cts vts) ->
    cts f' = Some Tf ->
    ftype_alias_simple Tf = Some s ->
    rw_prim flags C E
      (E_let bp
         (N_prim (P_unary (UP_project_function_slot f f') x)) body)
      (E_let bp (N_simple s) body)

(** RULE S.Rewrite.Prim.PhysEqual (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_phys_equal
    CODE middle_end/flambda2/types/provers.ml#prove_physical_equality *)
(* Premises in doc order: phys_equal(op, x, y) with op in {Eq, Neq};
   the prover proves the two values definitely equal or definitely
   distinct.  The fold is b for Eq and its negation for Neq
   (mirroring the ch. 06 P.Binary.PhysEqual NOTE; the result is a
   naked immediate, per the denotation).  ENCODING NOTE: the
   definitely-equal envelope is "same canonical simple"; the
   definitely-distinct envelope is "two distinct tagged-immediate
   constants" -- prove_physical_equality's richer disequality
   analyses (immediate vs block, distinct symbols, ...) are left
   unrelated. *)
| S_Rewrite_Prim_PhysEqual_Equal : forall E bp ec x y i body,
    canonical E x = canonical E y ->
    i = match ec with EC_eq => 1 | EC_neq => 0 end ->
    rw_prim flags C E
      (E_let bp (N_prim (P_binary (BP_phys_equal ec) x y)) body)
      (E_let bp (N_simple (Simple_const (Const_naked_immediate i)))
         body)
| S_Rewrite_Prim_PhysEqual_Distinct : forall E bp ec x y a b i body,
    canonical E x = Simple_const (Const_tagged_immediate a) ->
    canonical E y = Simple_const (Const_tagged_immediate b) ->
    a <> b ->
    i = match ec with EC_eq => 0 | EC_neq => 1 end ->
    rw_prim flags C E
      (E_let bp (N_prim (P_binary (BP_phys_equal ec) x y)) body)
      (E_let bp (N_simple (Simple_const (Const_naked_immediate i)))
         body)

(** RULE S.Rewrite.Prim.ObjDupElide (CLAIM descriptive)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_obj_dup *)
(* Premise: obj_dup(x) where x's type proves it an immutable
   boxed/tagged number or a string; the copy is elided.  Three
   constructors: heap-mode boxed number, string, tagged immediate
   (the variant whose blocks arm is empty, non-null).  The NOTES'
   local-mode boxed-number case becomes an unbox-then-rebox rather
   than an elision and is not transcribed (hence the AMT_heap
   premise).  Sound because physical identity of an immutable value
   is not observable in a way a duplicate could change -- but see
   the W-19 watch item on in-model deterministic physical equality,
   flagged to main with the CSE sharing rules. *)
| S_Rewrite_Prim_ObjDupElide_Boxed :
    forall E bp x U hnn bn t isnull body,
    expand_head E (fst (type_simple_in_term E K_value x)) U ->
    U = FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok hnn) isnull))) ->
    boxed_payload_view hnn = Some (bn, t, AMT_heap) ->
    rw_prim flags C E
      (E_let bp (N_prim (P_unary UP_obj_dup x)) body)
      (E_let bp (N_simple x) body)
| S_Rewrite_Prim_ObjDupElide_String :
    forall E bp x U infos isnull body,
    expand_head E (fst (type_simple_in_term E K_value x)) U ->
    U = FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok (HV_string infos)) isnull))) ->
    rw_prim flags C E
      (E_let bp (N_prim (P_unary UP_obj_dup x)) body)
      (E_let bp (N_simple x) body)
| S_Rewrite_Prim_ObjDupElide_Imm :
    forall E bp x U ii imms gt rl ext u body,
    expand_head E (fst (type_simple_in_term E K_value x)) U ->
    U = FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok (HV_variant ii imms gt (Ou_known rl) ext u))
          Not_null))) ->
    rlfb_empty rl ->
    rw_prim flags C E
      (E_let bp (N_prim (P_unary UP_obj_dup x)) body)
      (E_let bp (N_simple x) body).

(** RULE S.Rewrite.Prim.CompareRecovery (CLAIM descriptive)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#recover_comparison_primitive
    CODE middle_end/flambda2/simplify/comparison_result.ml#convert_result_compared_to_tagged_zero
    Premises in doc order: the primitive is cmp((compare x y), 0)
    with cmp an integer relation against the constant 0, and
    "compare x y" was recorded as a comparison result in denv.
    Conclusion: the binding collapses to the direct relation
    x <cmp> y, trying both operand orders (flipping Lt and Gt).
    Sound because (compare x y) <op> 0 is equivalent to the
    corresponding direct order relation.
    ENCODING NOTE: the premise quantifies over denv's recorded
    comparison results (comparison_result.ml -- dacc internals
    outside the model), so per the catalog-37 decision rule this
    descriptive rule is a documented anchor. *)
Definition S_Rewrite_Prim_CompareRecovery_documented : Prop := True.

(* ================================================================== *)
(* 7. The static/dynamic sharing split                                *)
(* ================================================================== *)

(** RULE S.Rewrite.Share.StaticDynamicSplit (CLAIM normative)
    -- 10-simplify-rewrites.md
    CODE middle_end/flambda2/terms/flambda_primitive.ml#unary_primitive_eligible_for_cse
    CODE middle_end/flambda2/terms/flambda_primitive.ml#binary_primitive_eligible_for_cse
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_immutable_block_load0
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load
    A pure projection's sharing mechanism is assigned by whether its
    access path is STATIC (in the primitive payload) or DYNAMIC (a
    Simple operand): (1) static path -- Block_load,
    Project_function_slot, Project_value_slot, Unbox_number,
    Untag_immediate -- not CSE-eligible, shared exclusively through
    the types domain, each installing an inverse-construction CSE
    equation (Make_block from a fully-known load; Box_number from
    unbox, gated on proven-Heap mode; Tag_immediate from untag);
    (2) dynamic index -- immutable Array_load -- forward
    CSE-eligible, types recover only constant-index instances;
    (3) discriminators (Is_int, Get_tag, Is_null) ride BOTH forward
    CSE and the relational types domain, Get_header only CSE;
    (4) String_or_bigstring_load (String) rides neither (its storage
    may be mutable bytes via the deprecated %caml_string_get*
    builtins), only contents-known folds (PENDING UPSTREAM: the
    fold lives on flambda2-string-load-fold, 9712d270eb -- see the
    rule's proviso).
    ENCODING NOTE (hybrid, catalog): the eligibility-assignment
    clauses are stated on cse_prim_pred and proved outright
    (envelope-Qed); the types-domain sharing clauses and the
    inverse-construction equations quantify over the simplifiers'
    dacc updates and are documented above.  Composes
    S.Rewrite.CSE.Eligible, T.Prove.MeetShortcut,
    T.Grammar.RowLike.Index. *)
Theorem S_Rewrite_Share_StaticDynamicSplit :
  (forall bak mu i,
     ~ cse_prim_pred (Op_unary (UP_block_load bak mu i)))
  /\ (forall f f',
        ~ cse_prim_pred (Op_unary (UP_project_function_slot f f')))
  /\ (forall f w,
        ~ cse_prim_pred (Op_unary (UP_project_value_slot f w)))
  /\ (forall bn, ~ cse_prim_pred (Op_unary (UP_unbox_number bn)))
  /\ ~ cse_prim_pred (Op_unary UP_untag_immediate)
  /\ (forall ak alk,
        cse_prim_pred (Op_binary (BP_array_load ak alk Immutable)))
  /\ (forall vo, cse_prim_pred (Op_unary (UP_is_int vo)))
  /\ cse_prim_pred (Op_unary UP_get_tag)
  /\ cse_prim_pred (Op_unary UP_is_null)
  /\ cse_prim_pred (Op_unary UP_get_header)
  /\ (forall w,
        ~ cse_prim_pred
            (Op_binary (BP_string_or_bigstring_load SLV_string w))).
Proof.
  repeat split.
  - intros bak mu i Hc. inversion Hc.
  - intros f f' Hc. inversion Hc.
  - intros f w Hc. inversion Hc.
  - intros bn Hc. inversion Hc.
  - intro Hc. inversion Hc.
  - intros ak alk. apply CPP_array_load_imm. left. reflexivity.
  - intro vo. apply CPP_is_int.
  - apply CPP_get_tag.
  - apply CPP_is_null.
  - apply CPP_get_header.
  - intros w Hc. inversion Hc.
Qed.

(* ================================================================== *)
(* 8. Pilot lemmas (Tier 1)                                           *)
(* ================================================================== *)

(* Physical equality on a shared canonical folds to true. *)
Lemma rw_prim_phys_equal_refl :
  forall fl C E bp x body,
    rw_prim fl C E
      (E_let bp (N_prim (P_binary (BP_phys_equal EC_eq) x x)) body)
      (E_let bp
         (N_simple (Simple_const (Const_naked_immediate 1))) body).
Proof.
  intros fl C E bp x body.
  eapply S_Rewrite_Prim_PhysEqual_Equal; reflexivity.
Qed.

(* x + 0 rewrites to x whenever the right operand is proven zero. *)
Lemma rw_prim_add_zero :
  forall fl C E bp k s1 s2 body,
    proven_const E (kind_si k) s2 (const_si k 0) ->
    rw_prim fl C E
      (E_let bp
         (N_prim (P_binary (BP_int_arith k BIA_add) s1 s2)) body)
      (E_let bp (N_simple s1) body).
Proof.
  intros fl C E bp k s1 s2 body Hz.
  change (N_simple s1) with (int_id_named k s1 IIR_other).
  eapply S_Rewrite_Prim_IntIdentity_Arith.
  - apply IAI_add_0.
  - right. repeat split.
  - exact Hz.
Qed.
