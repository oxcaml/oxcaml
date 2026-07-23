(** * Concretization.v — ch. 07 sec. 4: gamma_E, consistency, set form

    Mechanizes the concretization judgment of 07-types-domain.md
    section 4 (with section 5's extension satisfaction).  gamma is
    indexed by a concrete environment rho and heap H because types are
    relational (Equals types and relational immediates refer to the
    values of other names); the doc's set form gamma_E(T) is the
    derived [gamma_set] below (exists rho H consistent with E).

    All T.Gamma.* rules are CLAIM interpretive: the code does not
    compute gamma, and this reading is the doc's intended denotation.
    Per the status mapping, the per-former clauses are constructors
    (conjectured defining clauses) and the two property rules
    (T.Gamma.Kind, T.Gamma.Closures.CodeAgeLoose) are theorems.

    Owner: Scott (wave 3).  Imports: Base, Syntax, Values,
    TypeGrammar. *)

From Stdlib Require Import ZArith List Bool.
From Flambda2 Require Import Base Syntax Values TypeGrammar.
Import ListNotations.

Open Scope Z_scope.

(* ------------------------------------------------------------------ *)
(* Kind of a runtime value                                             *)
(* ------------------------------------------------------------------ *)

(* The kinds partition Val (03-kinds.md; quoted by T.Gamma.Kind), so
   every value has exactly one kind. *)
Definition kind_of_value (v : value) : kind :=
  match v with
  | V_tagged_imm _ | V_ptr _ | V_clos _ _ | V_null => K_value
  | V_naked_imm _ => K_naked_immediate
  | V_naked_int8 _ => K_naked_int8
  | V_naked_int16 _ => K_naked_int16
  | V_naked_int32 _ => K_naked_int32
  | V_naked_int64 _ => K_naked_int64
  | V_naked_nativeint _ => K_naked_nativeint
  | V_naked_float _ => K_naked_float
  | V_naked_float32 _ => K_naked_float32
  | V_naked_vec128 _ => K_naked_vec128
  | V_naked_vec256 _ => K_naked_vec256
  | V_naked_vec512 _ => K_naked_vec512
  | V_region _ => K_region
  | V_rec_info => K_rec_info
  end.

Definition value_has_kind (v : value) (k : kind) : Prop :=
  kind_of_value v = k.

(* ------------------------------------------------------------------ *)
(* Small helpers on types and heap objects                             *)
(* ------------------------------------------------------------------ *)

(* Whether a type is the unrestricted top of its kind (the Unknown
   descr).  Used premise-style by the top clause of gamma so that
   inversion computes. *)
Definition ftype_is_unknown (T : ftype) : bool :=
  match T with
  | FT_value Oub_unknown
  | FT_naked_immediate Oub_unknown
  | FT_naked_float Oub_unknown
  | FT_naked_float32 Oub_unknown
  | FT_naked_int8 Oub_unknown
  | FT_naked_int16 Oub_unknown
  | FT_naked_int32 Oub_unknown
  | FT_naked_int64 Oub_unknown
  | FT_naked_nativeint Oub_unknown
  | FT_naked_vec128 Oub_unknown
  | FT_naked_vec256 Oub_unknown
  | FT_naked_vec512 Oub_unknown
  | FT_rec_info Oub_unknown
  | FT_region Oub_unknown => true
  | _ => false
  end.

Definition tag_z (t : tag) : Z :=
  match t with Mk_tag n => Z.of_nat n end.

(* Is_int / Is_null as 0/1 flags on values (ch. 07 sec. 4.2). *)
Definition is_int_flag (v : value) : Z :=
  match v with V_tagged_imm _ => 1 | _ => 0 end.

Definition is_null_flag (v : value) : Z :=
  match v with V_null => 1 | _ => 0 end.

(* Tag and size of the block object at an address.
   ENCODING NOTE: HO_FloatBlock stores no tag (the runtime
   Double_array_tag is implicit in the object form), so float blocks
   are not related by this view; Float_record row-like cases match on
   the object form directly and ignore the tag key, and get_tag /
   tag-keyed-Unknown readings admit only tagged block objects. *)
Inductive block_tag_size (H : heap) (a : address) : tag -> Z -> Prop :=
  | BTS_block : forall t mu vs,
      H (HK_addr a) = Some (HO_Block t mu vs) ->
      block_tag_size H a t (Z.of_nat (length vs))
  | BTS_mixed : forall t mu sg vs,
      H (HK_addr a) = Some (HO_MixedBlock t mu sg vs) ->
      block_tag_size H a t (Z.of_nat (length vs)).

(* A pointer to any block object (scannable, mixed, or float). *)
Definition is_block_obj (o : heap_object) : Prop :=
  match o with
  | HO_Block _ _ _ | HO_MixedBlock _ _ _ _ | HO_FloatBlock _ _ => True
  | _ => False
  end.

Definition is_block_ptr (H : heap) (a : address) : Prop :=
  exists o, H (HK_addr a) = Some o /\ is_block_obj o.

(* R(w) for the three relations of ch. 07 sec. 2.4 / 4.2:
   Is_int(w) = 1 iff w is a tagged immediate; Is_null(w) = 1 iff
   w = Null; Get_tag(w) = the tag of block w (defined only on block
   pointers). *)
Inductive relation_denotes (H : heap) : irelation -> value -> Z -> Prop :=
  | RD_is_int : forall w,
      relation_denotes H Rel_is_int w (is_int_flag w)
  | RD_is_null : forall w,
      relation_denotes H Rel_is_null w (is_null_flag w)
  | RD_get_tag : forall a t sz,
      block_tag_size H a t sz ->
      relation_denotes H Rel_get_tag (V_ptr a) (tag_z t).

(* The unified (descr) view of a Naked_immediate head: the pair
   (set Or_unknown, relations map) — see the rule comment on
   head_naked_immediate in TypeGrammar.v. *)
Definition hni_view (h : head_naked_immediate)
  : or_unknown (list Z) * inverse_relations :=
  match h with
  | Naked_immediates s => (Ou_known s, fempty)
  | Inverse_relations m => (Ou_unknown, m)
  | Naked_immediates_and_inverse_relations s m => (Ou_known s, m)
  end.

(* The Boxed_kappa heads as (naked kind, contents type); alloc modes
   are carried but not concretized (ch. 07 sec. 6). *)
Definition boxed_view (h : head_value_non_null)
  : option (kind * ftype) :=
  match h with
  | HV_boxed_float32 t _ => Some (K_naked_float32, t)
  | HV_boxed_float t _ => Some (K_naked_float, t)
  | HV_boxed_int32 t _ => Some (K_naked_int32, t)
  | HV_boxed_int64 t _ => Some (K_naked_int64, t)
  | HV_boxed_nativeint t _ => Some (K_naked_nativeint, t)
  | HV_boxed_vec128 t _ => Some (K_naked_vec128, t)
  | HV_boxed_vec256 t _ => Some (K_naked_vec256, t)
  | HV_boxed_vec512 t _ => Some (K_naked_vec512, t)
  | _ => None
  end.

(* Block-size admission by a row-like index domain
   (T.Gamma.Value.RowLikeBlocks: exactly when Known, at least when
   At_least; the doc identifies the Known/At_least payload with the
   tracked field count — the code invariant check_field_tys — so the
   admission is stated on the stored Block_size). *)
Definition size_admitted (d : row_like_index_domain block_size)
  (n : Z) : Prop :=
  match d with
  | Rl_known sz => n = sz
  | Rl_at_least sz => sz <= n
  end.

(* The Set_of_closures_contents named by a closures index.
   ENCODING NOTE: T.Gamma.Value.Closures says the closure block
   "contains at least" the named slots, for both Known and At_least
   indices; the exactly-vs-at-least sharpening for Known closure
   indices is not stated by the doc's gamma clause and is not modeled
   here. *)
Definition socc_of_domain
  (d : row_like_index_domain set_of_closures_contents)
  : set_of_closures_contents :=
  match d with Rl_known c => c | Rl_at_least c => c end.

Definition closure_has_slots
  (funs : fmap function_slot (code_id * arity_info))
  (venv : fmap value_slot value)
  (c : set_of_closures_contents) : Prop :=
  (forall fs, In fs (socc_function_slots c) ->
     exists e, funs fs = Some e) /\
  (forall w, In w (socc_value_slots c) ->
     exists v, venv w = Some v).

(* The version class of a code id under the newer_version_of preorder
   (T.Gamma.Closures.CodeAgeLoose): related in either direction. *)
Definition code_age_same_class (E : tenv) (c1 c2 : code_id) : Prop :=
  code_age_newer_eq E c1 c2 \/ code_age_newer_eq E c2 c1.

(* Code-pointer admission for the selected function slot: when the
   case's function type is known, the slot's actual code id implements
   SOME code-age-related version of the function_type's code_id, NOT
   necessarily that code id exactly (T.Gamma.Value.Closures,
   T.Gamma.Closures.CodeAgeLoose).  The function_type's rec_info is
   opaque here (ch. 07 sec. 6; ch. 11 owns it). *)
Definition function_type_code_ok (E : tenv)
  (funs : fmap function_slot (code_id * arity_info))
  (f : function_slot)
  (oft : option (or_unknown function_type)) : Prop :=
  match oft with
  | Some (Ou_known (Mk_function_type cid _)) =>
      exists cid_act ai,
        funs f = Some (cid_act, ai) /\
        code_age_same_class E cid_act cid
  | _ => True
  end.

(* Witness constraints tying tracking variables to rho. *)

(* is_null tracking (T.Gamma.Value.Nullability): when is_null =
   Maybe_null (Some y), rho(y) = 1 if this value is Null else 0. *)
Definition null_witness (rho : env) (inl : is_null_ty) (v : value)
  : Prop :=
  match inl with
  | Not_null => True
  | Maybe_null None => True
  | Maybe_null (Some y) =>
      rho (Name_var y) = Some (V_naked_imm (is_null_flag v))
  end.

(* is_int tracking (T.Gamma.Value.Variant): rho(b) = 1 on the
   immediates arm, 0 on the blocks arm. *)
Definition is_int_witness (rho : env) (b : option variable) (z : Z)
  : Prop :=
  match b with
  | None => True
  | Some x => rho (Name_var x) = Some (V_naked_imm z)
  end.

(* get_tag tracking (T.Gamma.Value.Variant): on the blocks arm,
   rho(g) = the block's tag. *)
Definition get_tag_witness (rho : env) (H : heap)
  (gt : option variable) (a : address) : Prop :=
  match gt with
  | None => True
  | Some g =>
      exists t sz,
        block_tag_size H a t sz /\
        rho (Name_var g) = Some (V_naked_imm (tag_z t))
  end.

(* Per-arm extensions of a variant (T.Grammar.Disjunction.Extensions);
   No_extensions is the empty extension. *)
Definition ext_when_immediate (ex : variant_extensions)
  : env_extension :=
  match ex with No_extensions => fempty | Ext wi _ => wi end.

Definition ext_when_block (ex : variant_extensions) : env_extension :=
  match ex with No_extensions => fempty | Ext _ wb => wb end.

(* Element-kind admission for arrays (T.Gamma.Value.Boxed): elements
   have the stated kind; a Bottom element kind admits only the empty
   array. *)
Definition array_elem_kinds_ok (ek : or_unknown_or_bottom kind_ws)
  (elems : list value) : Prop :=
  match ek with
  | Oub_unknown => True
  | Oub_bottom => elems = nil
  | Oub_ok kws => Forall (fun w => value_has_kind w (ws_kind kws)) elems
  end.

(* String_info contents admission (T.Gamma.Value.Boxed). *)
Definition string_contents_ok (sc : string_contents) (bytes : list Z)
  : Prop :=
  match sc with
  | SC_unknown_or_mutable => True
  | SC_contents bs => bs = bytes
  end.

(* ------------------------------------------------------------------ *)
(* The concretization judgment                                         *)
(* ------------------------------------------------------------------ *)

(* ENCODING NOTE (gamma's use of E): the doc writes gamma_E, but the
   clauses below read E only through its code-age relation
   (function_type_code_ok); the constraints contributed by E's
   equations and aliases enter through rho via [consistent] below.  In
   particular T.Gamma.EnvExtension's gamma_(E u eps) coincides with
   gamma_E over the same rho (an extension never changes te_code_age),
   so [sat_ext] can call gamma at the SAME E, and the rule's
   gamma_(E u eps) SUBSETEQ gamma_E claim is definitional in this
   encoding (satisfying more equations only filters rho). *)

Inductive gamma (E : tenv) (rho : env) (H : heap)
  : ftype -> value -> Prop :=
  (** RULE T.Gamma.TopBottom (CLAIM interpretive) -- 07-types-domain.md
      CODE middle_end/flambda2/types/grammar/type_descr.mli#unknown
      CODE middle_end/flambda2/types/grammar/type_descr.mli#bottom
      gamma_E(Unknown_kappa) = all values of kind kappa (constructor
      below); gamma_E(Bottom_kappa) = empty, realized by the ABSENCE
      of any constructor for a Bottom descr and made on the nose by
      [gamma_bottom_empty] (Qed) after this block. *)
  | G_unknown : forall (T : ftype) (v : value),
      ftype_is_unknown T = true ->
      value_has_kind v (kind_of_ftype T) ->
      gamma E rho H T v
  (** RULE T.Gamma.Alias (CLAIM interpretive) -- 07-types-domain.md
      CODE middle_end/flambda2/types/grammar/type_grammar.mli#alias_type_of
      CODE middle_end/flambda2/types/grammar/type_grammar.ml#get_alias_exn
      gamma_E(Equals s) under rho is the singleton { rho(s) }.
      ENCODING NOTE: the doc applies s's coercion to the value; the
      machine erases coercions (OS.Simple.Eval, Values.v), so
      simple_eval already implements the doc's rho(s).  The kind
      premise encodes the code's alias kind-homogeneity (alias_type_of
      takes the kind, so an Equals type and its target agree on kind);
      without it this clause would be gamma's one kind-crossing
      channel, refuting T.Gamma.Kind below (KF-050). *)
  | G_equals : forall (T : ftype) (s : simple) (v : value),
      ftype_alias_simple T = Some s ->
      simple_eval rho s = Some v ->
      value_has_kind v (kind_of_ftype T) ->
      gamma E rho H T v
  (** RULE T.Gamma.Value.Nullability (CLAIM interpretive)
      -- 07-types-domain.md
      CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_value
      VERIFIED 14-validation/n_way_join_preserves_null.md
      gamma of a Value head is gamma(non_null) united with { Null }
      when is_null = Maybe_null (empty when Not_null); the two
      constructors are the two arms of the union.  When is_null =
      Maybe_null { is_null = Some y }, rho(y) = (1 if this value is
      Null else 0) — on BOTH arms (null_witness). *)
  | G_value_non_null : forall nn inl v,
      gamma_vnn E rho H nn v ->
      null_witness rho inl v ->
      gamma E rho H
        (FT_value (Oub_ok (No_alias (Mk_head_value nn inl)))) v
  | G_value_null : forall nn yo,
      null_witness rho (Maybe_null yo) V_null ->
      gamma E rho H
        (FT_value (Oub_ok (No_alias (Mk_head_value nn (Maybe_null yo)))))
        V_null
  (** RULE T.Gamma.Naked.Relational (CLAIM interpretive)
      -- 07-types-domain.md
      CODE middle_end/flambda2/types/grammar/type_grammar.ml#head_of_kind_naked_immediate
      gamma of a Naked_immediate head with set S and relations M,
      under rho, is { v | (S = Unknown \/ v in S) /\ for every
      (R |-> N) in M and x in N, v = R(rho(x)) }.  The premises are in
      that order, on the unified (set, relations) view hni_view. *)
  | G_naked_immediate : forall h n,
      (match fst (hni_view h) with
       | Ou_unknown => True
       | Ou_known s => In n s
       end) ->
      (forall R N x,
          snd (hni_view h) R = Some N ->
          In x N ->
          exists w, rho x = Some w /\ relation_denotes H R w n) ->
      gamma E rho H (FT_naked_immediate (Oub_ok (No_alias h)))
        (V_naked_imm n)
  (** RULE T.Gamma.Naked.Set (CLAIM interpretive) -- 07-types-domain.md
      CODE middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_naked_float
      For each remaining naked-number kind, gamma of a head is the
      finite set of constants it names (one constructor per kind;
      gamma(Unknown) is the top clause above; the empty head never
      occurs — T.Grammar.NakedNumber.NonEmptySet). *)
  | G_naked_float : forall (s : head_naked_float) f,
      In f s ->
      gamma E rho H (FT_naked_float (Oub_ok (No_alias s)))
        (V_naked_float f)
  | G_naked_float32 : forall (s : head_naked_float32) f,
      In f s ->
      gamma E rho H (FT_naked_float32 (Oub_ok (No_alias s)))
        (V_naked_float32 f)
  | G_naked_int8 : forall (s : head_naked_int8) n,
      In n s ->
      gamma E rho H (FT_naked_int8 (Oub_ok (No_alias s)))
        (V_naked_int8 n)
  | G_naked_int16 : forall (s : head_naked_int16) n,
      In n s ->
      gamma E rho H (FT_naked_int16 (Oub_ok (No_alias s)))
        (V_naked_int16 n)
  | G_naked_int32 : forall (s : head_naked_int32) n,
      In n s ->
      gamma E rho H (FT_naked_int32 (Oub_ok (No_alias s)))
        (V_naked_int32 n)
  | G_naked_int64 : forall (s : head_naked_int64) n,
      In n s ->
      gamma E rho H (FT_naked_int64 (Oub_ok (No_alias s)))
        (V_naked_int64 n)
  | G_naked_nativeint : forall (s : head_naked_nativeint) n,
      In n s ->
      gamma E rho H (FT_naked_nativeint (Oub_ok (No_alias s)))
        (V_naked_nativeint n)
  | G_naked_vec128 : forall (s : head_naked_vec128) b,
      In b s ->
      gamma E rho H (FT_naked_vec128 (Oub_ok (No_alias s)))
        (V_naked_vec128 b)
  | G_naked_vec256 : forall (s : head_naked_vec256) b,
      In b s ->
      gamma E rho H (FT_naked_vec256 (Oub_ok (No_alias s)))
        (V_naked_vec256 b)
  | G_naked_vec512 : forall (s : head_naked_vec512) b,
      In b s ->
      gamma E rho H (FT_naked_vec512 (Oub_ok (No_alias s)))
        (V_naked_vec512 b)
  (* Rec_info and Region heads are trivial (ch. 07 sec. 2.6; depth /
     rec_info precision is opaque per sec. 6), so an Ok head admits
     every value of the kind, like Unknown. *)
  | G_rec_info_head : forall ri,
      gamma E rho H (FT_rec_info (Oub_ok (No_alias ri))) V_rec_info
  | G_region_head : forall i,
      gamma E rho H (FT_region (Oub_ok (No_alias tt))) (V_region i)

(* The non-null part of a Value head (T.Gamma.Value.Nullability):
   gamma(non_null) is empty when Bottom (no constructor) and all
   non-null values of kind Value when Unknown. *)
with gamma_vnn (E : tenv) (rho : env) (H : heap)
  : or_unknown_or_bottom head_value_non_null -> value -> Prop :=
  | GV_unknown : forall v,
      value_has_kind v K_value ->
      v <> V_null ->
      gamma_vnn E rho H Oub_unknown v
  | GV_head : forall h v,
      gamma_hvnn E rho H h v ->
      gamma_vnn E rho H (Oub_ok h) v

with gamma_hvnn (E : tenv) (rho : env) (H : heap)
  : head_value_non_null -> value -> Prop :=
  (** RULE T.Gamma.Value.Variant (CLAIM interpretive)
      -- 07-types-domain.md
      CODE middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null
      gamma(Variant) = gamma(immediates) u gamma(blocks): tagged
      immediates whose untagged value lies in the immediates arm, and
      pointers to blocks described by the row_like_for_blocks.  When
      is_int = Some b, rho(b) = 1 on the immediates arm and 0 on the
      blocks arm; when get_tag = Some g and the value is a block,
      rho(g) = that block's tag.  Extensions constrain rho per arm
      (sec. 5).  The is_unique flag is an optimization hint and does
      not affect gamma. *)
  | GH_variant_imm : forall ii imms gt bl ex u n,
      gamma_imm_arm E rho H imms n ->
      is_int_witness rho ii 1 ->
      sat_ext E rho H (ext_when_immediate ex) ->
      gamma_hvnn E rho H (HV_variant ii imms gt bl ex u)
        (V_tagged_imm n)
  | GH_variant_block : forall ii imms gt bl ex u a,
      gamma_blocks_arm E rho H bl a ->
      is_int_witness rho ii 0 ->
      get_tag_witness rho H gt a ->
      sat_ext E rho H (ext_when_block ex) ->
      gamma_hvnn E rho H (HV_variant ii imms gt bl ex u) (V_ptr a)
  (** RULE T.Gamma.Value.Boxed (CLAIM interpretive)
      -- 07-types-domain.md
      CODE middle_end/flambda2/types/grammar/type_grammar.mli#head_of_kind_value_non_null
      Boxed_kappa T am: pointers to a boxed number of naked kind kappa
      whose contents lie in gamma(T).  Array: array pointers whose
      length lies in gamma(length), whose elements have kind
      element_kind, and (when contents = Immutable Fbar) whose element
      i lies in gamma(F_i).  String: string pointers whose (length,
      contents) match the String_info set.  Mutable_block: all block
      pointers (contents untracked).  Alloc modes are carried but not
      concretized (sec. 6). *)
  | GH_boxed : forall h k T a w,
      boxed_view h = Some (k, T) ->
      H (HK_addr a) = Some (HO_Boxed k w) ->
      gamma E rho H T w ->
      gamma_hvnn E rho H h (V_ptr a)
  | GH_array : forall ek len ac am a ak mu elems,
      H (HK_addr a) = Some (HO_Array ak mu elems) ->
      gamma E rho H len (V_tagged_imm (Z.of_nat (length elems))) ->
      array_elem_kinds_ok ek elems ->
      gamma_array_contents E rho H ac elems ->
      gamma_hvnn E rho H (HV_array ek len ac am) (V_ptr a)
  | GH_string : forall infos a mu bytes si,
      H (HK_addr a) = Some (HO_Bytes mu bytes) ->
      In si infos ->
      si_size si = Z.of_nat (length bytes) ->
      string_contents_ok (si_contents si) bytes ->
      gamma_hvnn E rho H (HV_string infos) (V_ptr a)
  | GH_mutable_block : forall am a o,
      H (HK_addr a) = Some o ->
      is_block_obj o ->
      gamma_hvnn E rho H (HV_mutable_block am) (V_ptr a)
  (* Closures pointers; the clause proper is gamma_closures below. *)
  | GH_closures : forall rl am l f,
      gamma_closures E rho H rl l f ->
      gamma_hvnn E rho H (HV_closures rl am) (V_clos l f)

(* Lifting of the variant's immediates arm: Unknown admits every
   untagged immediate; a known arm is a Naked_immediate type. *)
with gamma_imm_arm (E : tenv) (rho : env) (H : heap)
  : or_unknown ftype -> Z -> Prop :=
  | GIA_unknown : forall n, gamma_imm_arm E rho H Ou_unknown n
  | GIA_known : forall Ti n,
      gamma E rho H Ti (V_naked_imm n) ->
      gamma_imm_arm E rho H (Ou_known Ti) n

(* Lifting of the variant's blocks arm: Unknown admits every block
   pointer. *)
with gamma_blocks_arm (E : tenv) (rho : env) (H : heap)
  : or_unknown row_like_for_blocks -> address -> Prop :=
  | GBA_unknown : forall a,
      is_block_ptr H a ->
      gamma_blocks_arm E rho H Ou_unknown a
  | GBA_known : forall rl a,
      gamma_blocks E rho H rl a ->
      gamma_blocks_arm E rho H (Ou_known rl) a

(** RULE T.Gamma.Value.RowLikeBlocks (CLAIM interpretive)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/grammar/type_grammar.mli#row_like_for_blocks
    gamma(row_like_for_blocks) = block pointers p such that for some
    case (tag t, index d, fields Fbar) in known_tags (or other_tags,
    for tags with no known_tags entry), p points to a block of tag t
    whose size is admitted by d, whose tracked field i holds a value
    in gamma(F_i), and rho satisfies that case's env_extension.  A
    known_tags entry mapped to Unknown admits any block of that
    tag. *)
with gamma_blocks (E : tenv) (rho : env) (H : heap)
  : row_like_for_blocks -> address -> Prop :=
  | GB_known : forall kt ot am t c a,
      kt t = Some (Ou_known c) ->
      gamma_block_case E rho H t c a ->
      gamma_blocks E rho H (Mk_row_like_for_blocks kt ot am) a
  | GB_known_top : forall kt ot am t sz a,
      kt t = Some Ou_unknown ->
      block_tag_size H a t sz ->
      gamma_blocks E rho H (Mk_row_like_for_blocks kt ot am) a
  | GB_other : forall kt ot am t c a,
      kt t = None ->
      ot = Ob_ok c ->
      gamma_block_case E rho H t c a ->
      gamma_blocks E rho H (Mk_row_like_for_blocks kt ot am) a

(* One row-like block case at a given tag, per Block_shape: the object
   form must match the index's shape (Value_only / Mixed_record sigma
   / Float_record); tracked fields are the maps_to prefix. *)
with gamma_block_case (E : tenv) (rho : env) (H : heap)
  : tag -> row_like_block_case -> address -> Prop :=
  | GBC_value_only : forall t Ts dom eps a mu vs,
      H (HK_addr a) = Some (HO_Block t mu vs) ->
      size_admitted dom (Z.of_nat (length vs)) ->
      Forall2 (gamma E rho H) Ts (firstn (length Ts) vs) ->
      sat_ext E rho H eps ->
      gamma_block_case E rho H t
        (Mk_row_like_block_case Ts
           (Mk_row_like_index dom (Scannable Value_only)) eps) a
  | GBC_mixed : forall t Ts dom sg eps a mu vs,
      H (HK_addr a) = Some (HO_MixedBlock t mu sg vs) ->
      size_admitted dom (Z.of_nat (length vs)) ->
      Forall2 (gamma E rho H) Ts (firstn (length Ts) vs) ->
      sat_ext E rho H eps ->
      gamma_block_case E rho H t
        (Mk_row_like_block_case Ts
           (Mk_row_like_index dom (Scannable (Mixed_record sg))) eps) a
  | GBC_float : forall t Ts dom eps a mu fs,
      H (HK_addr a) = Some (HO_FloatBlock mu fs) ->
      size_admitted dom (Z.of_nat (length fs)) ->
      Forall2 (gamma E rho H) Ts
        (map V_naked_float (firstn (length Ts) fs)) ->
      sat_ext E rho H eps ->
      gamma_block_case E rho H t
        (Mk_row_like_block_case Ts
           (Mk_row_like_index dom Float_record) eps) a

(* Array contents: Unknown and Mutable track nothing; Immutable Fbar
   constrains every element (T.Gamma.Value.Boxed). *)
with gamma_array_contents (E : tenv) (rho : env) (H : heap)
  : or_unknown array_contents -> list value -> Prop :=
  | GAC_unknown : forall elems,
      gamma_array_contents E rho H Ou_unknown elems
  | GAC_mutable : forall elems,
      gamma_array_contents E rho H (Ou_known AC_mutable) elems
  | GAC_immutable : forall Ts elems,
      Forall2 (gamma E rho H) Ts elems ->
      gamma_array_contents E rho H (Ou_known (AC_immutable Ts)) elems

(** RULE T.Gamma.Value.Closures (CLAIM interpretive)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/grammar/type_grammar.mli#row_like_for_closures
    CODE middle_end/flambda2/types/grammar/type_grammar.mli#closures_entry
    gamma(Closures { by_function_slot }) = closure pointers selecting
    a function slot f present in known_closures (or admitted by
    other_closures, kept for fidelity though documented always
    Bottom) such that: the closure block contains at least the
    function and value slots named by the case's
    Set_of_closures_contents; the code pointer for f implements SOME
    code-age-related version of the function_type's code_id (the
    version class under newer_version_of), NOT necessarily code_id
    exactly (function_type_code_ok; see
    T.Gamma.Closures.CodeAgeLoose); and each closure-type /
    value-slot component holds a value in gamma of the corresponding
    component type.  rho satisfies the case's env_extension. *)
with gamma_closures (E : tenv) (rho : env) (H : heap)
  : row_like_for_closures -> location -> function_slot -> Prop :=
  | GC_known : forall kc oc l f ftm ctm vtm idx eps funs venv,
      kc f = Some
        (Mk_row_like_closures_case
           (Mk_closures_entry ftm ctm vtm) idx eps) ->
      H (HK_addr (Addr_loc l)) = Some (HO_Closures funs venv) ->
      closure_has_slots funs venv (socc_of_domain (rli_domain idx)) ->
      function_type_code_ok E funs f (ftm f) ->
      (forall f' T, ctm f' = Some T ->
          gamma E rho H T (V_clos l f')) ->
      (forall w T, vtm w = Some T ->
          exists vw, venv w = Some vw /\ gamma E rho H T vw) ->
      sat_ext E rho H eps ->
      gamma_closures E rho H (Mk_row_like_for_closures kc oc) l f
  | GC_other : forall kc oc l f ftm ctm vtm idx eps funs venv,
      kc f = None ->
      oc = Ob_ok
        (Mk_row_like_closures_case
           (Mk_closures_entry ftm ctm vtm) idx eps) ->
      H (HK_addr (Addr_loc l)) = Some (HO_Closures funs venv) ->
      closure_has_slots funs venv (socc_of_domain (rli_domain idx)) ->
      function_type_code_ok E funs f (ftm f) ->
      (forall f' T, ctm f' = Some T ->
          gamma E rho H T (V_clos l f')) ->
      (forall w T, vtm w = Some T ->
          exists vw, venv w = Some vw /\ gamma E rho H T vw) ->
      sat_ext E rho H eps ->
      gamma_closures E rho H (Mk_row_like_for_closures kc oc) l f

(** RULE T.Gamma.EnvExtension (CLAIM interpretive)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#env_extension
    CODE middle_end/flambda2/types/env/typing_env_extension.mli#t
    An extension eps = { name_i = T_i } denotes the constraint that
    rho(name_i) lies in gamma of T_i for all i (the intersection of
    its equations' constraints).  See the ENCODING NOTE above the
    block: gamma_(E u eps) coincides with gamma_E over a fixed rho, so
    the equations are read at E, and gamma_(E u eps) SUBSETEQ gamma_E
    is definitional.
    ENCODING NOTE: a name is read with simple_eval (OS.Simple.Eval),
    not a literal rho lookup -- variables read rho; symbols fall back
    to their static denotation [ptr sym] when rho does not rebind
    them.  A literal rho(n) would make extensions recording symbol
    equations unsatisfiable under consistent rho, since consistent
    does not require rho to bind symbols (KF-051). *)
with sat_ext (E : tenv) (rho : env) (H : heap)
  : env_extension -> Prop :=
  | Sat_ext_intro : forall eps,
      (forall n T, eps n = Some T ->
          exists v,
            simple_eval rho (simple_of_name n) = Some v /\
            gamma E rho H T v) ->
      sat_ext E rho H eps.

(* The Bottom half of T.Gamma.TopBottom, on the nose. *)
Lemma gamma_bottom_empty :
  forall E rho H k v, gamma E rho H (bottom_of_kind k) v -> False.
Proof.
  intros E rho H k v Hg.
  destruct k as [ | nnk | | ]; [ | destruct nnk | | ];
    simpl in Hg; inversion Hg; cbn in *; discriminate.
Qed.

(* ------------------------------------------------------------------ *)
(* Consistency of a concrete environment with E, and the set form      *)
(* ------------------------------------------------------------------ *)

(* "some rho consistent with E" (ch. 07 sec. 4 preamble): rho ranges
   over E's domain, satisfies every stored equation, and does not
   distinguish a simple from its canonical (aliases denote equality
   up to coercion, and coercions are erased at runtime).  No rho is
   consistent with a bottom env (T.Env.Find.Bottom: gamma of
   everything is empty there).  Stored equations are read with
   simple_eval, the same reading as sat_ext (KF-051): a symbol's
   equation may be carried by its static denotation rather than a rho
   binding.  The last clause pins symbol bindings to kind Value
   (symbols denote statics -- OS.Let.Static binds closures and block
   addresses only), which expansion through the symbol default
   any_value relies on (T.Expand.Head; KF-052).  FROZEN interface
   name. *)
Definition consistent (E : tenv) (rho : env) (H : heap) : Prop :=
  te_is_bottom E = false /\
  (forall n v, rho n = Some v -> name_bound_in E n) /\
  (forall n ne,
      te_types E n = Some ne ->
      exists v,
        simple_eval rho (simple_of_name n) = Some v /\
        gamma E rho H (ne_type ne) v) /\
  (forall s, simple_eval rho (canonical E s) = simple_eval rho s) /\
  (forall sym v,
      rho (Name_sym sym) = Some v -> value_has_kind v K_value).

(* The doc's set form gamma_E(T): values admitted under SOME
   consistent rho (and heap).  Derived definition; the judgment above
   is the per-former content. *)
Definition gamma_set (E : tenv) (T : ftype) (v : value) : Prop :=
  exists rho H, consistent E rho H /\ gamma E rho H T v.

(** RULE T.Gamma.Kind (CLAIM interpretive) -- 07-types-domain.md
    CODE middle_end/flambda2/types/grammar/type_grammar.mli#kind
    gamma_E(T) contains only values of kind kappa(T): concretization
    never crosses kinds.  Stated under consistency, the doc's ambient
    hypothesis for the set form; the proof does not consume it -- kind
    agreement at aliases is G_equals's kind premise (the code's alias
    kind-homogeneity, KF-050), and every other clause fixes the value
    former syntactically. *)
Theorem T_Gamma_Kind :
  forall E rho H,
    consistent E rho H ->
    forall T v, gamma E rho H T v ->
    value_has_kind v (kind_of_ftype T).
Proof.
  intros E rho H Hcons T v Hg.
  destruct Hg; try reflexivity; try assumption.
  match goal with
  | Hvnn : gamma_vnn _ _ _ _ _ |- _ =>
      destruct Hvnn; [ assumption | ]
  end.
  match goal with
  | Hh : gamma_hvnn _ _ _ _ _ |- _ => destruct Hh; reflexivity
  end.
Qed.

(** RULE T.Gamma.Closures.CodeAgeLoose (CLAIM interpretive)
    -- 07-types-domain.md
    CODE middle_end/flambda2/types/meet_and_join.ml#meet_code_id
    CODE middle_end/flambda2/types/env/code_age_relation.ml#meet
    CODE middle_end/flambda2/types/env/typing_env.ml#add_to_code_age_relation
    The gamma clause for Closures code pointers reads the
    function_type's code_id up to the newer_version_of preorder, not
    exactly: a closure whose actual code id cid_act is merely
    code-age-related to the stated cid is admitted whenever the other
    premises hold.  In this mechanization the loose reading is built
    into function_type_code_ok, so the rule is provable by applying
    the constructors (Qed rather than Admitted).  The doc's NOTES on
    meet_code_id's Both_inputs abuse live in the ch. 07 doc rule's own
    NOTES; ch. 08's mechanization does not model meet_code_id
    (MeetJoin.v holds te_code_age constant through descent). *)
Theorem T_Gamma_Closures_CodeAgeLoose :
  forall E rho H kc oc am l f ftm ctm vtm idx eps funs venv
         cid ri cid_act ai,
    kc f = Some
      (Mk_row_like_closures_case
         (Mk_closures_entry ftm ctm vtm) idx eps) ->
    H (HK_addr (Addr_loc l)) = Some (HO_Closures funs venv) ->
    closure_has_slots funs venv (socc_of_domain (rli_domain idx)) ->
    ftm f = Some (Ou_known (Mk_function_type cid ri)) ->
    funs f = Some (cid_act, ai) ->
    code_age_same_class E cid_act cid ->
    (forall f' T, ctm f' = Some T ->
        gamma E rho H T (V_clos l f')) ->
    (forall w T, vtm w = Some T ->
        exists vw, venv w = Some vw /\ gamma E rho H T vw) ->
    sat_ext E rho H eps ->
    gamma_hvnn E rho H
      (HV_closures (Mk_row_like_for_closures kc oc) am) (V_clos l f).
Proof.
  intros E rho H kc oc am l f ftm ctm vtm idx eps funs venv
    cid ri cid_act ai Hkc Hheap Hslots Hft Hfuns Hclass Hct Hvt Hext.
  apply GH_closures.
  apply GC_known
    with (ftm := ftm) (ctm := ctm) (vtm := vtm) (idx := idx)
         (eps := eps) (funs := funs) (venv := venv); auto.
  unfold function_type_code_ok. rewrite Hft.
  exists cid_act, ai. auto.
Qed.

(* ------------------------------------------------------------------ *)
(* Tier-1 pilot (Qed): a singleton constant type concretizes to        *)
(* exactly that constant                                               *)
(* ------------------------------------------------------------------ *)

Lemma gamma_singleton_naked_immediate :
  forall E rho H n v,
    gamma E rho H
      (FT_naked_immediate
         (Oub_ok (No_alias (Naked_immediates (n :: nil)))))
      v
    <-> v = V_naked_imm n.
Proof.
  intros E rho H n v; split.
  - intro Hg; inversion Hg; subst; cbn in *; try discriminate.
    match goal with
    | Hin : _ \/ False |- _ =>
        destruct Hin as [Heq | []]; subst; reflexivity
    end.
  - intro Hv; subst.
    apply G_naked_immediate.
    + cbn. left. reflexivity.
    + intros R N x Hm Hin. cbv in Hm. discriminate Hm.
Qed.
