(* ===================================================================== *)
(* MeetJoin.v -- flambda2 formalism, chapter 08: meet, join, provers,   *)
(* expand_head, reify.  Meet is modeled as a relational envelope: the   *)
(* inductive judgments below derive the results the documented          *)
(* algorithm can produce (plus sound slack where the docs leave the     *)
(* algorithm free); the soundness rules are stated against gamma from   *)
(* Concretization.v.                                                     *)
(*                                                                       *)
(* Owner: Scott (Team Hilbert).  Wave 4.                                 *)
(* Imports: Base, Syntax, Values, TypeGrammar, Concretization.           *)
(* ===================================================================== *)

From Stdlib Require Import ZArith List Bool.
From Flambda2 Require Import Base Syntax Values TypeGrammar
  Concretization.
Import ListNotations.
Open Scope Z_scope.

(* --------------------------------------------------------------------- *)
(* Results                                                                *)
(* --------------------------------------------------------------------- *)

(* FROZEN interface (team brief; CORRESPONDENCE encoding table): the
   judgment E |- T1 /\ T2 = T |> eps lands in meet_res.  The extension
   eps is the delta E' minus E of the doc's returned environment. *)
Inductive meet_res : Type :=
  | Meet_bottom
  | Meet_ok (T : ftype) (eps : env_extension).

(* Generic Or_bottom-with-extension for component meets (heads, arms). *)
Inductive mres (A : Type) : Type :=
  | Mres_bottom
  | Mres_ok (a : A) (eps : env_extension).
Arguments Mres_bottom {A}.
Arguments Mres_ok {A}.

(** RULE T.Meet.Dispatch (STATUS descriptive) -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet.ml#meet
    CODE middle_end/flambda2/ui/flambda_features.ml#use_n_way_join
    CODE middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join
    Premise: join_algorithm() = Binary (the default).  Conclusion: meet
    dispatches to Meet_and_join.meet (binary) and join-at-merge-points to
    Join_levels_old.cut_and_n_way_join (pairwise binary join); N_way
    selects Meet_and_n_way_join.meet and
    Join_env.cut_and_n_way_join_with_analysis, Checked runs both and
    diffs them, and -flambda2-meet-algorithm is a no-op.
    ENCODING NOTE: algorithm selection between two implementations of
    the same specification is pass plumbing with no counterpart among
    the objects modeled here; the judgments below model the shared
    specification (and the Binary algorithm's shapes), so this rule is
    recorded as a documented anchor. *)
Definition T_Meet_Dispatch_documented : Prop := True.

(* --------------------------------------------------------------------- *)
(* Small helpers                                                          *)
(* --------------------------------------------------------------------- *)

(* can_E(T): the canonical simple of T when T is an alias type. *)
Definition can_alias (E : tenv) (T : ftype) : option simple :=
  match ftype_alias_simple T with
  | Some s => Some (canonical E s)
  | None => None
  end.

(* Whether a type is the Bottom of its kind (mirror of
   ftype_is_unknown in Concretization.v). *)
Definition ftype_is_bottom (T : ftype) : bool :=
  match T with
  | FT_value Oub_bottom
  | FT_naked_immediate Oub_bottom
  | FT_naked_float Oub_bottom
  | FT_naked_float32 Oub_bottom
  | FT_naked_int8 Oub_bottom
  | FT_naked_int16 Oub_bottom
  | FT_naked_int32 Oub_bottom
  | FT_naked_int64 Oub_bottom
  | FT_naked_nativeint Oub_bottom
  | FT_naked_vec128 Oub_bottom
  | FT_naked_vec256 Oub_bottom
  | FT_naked_vec512 Oub_bottom
  | FT_rec_info Oub_bottom
  | FT_region Oub_bottom => true
  | _ => false
  end.

(* The type stored in E for the name underlying a simple.
   ENCODING NOTE: a constant simple is over-approximated to the
   unknown of its kind (its exact singleton type is the
   concretization-side reading, T.Env.Find.Canonical); expand_head
   below therefore excludes constant canonicals, keeping its
   preservation statement honest. *)
Definition stored_type (E : tenv) (k : kind) (s : simple) : ftype :=
  match simple_name s with
  | Some n => tenv_find E n k
  | None => unknown_of_kind k
  end.

(* Record the equation s : T in an extension.  Constants take no
   equation (they are never given stored types). *)
Definition record_on (s : simple) (T : ftype) (e : env_extension)
  : env_extension :=
  match simple_name s with
  | Some n => fupd name_eqb e n T
  | None => e
  end.

(* Union of two extensions.  ENCODING NOTE: left-biased on a shared
   name; the code meets the two equations instead.  Keeping one of the
   two is a gamma-enlargement of the conjunction, i.e. slack in the
   sound direction of T.Meet.Sound. *)
Definition ext_union (e1 e2 : env_extension) : env_extension :=
  fun n => match e1 n with
           | Some T => Some T
           | None => e2 n
           end.

(* Finite-set intersection/union, up to membership extensionality
   (sets-as-lists ENCODING NOTE, TypeGrammar.v). *)
Definition inter_spec {A : Type} (s1 s2 s : list A) : Prop :=
  forall x, In x s <-> In x s1 /\ In x s2.

Definition union_spec {A : Type} (s1 s2 s : list A) : Prop :=
  forall x, In x s <-> In x s1 \/ In x s2.

(* Nullability meet: Not_null absorbs.
   ENCODING NOTE: on Maybe_null/Maybe_null the code meets the is_null
   relation variables (aliasing them); the envelope keeps the left
   witness, sound for the superset direction. *)
Definition meet_null (n1 n2 : is_null_ty) : is_null_ty :=
  match n1, n2 with
  | Not_null, _ => Not_null
  | _, Not_null => Not_null
  | Maybe_null v1, Maybe_null _ => Maybe_null v1
  end.

(* Fold a met non-null part and a met nullability into a head result:
   Bottom non-null with Not_null is the empty head (meet_disjunction). *)
Definition mk_hv_res (nn : or_unknown_or_bottom head_value_non_null)
    (isnull : is_null_ty) (eps : env_extension) : mres head_value :=
  match nn, isnull with
  | Oub_bottom, Not_null => Mres_bottom
  | _, _ => Mres_ok (Mk_head_value nn isnull) eps
  end.

Definition hv_of_hvnn_res (r : mres head_value_non_null)
    (isnull : is_null_ty) : mres head_value :=
  match r with
  | Mres_bottom => mk_hv_res Oub_bottom isnull fempty
  | Mres_ok h eps => Mres_ok (Mk_head_value (Oub_ok h) isnull) eps
  end.

Definition hv_meet_res (r : mres head_value) : meet_res :=
  match r with
  | Mres_bottom => Meet_bottom
  | Mres_ok h eps => Meet_ok (FT_value (Oub_ok (No_alias h))) eps
  end.

(* Result shape of the alias-vs-concrete meet: the alias, with the
   refined type recorded on the canonical in eps. *)
Definition alias_res (k : kind) (s : simple) (r : meet_res)
  : meet_res :=
  match r with
  | Meet_bottom => Meet_bottom
  | Meet_ok U eps => Meet_ok (alias_type_of k s) (record_on s U eps)
  end.

(* Result shape of the alias-vs-alias meet: keep s1 (the
   earlier-bound), demote s2 to an alias of s1, and record the met
   underlying type on s1. *)
Definition alias_alias_res (k : kind) (s1 s2 : simple) (r : meet_res)
  : meet_res :=
  match r with
  | Meet_bottom => Meet_bottom
  | Meet_ok U eps =>
      Meet_ok (alias_type_of k s1)
        (record_on s2 (alias_type_of k s1) (record_on s1 U eps))
  end.

(* Top-constructor discriminator on non-null value heads, for the
   incompatibility rule. *)
Definition hvnn_code (h : head_value_non_null) : nat :=
  match h with
  | HV_variant _ _ _ _ _ _ => 0%nat
  | HV_mutable_block _ => 1%nat
  | HV_boxed_float32 _ _ => 2%nat
  | HV_boxed_float _ _ => 3%nat
  | HV_boxed_int32 _ _ => 4%nat
  | HV_boxed_int64 _ _ => 5%nat
  | HV_boxed_nativeint _ _ => 6%nat
  | HV_boxed_vec128 _ _ => 7%nat
  | HV_boxed_vec256 _ _ => 8%nat
  | HV_boxed_vec512 _ _ => 9%nat
  | HV_closures _ _ => 10%nat
  | HV_string _ => 11%nat
  | HV_array _ _ _ _ => 12%nat
  end.

(* Alloc-mode meet.  ENCODING NOTE: Heap vs Local is left unrelated
   (meet_alloc_mode has no Bottom result; the incompatible pair does
   not arise for well-kinded inputs and the docs state no result for
   it). *)
Inductive meet_amt
  : alloc_mode_types -> alloc_mode_types -> alloc_mode_types -> Prop :=
  | MA_same : forall a, meet_amt a a a
  | MA_hol_l : forall a, meet_amt AMT_heap_or_local a a
  | MA_hol_r : forall a, meet_amt a AMT_heap_or_local a.

(* Replace the contents type of a boxed head (identity on non-boxed). *)
Definition set_boxed_contents (h : head_value_non_null) (t : ftype)
  : head_value_non_null :=
  match h with
  | HV_boxed_float32 _ am => HV_boxed_float32 t am
  | HV_boxed_float _ am => HV_boxed_float t am
  | HV_boxed_int32 _ am => HV_boxed_int32 t am
  | HV_boxed_int64 _ am => HV_boxed_int64 t am
  | HV_boxed_nativeint _ am => HV_boxed_nativeint t am
  | HV_boxed_vec128 _ am => HV_boxed_vec128 t am
  | HV_boxed_vec256 _ am => HV_boxed_vec256 t am
  | HV_boxed_vec512 _ am => HV_boxed_vec512 t am
  | _ => h
  end.

(* A bottom arm re-embedded into the variant: the immediates arm as
   the Bottom naked-immediate type, the blocks arm as the empty
   row-like (no known tags, other_tags Bottom). *)
Definition imm_arm_of (r : or_bottom (or_unknown ftype))
  : or_unknown ftype :=
  match r with
  | Ob_bottom => Ou_known (bottom_of_kind K_naked_immediate)
  | Ob_ok a => a
  end.

Definition blocks_arm_of
    (r : or_bottom (or_unknown row_like_for_blocks))
  : or_unknown row_like_for_blocks :=
  match r with
  | Ob_bottom =>
      Ou_known
        (Mk_row_like_for_blocks fempty Ob_bottom AMT_heap_or_local)
  | Ob_ok a => a
  end.

(* Rebuild a met variant; Bottom iff both arms are Bottom (the rule
   T.Meet.Variant's bottom clause).
   ENCODING NOTE: the is_int/get_tag witnesses and the extensions are
   taken from the LEFT input (the code aliases the relation variables
   and joins extensions across surviving disjuncts); keeping the left
   data is slack in the sound (superset) direction. *)
Definition variant_res (ii : option variable) (gt : option variable)
    (ext : variant_extensions)
    (rimm : or_bottom (or_unknown ftype))
    (rbl : or_bottom (or_unknown row_like_for_blocks))
    (u : bool) (eps : env_extension) : mres head_value_non_null :=
  match rimm, rbl with
  | Ob_bottom, Ob_bottom => Mres_bottom
  | _, _ =>
      Mres_ok
        (HV_variant ii (imm_arm_of rimm) gt (blocks_arm_of rbl) ext u)
        eps
  end.

(* The empty row-like: no known tags and Bottom other_tags (gamma of
   its blocks arm is empty; used by the missed-bottom witness and the
   immediates-only prover cases). *)
Definition rlfb_empty (r : row_like_for_blocks) : Prop :=
  match r with
  | Mk_row_like_for_blocks kt ot _ =>
      (forall t, kt t = None) /\ ot = Ob_bottom
  end.

(* --------------------------------------------------------------------- *)
(* Block shapes and row-like indices                                      *)
(* --------------------------------------------------------------------- *)

(** RULE T.Meet.BlockShape (STATUS normative) -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet_and_join.ml#meet_row_like_for_blocks
    CODE middle_end/flambda2/types/meet_and_join.ml#join_row_like_for_blocks
    CODE middle_end/flambda2/types/grammar/more_type_creators.ml#unknown_from_shape
    CODE middle_end/flambda2/kinds/flambda_kind.ml#Block_shape.equal
    VERIFIED 14-validation/mixed-04-join.md
    Meeting two block cases with shapes sigma1, sigma2: if
    Block_shape.equal then the met index keeps sigma1 (fields met
    pointwise), otherwise that index pair is Bottom.  Joining: equal
    keeps sigma1, unequal drops the shape to Unknown.  Both halves of
    the rule are here (meet_shape / join_shape below); Block_shape.equal
    is exact (no subshaping) and is encoded as propositional equality
    on block_shape.  The unknown_from_shape per-field refill on the
    join side is not modeled (the row-like join is left unrelated
    below). *)
Inductive meet_shape
  : block_shape -> block_shape -> or_bottom block_shape -> Prop :=
  | MS_equal : forall s, meet_shape s s (Ob_ok s)
  | MS_diff : forall s1 s2, s1 <> s2 -> meet_shape s1 s2 Ob_bottom.

Inductive join_shape
  : block_shape -> block_shape -> or_unknown block_shape -> Prop :=
  | JS_equal : forall s, join_shape s s (Ou_known s)
  | JS_diff : forall s1 s2, s1 <> s2 -> join_shape s1 s2 Ou_unknown.

(* Meet of row-like index domains over Block_size (Known = singleton,
   At_least = upward cone; ch. 07 T.Grammar.RowLike.Index). *)
Inductive meet_index_domain
  : row_like_index_domain block_size
  -> row_like_index_domain block_size
  -> or_bottom (row_like_index_domain block_size) -> Prop :=
  | MID_known_known : forall l,
      meet_index_domain (Rl_known l) (Rl_known l) (Ob_ok (Rl_known l))
  | MID_known_known_diff : forall l1 l2,
      l1 <> l2 ->
      meet_index_domain (Rl_known l1) (Rl_known l2) Ob_bottom
  | MID_known_at_least : forall l l',
      l' <= l ->
      meet_index_domain (Rl_known l) (Rl_at_least l')
        (Ob_ok (Rl_known l))
  | MID_known_at_least_bottom : forall l l',
      l < l' ->
      meet_index_domain (Rl_known l) (Rl_at_least l') Ob_bottom
  | MID_at_least_known : forall l l',
      l <= l' ->
      meet_index_domain (Rl_at_least l) (Rl_known l')
        (Ob_ok (Rl_known l'))
  | MID_at_least_known_bottom : forall l l',
      l' < l ->
      meet_index_domain (Rl_at_least l) (Rl_known l') Ob_bottom
  | MID_at_least : forall l1 l2,
      meet_index_domain (Rl_at_least l1) (Rl_at_least l2)
        (Ob_ok (Rl_at_least (Z.max l1 l2))).

Inductive meet_index
  : row_like_index block_size block_shape
  -> row_like_index block_size block_shape
  -> or_bottom (row_like_index block_size block_shape) -> Prop :=
  | MI_ok : forall d1 d2 d s1 s2 s,
      meet_index_domain d1 d2 (Ob_ok d) ->
      meet_shape s1 s2 (Ob_ok s) ->
      meet_index (Mk_row_like_index d1 s1) (Mk_row_like_index d2 s2)
        (Ob_ok (Mk_row_like_index d s))
  | MI_domain_bottom : forall d1 d2 s1 s2,
      meet_index_domain d1 d2 Ob_bottom ->
      meet_index (Mk_row_like_index d1 s1) (Mk_row_like_index d2 s2)
        Ob_bottom
  | MI_shape_bottom : forall d1 d2 s1 s2,
      meet_shape s1 s2 Ob_bottom ->
      meet_index (Mk_row_like_index d1 s1) (Mk_row_like_index d2 s2)
        Ob_bottom.

(* --------------------------------------------------------------------- *)
(* Naked-number set meet / join                                           *)
(* --------------------------------------------------------------------- *)

(** RULE T.Meet.NakedNumber (STATUS normative) -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet_and_join.ml#meet_expanded_head0
    CODE middle_end/flambda2/types/meet_and_join.ml#set_meet
    Premise: T1, T2 are heads of the same naked-number kind, denoting
    immediate sets S1, S2.  Conclusion: E |- T1 /\ T2 = the head for
    the intersection of S1 and S2, with empty extension; Bottom if the
    intersection is empty.  Per kind: naked immediates plus
    floats/float32/int8/16/32/64/nativeint/vec128/256/512.
    ENCODING NOTE: naked-immediate heads carrying inverse relations
    are left unrelated here; their meet additionally runs the
    relational reducer, recorded at the T.Meet.Relational anchor
    below. *)
Inductive naked_set_meet : ftype -> ftype -> meet_res -> Prop :=
  | NSM_imm : forall s1 s2 s,
      inter_spec s1 s2 s ->
      head_nonempty s ->
      naked_set_meet
        (FT_naked_immediate
          (Oub_ok (No_alias (Naked_immediates s1))))
        (FT_naked_immediate
          (Oub_ok (No_alias (Naked_immediates s2))))
        (Meet_ok
          (FT_naked_immediate
            (Oub_ok (No_alias (Naked_immediates s))))
          fempty)
  | NSM_imm_bottom : forall s1 s2,
      inter_spec s1 s2 (@nil Z) ->
      naked_set_meet
        (FT_naked_immediate
          (Oub_ok (No_alias (Naked_immediates s1))))
        (FT_naked_immediate
          (Oub_ok (No_alias (Naked_immediates s2))))
        Meet_bottom
  | NSM_float : forall (s1 s2 s : head_naked_float),
      inter_spec s1 s2 s ->
      head_nonempty s ->
      naked_set_meet
        (FT_naked_float (Oub_ok (No_alias s1)))
        (FT_naked_float (Oub_ok (No_alias s2)))
        (Meet_ok (FT_naked_float (Oub_ok (No_alias s))) fempty)
  | NSM_float_bottom : forall (s1 s2 : head_naked_float),
      inter_spec s1 s2 nil ->
      naked_set_meet
        (FT_naked_float (Oub_ok (No_alias s1)))
        (FT_naked_float (Oub_ok (No_alias s2)))
        Meet_bottom
  | NSM_float32 : forall (s1 s2 s : head_naked_float32),
      inter_spec s1 s2 s ->
      head_nonempty s ->
      naked_set_meet
        (FT_naked_float32 (Oub_ok (No_alias s1)))
        (FT_naked_float32 (Oub_ok (No_alias s2)))
        (Meet_ok (FT_naked_float32 (Oub_ok (No_alias s))) fempty)
  | NSM_float32_bottom : forall (s1 s2 : head_naked_float32),
      inter_spec s1 s2 nil ->
      naked_set_meet
        (FT_naked_float32 (Oub_ok (No_alias s1)))
        (FT_naked_float32 (Oub_ok (No_alias s2)))
        Meet_bottom
  | NSM_int8 : forall (s1 s2 s : head_naked_int8),
      inter_spec s1 s2 s ->
      head_nonempty s ->
      naked_set_meet
        (FT_naked_int8 (Oub_ok (No_alias s1)))
        (FT_naked_int8 (Oub_ok (No_alias s2)))
        (Meet_ok (FT_naked_int8 (Oub_ok (No_alias s))) fempty)
  | NSM_int8_bottom : forall (s1 s2 : head_naked_int8),
      inter_spec s1 s2 nil ->
      naked_set_meet
        (FT_naked_int8 (Oub_ok (No_alias s1)))
        (FT_naked_int8 (Oub_ok (No_alias s2)))
        Meet_bottom
  | NSM_int16 : forall (s1 s2 s : head_naked_int16),
      inter_spec s1 s2 s ->
      head_nonempty s ->
      naked_set_meet
        (FT_naked_int16 (Oub_ok (No_alias s1)))
        (FT_naked_int16 (Oub_ok (No_alias s2)))
        (Meet_ok (FT_naked_int16 (Oub_ok (No_alias s))) fempty)
  | NSM_int16_bottom : forall (s1 s2 : head_naked_int16),
      inter_spec s1 s2 nil ->
      naked_set_meet
        (FT_naked_int16 (Oub_ok (No_alias s1)))
        (FT_naked_int16 (Oub_ok (No_alias s2)))
        Meet_bottom
  | NSM_int32 : forall (s1 s2 s : head_naked_int32),
      inter_spec s1 s2 s ->
      head_nonempty s ->
      naked_set_meet
        (FT_naked_int32 (Oub_ok (No_alias s1)))
        (FT_naked_int32 (Oub_ok (No_alias s2)))
        (Meet_ok (FT_naked_int32 (Oub_ok (No_alias s))) fempty)
  | NSM_int32_bottom : forall (s1 s2 : head_naked_int32),
      inter_spec s1 s2 nil ->
      naked_set_meet
        (FT_naked_int32 (Oub_ok (No_alias s1)))
        (FT_naked_int32 (Oub_ok (No_alias s2)))
        Meet_bottom
  | NSM_int64 : forall (s1 s2 s : head_naked_int64),
      inter_spec s1 s2 s ->
      head_nonempty s ->
      naked_set_meet
        (FT_naked_int64 (Oub_ok (No_alias s1)))
        (FT_naked_int64 (Oub_ok (No_alias s2)))
        (Meet_ok (FT_naked_int64 (Oub_ok (No_alias s))) fempty)
  | NSM_int64_bottom : forall (s1 s2 : head_naked_int64),
      inter_spec s1 s2 nil ->
      naked_set_meet
        (FT_naked_int64 (Oub_ok (No_alias s1)))
        (FT_naked_int64 (Oub_ok (No_alias s2)))
        Meet_bottom
  | NSM_nativeint : forall (s1 s2 s : head_naked_nativeint),
      inter_spec s1 s2 s ->
      head_nonempty s ->
      naked_set_meet
        (FT_naked_nativeint (Oub_ok (No_alias s1)))
        (FT_naked_nativeint (Oub_ok (No_alias s2)))
        (Meet_ok (FT_naked_nativeint (Oub_ok (No_alias s))) fempty)
  | NSM_nativeint_bottom : forall (s1 s2 : head_naked_nativeint),
      inter_spec s1 s2 nil ->
      naked_set_meet
        (FT_naked_nativeint (Oub_ok (No_alias s1)))
        (FT_naked_nativeint (Oub_ok (No_alias s2)))
        Meet_bottom
  | NSM_vec128 : forall (s1 s2 s : head_naked_vec128),
      inter_spec s1 s2 s ->
      head_nonempty s ->
      naked_set_meet
        (FT_naked_vec128 (Oub_ok (No_alias s1)))
        (FT_naked_vec128 (Oub_ok (No_alias s2)))
        (Meet_ok (FT_naked_vec128 (Oub_ok (No_alias s))) fempty)
  | NSM_vec128_bottom : forall (s1 s2 : head_naked_vec128),
      inter_spec s1 s2 nil ->
      naked_set_meet
        (FT_naked_vec128 (Oub_ok (No_alias s1)))
        (FT_naked_vec128 (Oub_ok (No_alias s2)))
        Meet_bottom
  | NSM_vec256 : forall (s1 s2 s : head_naked_vec256),
      inter_spec s1 s2 s ->
      head_nonempty s ->
      naked_set_meet
        (FT_naked_vec256 (Oub_ok (No_alias s1)))
        (FT_naked_vec256 (Oub_ok (No_alias s2)))
        (Meet_ok (FT_naked_vec256 (Oub_ok (No_alias s))) fempty)
  | NSM_vec256_bottom : forall (s1 s2 : head_naked_vec256),
      inter_spec s1 s2 nil ->
      naked_set_meet
        (FT_naked_vec256 (Oub_ok (No_alias s1)))
        (FT_naked_vec256 (Oub_ok (No_alias s2)))
        Meet_bottom
  | NSM_vec512 : forall (s1 s2 s : head_naked_vec512),
      inter_spec s1 s2 s ->
      head_nonempty s ->
      naked_set_meet
        (FT_naked_vec512 (Oub_ok (No_alias s1)))
        (FT_naked_vec512 (Oub_ok (No_alias s2)))
        (Meet_ok (FT_naked_vec512 (Oub_ok (No_alias s))) fempty)
  | NSM_vec512_bottom : forall (s1 s2 : head_naked_vec512),
      inter_spec s1 s2 nil ->
      naked_set_meet
        (FT_naked_vec512 (Oub_ok (No_alias s1)))
        (FT_naked_vec512 (Oub_ok (No_alias s2)))
        Meet_bottom.

(* Naked-number set join (the finite-set-union clause of the join;
   the covering rule ids are T.Join.Sound / T.Join.Head).  Naked
   immediates join through their plain-set form only; heads with
   inverse relations are left unrelated (join keeps relations only if
   syntactically equal, per T.Join.Head NOTES). *)
Inductive naked_set_join : ftype -> ftype -> ftype -> Prop :=
  | NSJ_imm : forall s1 s2 s,
      union_spec s1 s2 s ->
      naked_set_join
        (FT_naked_immediate
          (Oub_ok (No_alias (Naked_immediates s1))))
        (FT_naked_immediate
          (Oub_ok (No_alias (Naked_immediates s2))))
        (FT_naked_immediate
          (Oub_ok (No_alias (Naked_immediates s))))
  | NSJ_float : forall (s1 s2 s : head_naked_float),
      union_spec s1 s2 s ->
      naked_set_join
        (FT_naked_float (Oub_ok (No_alias s1)))
        (FT_naked_float (Oub_ok (No_alias s2)))
        (FT_naked_float (Oub_ok (No_alias s)))
  | NSJ_float32 : forall (s1 s2 s : head_naked_float32),
      union_spec s1 s2 s ->
      naked_set_join
        (FT_naked_float32 (Oub_ok (No_alias s1)))
        (FT_naked_float32 (Oub_ok (No_alias s2)))
        (FT_naked_float32 (Oub_ok (No_alias s)))
  | NSJ_int8 : forall (s1 s2 s : head_naked_int8),
      union_spec s1 s2 s ->
      naked_set_join
        (FT_naked_int8 (Oub_ok (No_alias s1)))
        (FT_naked_int8 (Oub_ok (No_alias s2)))
        (FT_naked_int8 (Oub_ok (No_alias s)))
  | NSJ_int16 : forall (s1 s2 s : head_naked_int16),
      union_spec s1 s2 s ->
      naked_set_join
        (FT_naked_int16 (Oub_ok (No_alias s1)))
        (FT_naked_int16 (Oub_ok (No_alias s2)))
        (FT_naked_int16 (Oub_ok (No_alias s)))
  | NSJ_int32 : forall (s1 s2 s : head_naked_int32),
      union_spec s1 s2 s ->
      naked_set_join
        (FT_naked_int32 (Oub_ok (No_alias s1)))
        (FT_naked_int32 (Oub_ok (No_alias s2)))
        (FT_naked_int32 (Oub_ok (No_alias s)))
  | NSJ_int64 : forall (s1 s2 s : head_naked_int64),
      union_spec s1 s2 s ->
      naked_set_join
        (FT_naked_int64 (Oub_ok (No_alias s1)))
        (FT_naked_int64 (Oub_ok (No_alias s2)))
        (FT_naked_int64 (Oub_ok (No_alias s)))
  | NSJ_nativeint : forall (s1 s2 s : head_naked_nativeint),
      union_spec s1 s2 s ->
      naked_set_join
        (FT_naked_nativeint (Oub_ok (No_alias s1)))
        (FT_naked_nativeint (Oub_ok (No_alias s2)))
        (FT_naked_nativeint (Oub_ok (No_alias s)))
  | NSJ_vec128 : forall (s1 s2 s : head_naked_vec128),
      union_spec s1 s2 s ->
      naked_set_join
        (FT_naked_vec128 (Oub_ok (No_alias s1)))
        (FT_naked_vec128 (Oub_ok (No_alias s2)))
        (FT_naked_vec128 (Oub_ok (No_alias s)))
  | NSJ_vec256 : forall (s1 s2 s : head_naked_vec256),
      union_spec s1 s2 s ->
      naked_set_join
        (FT_naked_vec256 (Oub_ok (No_alias s1)))
        (FT_naked_vec256 (Oub_ok (No_alias s2)))
        (FT_naked_vec256 (Oub_ok (No_alias s)))
  | NSJ_vec512 : forall (s1 s2 s : head_naked_vec512),
      union_spec s1 s2 s ->
      naked_set_join
        (FT_naked_vec512 (Oub_ok (No_alias s1)))
        (FT_naked_vec512 (Oub_ok (No_alias s2)))
        (FT_naked_vec512 (Oub_ok (No_alias s))).

(* --------------------------------------------------------------------- *)
(* The meet judgment (mutual)                                             *)
(* --------------------------------------------------------------------- *)

(* FROZEN interface: meet : tenv -> ftype -> ftype -> meet_res -> Prop.
   ENCODING NOTE (relational envelope): meet is a relation, not a
   function -- it derives every result the documented algorithm may
   return.  Corners the docs leave to the algorithm (componentwise
   meets of arrays and closures, multi-tag / other_tags row-like
   meets) are left unrelated: no constructor covers them, so nothing
   is claimed about their results.  Extensions are transcribed exactly
   where the docs state them and are otherwise empty. *)
Inductive meet (E : tenv) : ftype -> ftype -> meet_res -> Prop :=
  | M_unknown_l : forall T1 T2,
      ftype_is_unknown T1 = true ->
      meet E T1 T2 (Meet_ok T2 fempty)
  | M_unknown_r : forall T1 T2,
      ftype_is_unknown T2 = true ->
      meet E T1 T2 (Meet_ok T1 fempty)
  | M_bottom_l : forall T1 T2,
      ftype_is_bottom T1 = true ->
      meet E T1 T2 Meet_bottom
  | M_bottom_r : forall T1 T2,
      ftype_is_bottom T2 = true ->
      meet E T1 T2 Meet_bottom
  (** RULE T.Meet.AliasAlias (STATUS normative) -- 08-meet-join.md
      CODE middle_end/flambda2/types/env/meet_env.ml#meet
      CODE middle_end/flambda2/types/env/meet_env.ml#add_alias_between_canonicals
      Premises in doc order: can_E(T1) = s1; can_E(T2) = s2; the
      extension records the alias equation s1 = s2 (demoting the
      later-bound to the earlier, by binding time); the underlying
      types of s1 and s2 are met, contributing further equations.
      Conclusion: E |- T1 /\ T2 = (= s1) |> eps (returning either
      alias is sound; the envelope keeps the earlier-bound s1, with
      the wlog premise on binding times selecting it).
      The s1 = s2 case is not excluded: there the derived extension's
      outer record_on s2 overwrites the inner equation on s1 with a
      self-alias, dropping the met underlying type, where the code's
      equal-canonicals path returns with no equation at all -- coarser
      than the code but in the sound direction.  (Sound relative to
      sat_ext's simple_eval reading of names; under the earlier
      literal-rho reading the self-alias equation on a symbol was
      unsatisfiable -- KF-051.) *)
  | M_alias_alias : forall T1 T2 s1 s2 k r,
      can_alias E T1 = Some s1 ->
      can_alias E T2 = Some s2 ->
      kind_of_ftype T1 = k ->
      (binding_time_of_simple E s1 <= binding_time_of_simple E s2)%nat ->
      meet E (stored_type E k s1) (stored_type E k s2) r ->
      meet E T1 T2 (alias_alias_res k s1 s2 r)
  (** RULE T.Meet.AliasConcrete (STATUS normative) -- 08-meet-join.md
      CODE middle_end/flambda2/types/env/meet_env.ml#meet
      CODE middle_end/flambda2/types/env/meet_env.ml#add_concrete_equation_on_canonical
      Premises in doc order: can_E(T1) = s1; T2 is a non-alias
      (concrete) type; eps refines the type of s1 by meeting its
      existing type with T2.  Conclusion: E |- T1 /\ T2 = (= s1) |>
      eps.  The two constructors below are this rule and its stated
      symmetric case (concrete on the left returns (= s2)). *)
  | M_alias_concrete_l : forall T1 T2 s1 k r,
      can_alias E T1 = Some s1 ->
      ftype_alias_simple T2 = None ->
      kind_of_ftype T2 = k ->
      meet E (stored_type E k s1) T2 r ->
      meet E T1 T2 (alias_res k s1 r)
  | M_alias_concrete_r : forall T1 T2 s2 k r,
      ftype_alias_simple T1 = None ->
      can_alias E T2 = Some s2 ->
      kind_of_ftype T1 = k ->
      meet E T1 (stored_type E k s2) r ->
      meet E T1 T2 (alias_res k s2 r)
  | M_naked : forall T1 T2 r,
      naked_set_meet T1 T2 r ->
      meet E T1 T2 r
  | M_value : forall h1 h2 r,
      meet_hv E h1 h2 r ->
      meet E (FT_value (Oub_ok (No_alias h1)))
        (FT_value (Oub_ok (No_alias h2))) (hv_meet_res r)

(* Value heads: nullability meets pointwise with the non-null part
   (meet_disjunction; the doc paragraph before T.Meet.Variant). *)
with meet_hv (E : tenv)
  : head_value -> head_value -> mres head_value -> Prop :=
  | MHV_unknown_l : forall n1 nn2 n2,
      meet_hv E (Mk_head_value Oub_unknown n1) (Mk_head_value nn2 n2)
        (mk_hv_res nn2 (meet_null n1 n2) fempty)
  | MHV_unknown_r : forall nn1 n1 n2,
      meet_hv E (Mk_head_value nn1 n1) (Mk_head_value Oub_unknown n2)
        (mk_hv_res nn1 (meet_null n1 n2) fempty)
  | MHV_bottom_l : forall n1 nn2 n2,
      meet_hv E (Mk_head_value Oub_bottom n1) (Mk_head_value nn2 n2)
        (mk_hv_res Oub_bottom (meet_null n1 n2) fempty)
  | MHV_bottom_r : forall nn1 n1 n2,
      meet_hv E (Mk_head_value nn1 n1) (Mk_head_value Oub_bottom n2)
        (mk_hv_res Oub_bottom (meet_null n1 n2) fempty)
  | MHV_heads : forall h1 n1 h2 n2 r,
      meet_hvnn E h1 h2 r ->
      meet_hv E (Mk_head_value (Oub_ok h1) n1)
        (Mk_head_value (Oub_ok h2) n2)
        (hv_of_hvnn_res r (meet_null n1 n2))

with meet_hvnn (E : tenv)
  : head_value_non_null -> head_value_non_null
  -> mres head_value_non_null -> Prop :=
  (** RULE T.Meet.ValueHeadIncompatible (STATUS normative)
      -- 08-meet-join.md
      CODE middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null
      Premises in doc order: T1, T2 are non-null value heads with
      different top constructors (e.g. Variant vs Boxed_float, String
      vs Closures); excluding the Variant/Mutable_block pair, handled
      specially (the mutable-block constructors below).  Conclusion:
      E |- T1 /\ T2 = Bottom.  Boxed numbers of matching flavour,
      closures, strings and arrays meet componentwise (boxed/string
      below; arrays and closures left unrelated -- see the envelope
      note on meet). *)
  | MH_incompatible : forall h1 h2,
      hvnn_code h1 <> hvnn_code h2 ->
      ~ (hvnn_code h1 = 0%nat /\ hvnn_code h2 = 1%nat) ->
      ~ (hvnn_code h1 = 1%nat /\ hvnn_code h2 = 0%nat) ->
      meet_hvnn E h1 h2 Mres_bottom
  (** RULE T.Meet.Variant (STATUS normative) -- 08-meet-join.md
      CODE middle_end/flambda2/types/meet_and_join.ml#meet_variant
      CODE middle_end/flambda2/types/meet_and_join.ml#meet_relation
      Premise: meeting Variant{is_int1, get_tag1, blocks1, imms1,
      ext1} with Variant{is_int2, ...}.  Conclusions in doc order:
      immediates := imms1 /\ imms2; blocks := blocks1 /\ blocks2
      (row-like meet); is_int and get_tag relation variables are
      aliased (meet_relation); extensions joined across the surviving
      disjuncts (meet_disjunction); the result is Bottom iff both the
      immediates and the blocks components are Bottom (variant_res);
      is_unique is the OR of the inputs.
      ENCODING NOTE: relation-variable aliasing and the
      extension-join are collapsed to keeping the left input's
      witnesses/extensions (see variant_res); get_tag's
      inside-the-disjunction placement is algorithmic detail not
      separately modeled. *)
  | MH_variant_ok : forall ii1 imms1 gt1 bl1 ext1 u1
        ii2 imms2 gt2 bl2 ext2 u2 rimm epsi rbl epsb,
      meet_imms_arm E imms1 imms2 rimm epsi ->
      meet_blocks_arm E bl1 bl2 rbl epsb ->
      meet_hvnn E
        (HV_variant ii1 imms1 gt1 bl1 ext1 u1)
        (HV_variant ii2 imms2 gt2 bl2 ext2 u2)
        (variant_res ii1 gt1 ext1 rimm rbl (orb u1 u2)
          (ext_union epsi epsb))
  (* Variant vs Mutable_block: Mutable_block is treated as the more
     precise -- only the alloc modes are met (T.Meet.ValueHeadIncompatible
     NOTES; sharpened by T.Meet.MutableBlockMissedBottom below). *)
  | MH_variant_mutable_known : forall ii imms gt kt ot am1 ext u
        am2 am,
      meet_amt am1 am2 am ->
      meet_hvnn E
        (HV_variant ii imms gt
          (Ou_known (Mk_row_like_for_blocks kt ot am1)) ext u)
        (HV_mutable_block am2)
        (Mres_ok (HV_mutable_block am) fempty)
  | MH_variant_mutable_top : forall ii imms gt ext u am2,
      meet_hvnn E
        (HV_variant ii imms gt Ou_unknown ext u)
        (HV_mutable_block am2)
        (Mres_ok (HV_mutable_block am2) fempty)
  | MH_mutable_variant_known : forall am1 ii imms gt kt ot am2 ext u
        am,
      meet_amt am1 am2 am ->
      meet_hvnn E
        (HV_mutable_block am1)
        (HV_variant ii imms gt
          (Ou_known (Mk_row_like_for_blocks kt ot am2)) ext u)
        (Mres_ok (HV_mutable_block am) fempty)
  | MH_mutable_variant_top : forall am1 ii imms gt ext u,
      meet_hvnn E
        (HV_mutable_block am1)
        (HV_variant ii imms gt Ou_unknown ext u)
        (Mres_ok (HV_mutable_block am1) fempty)
  | MH_mutable_mutable : forall am1 am2 am,
      meet_amt am1 am2 am ->
      meet_hvnn E (HV_mutable_block am1) (HV_mutable_block am2)
        (Mres_ok (HV_mutable_block am) fempty)
  (* Boxed numbers of matching flavour meet on their contents.
     ENCODING NOTE: set_boxed_contents h1 keeps the left head's alloc
     mode where the code meets the two modes; kept-left slack, sound
     for the superset direction. *)
  | MH_boxed : forall h1 h2 k t1 t2 t eps,
      boxed_view h1 = Some (k, t1) ->
      boxed_view h2 = Some (k, t2) ->
      meet E t1 t2 (Meet_ok t eps) ->
      meet_hvnn E h1 h2 (Mres_ok (set_boxed_contents h1 t) eps)
  | MH_boxed_bottom : forall h1 h2 k t1 t2,
      boxed_view h1 = Some (k, t1) ->
      boxed_view h2 = Some (k, t2) ->
      meet E t1 t2 Meet_bottom ->
      meet_hvnn E h1 h2 Mres_bottom
  (* Strings meet by String_info set intersection. *)
  | MH_string : forall i1 i2 i,
      inter_spec i1 i2 i ->
      head_nonempty i ->
      meet_hvnn E (HV_string i1) (HV_string i2)
        (Mres_ok (HV_string i) fempty)
  | MH_string_bottom : forall i1 i2,
      inter_spec i1 i2 (@nil string_info) ->
      meet_hvnn E (HV_string i1) (HV_string i2) Mres_bottom

(* The immediates arm of a variant meet: Unknown absorbs; Known meets
   the naked-immediate types. *)
with meet_imms_arm (E : tenv)
  : or_unknown ftype -> or_unknown ftype
  -> or_bottom (or_unknown ftype) -> env_extension -> Prop :=
  | MIA_unknown_l : forall a,
      meet_imms_arm E Ou_unknown a (Ob_ok a) fempty
  | MIA_unknown_r : forall a,
      meet_imms_arm E a Ou_unknown (Ob_ok a) fempty
  | MIA_known : forall t1 t2 t eps,
      meet E t1 t2 (Meet_ok t eps) ->
      meet_imms_arm E (Ou_known t1) (Ou_known t2)
        (Ob_ok (Ou_known t)) eps
  | MIA_bottom : forall t1 t2,
      meet E t1 t2 Meet_bottom ->
      meet_imms_arm E (Ou_known t1) (Ou_known t2) Ob_bottom fempty

with meet_blocks_arm (E : tenv)
  : or_unknown row_like_for_blocks -> or_unknown row_like_for_blocks
  -> or_bottom (or_unknown row_like_for_blocks) -> env_extension
  -> Prop :=
  | MBA_unknown_l : forall a,
      meet_blocks_arm E Ou_unknown a (Ob_ok a) fempty
  | MBA_unknown_r : forall a,
      meet_blocks_arm E a Ou_unknown (Ob_ok a) fempty
  | MBA_known : forall r1 r2 r,
      meet_rlfb E r1 r2 (Ob_ok r) ->
      meet_blocks_arm E (Ou_known r1) (Ou_known r2)
        (Ob_ok (Ou_known r)) fempty
  | MBA_bottom : forall r1 r2,
      meet_rlfb E r1 r2 Ob_bottom ->
      meet_blocks_arm E (Ou_known r1) (Ou_known r2) Ob_bottom fempty

(* Row-like meet, single-known-tag fragment.
   ENCODING NOTE: meets involving several known tags, differing single
   tags, or a non-Bottom other_tags case are left unrelated (incomplete
   row-like corners the docs flag at T.Meet.GreatestLowerBound). *)
with meet_rlfb (E : tenv)
  : row_like_for_blocks -> row_like_for_blocks
  -> or_bottom row_like_for_blocks -> Prop :=
  | MRL_single : forall t c1 c2 c am1 am2 am,
      meet_block_case E c1 c2 (Ob_ok c) ->
      meet_amt am1 am2 am ->
      meet_rlfb E
        (Mk_row_like_for_blocks
          (fupd tag_eqb fempty t (Ou_known c1)) Ob_bottom am1)
        (Mk_row_like_for_blocks
          (fupd tag_eqb fempty t (Ou_known c2)) Ob_bottom am2)
        (Ob_ok (Mk_row_like_for_blocks
          (fupd tag_eqb fempty t (Ou_known c)) Ob_bottom am))
  | MRL_single_bottom : forall t c1 c2 am1 am2,
      meet_block_case E c1 c2 Ob_bottom ->
      meet_rlfb E
        (Mk_row_like_for_blocks
          (fupd tag_eqb fempty t (Ou_known c1)) Ob_bottom am1)
        (Mk_row_like_for_blocks
          (fupd tag_eqb fempty t (Ou_known c2)) Ob_bottom am2)
        Ob_bottom

(* Block-case meet: indices met per T.Meet.BlockShape, fields met
   pointwise.  ENCODING NOTE: the case env_extension is kept from the
   left input (the code meets the two extensions); gamma-enlarging,
   sound direction. *)
with meet_block_case (E : tenv)
  : row_like_block_case -> row_like_block_case
  -> or_bottom row_like_block_case -> Prop :=
  | MBC_ok : forall f1 f2 f idx1 idx2 idx e1 e2,
      meet_index idx1 idx2 (Ob_ok idx) ->
      meet_fields E f1 f2 f ->
      meet_block_case E
        (Mk_row_like_block_case f1 idx1 e1)
        (Mk_row_like_block_case f2 idx2 e2)
        (Ob_ok (Mk_row_like_block_case f idx e1))
  | MBC_index_bottom : forall f1 f2 idx1 idx2 e1 e2,
      meet_index idx1 idx2 Ob_bottom ->
      meet_block_case E
        (Mk_row_like_block_case f1 idx1 e1)
        (Mk_row_like_block_case f2 idx2 e2)
        Ob_bottom

(* Pointwise field meet.  ENCODING NOTE: per-field extensions are
   discarded (gamma-enlarging, sound direction), and a per-field
   Bottom is not propagated to a case Bottom (the corresponding code
   behavior is part of the incomplete row-like corners left
   unrelated). *)
with meet_fields (E : tenv)
  : list ftype -> list ftype -> list ftype -> Prop :=
  | MF_nil : meet_fields E nil nil nil
  | MF_cons : forall t1 t2 t eps r1 r2 r,
      meet E t1 t2 (Meet_ok t eps) ->
      meet_fields E r1 r2 r ->
      meet_fields E (t1 :: r1) (t2 :: r2) (t :: r).

(* --------------------------------------------------------------------- *)
(* Meet: properties                                                       *)
(* --------------------------------------------------------------------- *)

(** RULE T.Meet.Sound (STATUS normative) -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet.ml#meet
    CODE middle_end/flambda2/types/env/meet_env.ml#meet
    Premise: E |- T1 /\ T2 = T |> eps.  Conclusion: gamma_{E;eps}(T)
    contains gamma_E(T1) intersect gamma_E(T2) -- meet must not drop
    any value consistent with both inputs, and eps is satisfied by any
    valuation in which the met value lies in both concretizations. *)
Theorem T_Meet_Sound :
  forall E rho H T1 T2 T eps v,
    meet E T1 T2 (Meet_ok T eps) ->
    consistent E rho H ->
    gamma E rho H T1 v ->
    gamma E rho H T2 v ->
    gamma E rho H T v /\ sat_ext E rho H eps.
Admitted.

(** RULE T.Meet.Bottom (STATUS normative) -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet.ml#meet
    CODE middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null
    Premise: E |- T1 /\ T2 = Bottom.  Conclusion: gamma_E(T1)
    intersect gamma_E(T2) is empty -- an unsound Bottom would delete
    live code. *)
Theorem T_Meet_Bottom :
  forall E rho H T1 T2 v,
    meet E T1 T2 Meet_bottom ->
    consistent E rho H ->
    gamma E rho H T1 v ->
    gamma E rho H T2 v ->
    False.
Admitted.

(** RULE T.Meet.GreatestLowerBound (STATUS conjectured)
    -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet_and_join.mli#meet
    Premise: E |- T1 /\ T2 = T |> eps.  Conclusion: gamma_{E;eps}(T)
    is contained in gamma_E(T1) and in gamma_E(T2) (design intent:
    greatest lower bound; with T.Meet.Sound this would make meet
    exact).  Refutable as a universal claim: the sharpest witnessed
    failure is T.Meet.MutableBlockMissedBottom below, whose result
    gamma is disjoint from the left input's.
    KF-053 DEMOTION (main's ruling; catalog entry 73, the
    REFUTED-CONJECTURE variant): stated as an UNASSERTED named
    Prop, not Theorem+Admitted -- T_Meet_MutableBlockMissedBottom's
    Qed below refutes this claim, so an Admitted here would be a
    usable False axiom.  Zero axiom footprint; the rule id stays
    greppable; the optional upgrade path is a Qed of the
    negation. *)
Definition T_Meet_GreatestLowerBound_claim : Prop :=
  forall E rho H T1 T2 T eps v,
    meet E T1 T2 (Meet_ok T eps) ->
    consistent E rho H ->
    gamma E rho H T v ->
    sat_ext E rho H eps ->
    gamma E rho H T1 v /\ gamma E rho H T2 v.

(** RULE T.Meet.Store.CoercionErasure (STATUS conjectured)
    -- 08-meet-join.md
    CODE middle_end/flambda2/types/env/meet_env.ml#add_concrete_equation_on_canonical
    CODE middle_end/flambda2/types/env/meet_env.ml#record_demotion
    CODE middle_end/flambda2/identifiers/coercion0.mli#change_depth
    CODE middle_end/flambda2/types/grammar/type_grammar.ml#apply_coercion
    Premises in doc order: add_equation resolves a name x to its
    canonical simple y @ co with co not the identity; the incoming
    fact ty meets the coerced existing type apply_coercion(T_y, co).
    Conclusion: the met type is stored DIRECTLY on y without applying
    the inverse coercion -- the coercion is erased at store time;
    value-level sound only because coercions preserve run-time values
    and gamma ignores Rec_info, but the depth components read by the
    inliner are skewed.
    ENCODING NOTE: coercions on aliases are not tracked in this model
    (te_canonical, TypeGrammar.v), so the erasure is baked into
    record_on -- it stores on the underlying name of the canonical
    simple, looking through any coercion.  The statement below pins
    that transcription; the inliner-visible skew is out of scope with
    rec_info opacity (ch. 07 sec. 6).
    Reflexivity-class Qed (catalog entry 34 as AMENDED, second
    instance class; STATUS conjectured preserved above): the proof
    is free because the erasure is baked into record_on, which
    stores on the canonical simple's underlying name looking through
    any coercion -- the pinned statement discharges definitionally
    (fupd lookup law). *)
Theorem T_Meet_Store_CoercionErasure :
  forall (s : simple) (n : name) (T : ftype) (e : env_extension),
    simple_name s = Some n ->
    record_on s T e n = Some T.
Proof.
  intros s n T e Hs.
  unfold record_on.
  rewrite Hs.
  apply fupd_lookup_hit.
  apply name_eqb_eq.
  reflexivity.
Qed.

(** RULE T.Meet.MutableBlockMissedBottom (STATUS conjectured)
    -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null
    Premise: meeting T1 = Variant{immediates = S not Bottom, blocks =
    Known B} against T2 = Mutable_block, where the concretizations are
    disjoint (e.g. B the empty row-like: an immediates-only variant).
    Conclusion: the meet is Mutable_block with the alloc modes met --
    NOT Bottom; the case inspects only the blocks arm's alloc mode.
    Witnessed in meet_test.ml.  Stated here on the envelope: the
    variant-vs-mutable constructor derives a non-Bottom result even
    with the empty row-like on the left, proved outright (Qed). *)
Theorem T_Meet_MutableBlockMissedBottom :
  forall E imms ii gt ext u,
    exists r,
      meet_hvnn E
        (HV_variant ii imms gt
          (Ou_known
            (Mk_row_like_for_blocks fempty Ob_bottom AMT_heap))
          ext u)
        (HV_mutable_block AMT_heap) r /\
      r <> Mres_bottom.
Proof.
  intros. eexists. split.
  - apply MH_variant_mutable_known. apply MA_same.
  - discriminate.
Qed.

(** RULE T.Meet.Terminates (STATUS descriptive) -- 08-meet-join.md
    CODE middle_end/flambda2/types/env/meet_env.ml#adding_equation_for_name
    CODE middle_end/flambda2/types/meet_and_join.ml#meet
    The meet/reduction fixpoint terminates: no infinite descending
    chains in the domains, extensions strictly constrain the
    environment, and recursive equations on a name already being
    updated are dropped (adding_equation_for_name).
    ENCODING NOTE: termination of the implementation's reduction
    fixpoint quantifies over the mutable meet_env update loop, which
    is not modeled (meet here is a derivation relation, where
    finiteness of derivations is structural); recorded as a documented
    anchor. *)
Definition T_Meet_Terminates_documented : Prop := True.

(* --------------------------------------------------------------------- *)
(* Environment descent: tenv_define / tenv_extends / tenv_descends        *)
(* --------------------------------------------------------------------- *)

(* The two operations by which a typing environment grows during a
   straight-line simplification descent, and their reflexive-transitive
   closure.  Interface addition approved by the coordinator; consumed
   by the stated (alias-coarsening) half of
   INV.Simplify.AliasesMonotoneDown in Soundness.v.
     CODE middle_end/flambda2/types/env/typing_env.ml#add_definition
     CODE middle_end/flambda2/types/env/typing_env.ml#add_env_extension
     CODE middle_end/flambda2/types/env/aliases.ml#add
   These are defined OPERATIONALLY (transcribing what the operations
   do), not by axiomatizing monotonicity properties -- otherwise the
   coarsening conjecture would be a vacuous corollary of its own
   definition.  Code-age extension (add_to_code_age) is part of the
   real descent lineage but is documented-unmodeled here: te_code_age
   is unchanged by these steps. *)

(* add_definition: bind a fresh name at kind k, at a binding time
   strictly later than every existing binding, with unconstrained
   type.  The name mode m is the caller's (Bound_name). *)
Inductive tenv_define (E : tenv) (n : name) (k : kind)
  : tenv -> Prop :=
  | TDef_intro : forall bt m,
      te_types E n = None ->
      (forall n' ne, te_types E n' = Some ne ->
         (ne_binding_time ne < bt)%nat) ->
      tenv_define E n k
        (Mk_tenv
           (fupd name_eqb (te_types E) n
              (Mk_name_entry (unknown_of_kind k) bt m))
           (te_canonical E) (te_defined_symbols E) (te_code_age E)
           (te_min_binding_time E) (te_is_bottom E)).

(* One binding (n |-> T) of an extension applied to E (the body of
   add_env_extension's fold).  Two cases, as in the code:
   - T is an alias type Equals(s): merge n's alias class into s's
     (aliases.ml#add).  The demotion discipline: the canonical with
     the earlier binding time survives (win); the other class
     representative (dem) and its whole class are redirected to win.
     A binding-time TIE is deliberately left to the premise's
     nondeterminism (either may win): the code breaks ties by name
     mode and definition order, which is not resolved here.
     ENCODING NOTE: the merged canonical map is characterized
     extensionally (pointwise on the derived `canonical` function)
     rather than constructed, because no boolean equality on simple
     is constructible (const carries opaque floats).  The operational
     content -- which merge happens, chosen by binding time -- is
     fully pinned; only the map's representation is left existential.
   - T is concrete: meet it into n's stored type (te_types).
     ENCODING NOTE: the inner meet's own extension eps' is discarded
     (same slack as the row-like field meets, see the left-bias
     inventory), and a Meet_bottom outcome has no step: an extension
     application that discovers bottom aborts the descent in the code
     (the environment is marked bottom), which this lineage relation
     does not model. *)
Inductive tenv_extends1 (E : tenv) (n : name) (T : ftype)
  : tenv -> Prop :=
  | TE1_alias : forall s win dem E',
      ftype_alias_simple T = Some s ->
      (win = canonical E (simple_of_name n) /\ dem = canonical E s
       \/ win = canonical E s /\ dem = canonical E (simple_of_name n))
      ->
      (binding_time_of_simple E win
         <= binding_time_of_simple E dem)%nat ->
      (forall x, canonical E x = dem -> canonical E' x = win) ->
      (forall x, canonical E x <> dem ->
         canonical E' x = canonical E x) ->
      te_types E' = te_types E ->
      te_defined_symbols E' = te_defined_symbols E ->
      te_code_age E' = te_code_age E ->
      te_min_binding_time E' = te_min_binding_time E ->
      te_is_bottom E' = te_is_bottom E ->
      tenv_extends1 E n T E'
  | TE1_type : forall k Tm eps' ne,
      ftype_alias_simple T = None ->
      kind_of_ftype T = k ->
      te_types E n = Some ne ->
      meet E (tenv_find E n k) T (Meet_ok Tm eps') ->
      tenv_extends1 E n T
        (Mk_tenv
           (fupd name_eqb (te_types E) n
              (Mk_name_entry Tm (ne_binding_time ne) (ne_mode ne)))
           (te_canonical E) (te_defined_symbols E) (te_code_age E)
           (te_min_binding_time E) (te_is_bottom E)).

Inductive tenv_extends_list
  : tenv -> list (name * ftype) -> tenv -> Prop :=
  | TEL_nil : forall E, tenv_extends_list E nil E
  | TEL_cons : forall E n T l E1 E2,
      tenv_extends1 E n T E1 ->
      tenv_extends_list E1 l E2 ->
      tenv_extends_list E ((n, T) :: l) E2.

(* add_env_extension: apply every binding of eps, in some order.
   ENCODING NOTE: env_extension is a function-typed fmap with no
   intrinsic ordering, so the fold order is existentially chosen (a
   list enumerating exactly eps's graph). *)
Definition tenv_extends (E : tenv) (eps : env_extension)
    (E' : tenv) : Prop :=
  exists l,
    (forall n T, eps n = Some T <-> In (n, T) l) /\
    tenv_extends_list E l E'.

(* The straight-line descent lineage: E' is reachable from E by
   definitions and extension applications only.  Program-point
   switches (cutting to a scope, join-point env construction) are
   excluded by construction. *)
Inductive tenv_descends : tenv -> tenv -> Prop :=
  | TDesc_refl : forall E, tenv_descends E E
  | TDesc_define : forall E n k E1 E2,
      tenv_define E n k E1 ->
      tenv_descends E1 E2 ->
      tenv_descends E E2
  | TDesc_extends : forall E eps E1 E2,
      tenv_extends E eps E1 ->
      tenv_descends E1 E2 ->
      tenv_descends E E2.

(* --------------------------------------------------------------------- *)
(* The naked-immediate relational reducer                                 *)
(* --------------------------------------------------------------------- *)

(* Meet-time construction helpers promised by the rule comment on
   head_naked_immediate in TypeGrammar.v (ch. 07,
   T.Grammar.NakedImmediate.Relational): what the code builds for
   is_null / is_int / get_tag of a scrutinee. *)

(* RWC.is_null of a constant. *)
Definition rwc_is_null (c : const) : Z :=
  match c with Const_null => 1 | _ => 0 end.

(* Relation.of_const: resolve a relation applied to a constant.
   is_int here is the REDUCER's semantics
   (type_grammar.ml#Relation.of_const): not (is_null c), i.e. 0 for
   Const_null.  The BUILDER is_int_for_scrutinee below is `= true`
   even for null; the two disagree by design (07-types-domain.md
   NOTES; KF-028 corrected an earlier conflation here).  get_tag of
   a constant does not resolve: of_const's Bottom answer is fused
   with the reducer's discard of Bottom results
   (meet_and_join.ml#reduce_inverse_relations), net effect
   identical. *)
Definition rel_of_const (r : irelation) (c : const) : option Z :=
  match r with
  | Rel_is_null => Some (rwc_is_null c)
  | Rel_is_int => Some (1 - rwc_is_null c)
  | Rel_get_tag => None
  end.

Definition is_null_for_scrutinee (s : simple) : ftype :=
  match simple_name s, s with
  | Some n, _ =>
      FT_naked_immediate (Oub_ok (No_alias (Inverse_relations
        (fupd irelation_eqb fempty Rel_is_null (n :: nil)))))
  | None, Simple_const c =>
      FT_naked_immediate (Oub_ok (No_alias
        (Naked_immediates (rwc_is_null c :: nil))))
  | None, Simple_name _ _ => FT_naked_immediate Oub_unknown
  end.

Definition is_int_for_scrutinee (s : simple) : ftype :=
  match simple_name s with
  | Some n =>
      FT_naked_immediate (Oub_ok (No_alias (Inverse_relations
        (fupd irelation_eqb fempty Rel_is_int (n :: nil)))))
  | None =>
      FT_naked_immediate
        (Oub_ok (No_alias (Naked_immediates (1 :: nil))))
  end.

Definition get_tag_for_block (b : simple) : ftype :=
  match simple_name b with
  | Some n =>
      FT_naked_immediate (Oub_ok (No_alias (Inverse_relations
        (fupd irelation_eqb fempty Rel_get_tag (n :: nil)))))
  | None => FT_naked_immediate Oub_unknown
  end.

(** RULE T.Meet.Relational (STATUS descriptive) -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet_and_join.ml#reduce_inverse_relations
    CODE middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_naked_immediate
    Premise: meeting a naked-immediate type carrying inverse relations
    Is_null / Is_int / Get_tag against a narrowed immediate set S.
    Conclusions in doc order: if S determines the relation, the
    implied equation is added to the related name (Is_int with S={1}
    gives any_tagged_immediate, S={0} any_block; Get_tag determining
    tags gives blocks_with_these_tags; Is_null analogous); S empty
    gives Bottom.  Reverse reductions are only performed for relations
    not already known on that side.
    ENCODING NOTE: the reducer is a heuristic rewrite inside the meet
    loop; its constructors-for-scrutinees are modeled above
    (is_null_for_scrutinee / is_int_for_scrutinee / get_tag_for_block,
    with rel_of_const resolving constant relations and the reducer
    deliberately discarding a Bottom result of that resolution), and
    the implied-equation propagation is subsumed by the extension
    slack of the meet envelope; recorded as a documented anchor. *)
Definition T_Meet_Relational_documented : Prop := True.

(* --------------------------------------------------------------------- *)
(* Join                                                                   *)
(* --------------------------------------------------------------------- *)

(* Binary join.  ENCODING NOTE: the structural head join
   (componentwise / disjunct-wise, T.Join.Head) is descriptive and
   left unrelated except for the naked-number set-union clause;
   Unknown is always a sound result of the specification (T.Join.Sound
   permits any over-approximation), so the envelope's partiality never
   understates the spec. *)
Inductive join2 (E : tenv) : ftype -> ftype -> ftype -> Prop :=
  (** RULE T.Join.SharedAlias (STATUS normative) -- 08-meet-join.md
      CODE middle_end/flambda2/types/meet_and_join.ml#join
      Premises in doc order: the alias sets of s1 (in the left env)
      and s2 (in the right) intersect in the target env at s; and, if
      joining under a bound_name, s is bound strictly earlier than
      that name.  Conclusion: E |- T1 \/ T2 = (= s).
      ENCODING NOTE: the three-environment Join_env is collapsed to
      the single E (branch environments are the fork env plus
      extensions; the level machinery is opaque, see T.Join.Levels),
      so the alias-set intersection premise becomes agreement of the
      canonicals in E; the bound-name clause has no counterpart with
      no bound_name in scope and is omitted. *)
  | J_shared_alias : forall T1 T2 s1 s2 s k,
      ftype_alias_simple T1 = Some s1 ->
      ftype_alias_simple T2 = Some s2 ->
      canonical E s1 = s ->
      canonical E s2 = s ->
      kind_of_ftype T1 = k ->
      join2 E T1 T2 (alias_type_of k s)
  (* Bottom inputs are absorbed; Unknown absorbs the other side
     (T.Join.Sound NOTES / T.Join.Head bottom and top clauses). *)
  | J_bottom_l : forall T1 T2,
      ftype_is_bottom T1 = true ->
      join2 E T1 T2 T2
  | J_bottom_r : forall T1 T2,
      ftype_is_bottom T2 = true ->
      join2 E T1 T2 T1
  | J_unknown_l : forall T1 T2,
      ftype_is_unknown T1 = true ->
      join2 E T1 T2 T1
  | J_unknown_r : forall T1 T2,
      ftype_is_unknown T2 = true ->
      join2 E T1 T2 T2
  | J_naked : forall T1 T2 T,
      naked_set_join T1 T2 T ->
      join2 E T1 T2 T.

(* FROZEN interface: the n-way join at a merge point, as the pairwise
   fold of the Binary algorithm (T.Meet.Dispatch default). *)
Inductive nway_join (E : tenv) : list ftype -> ftype -> Prop :=
  | NWJ_single : forall T, nway_join E (T :: nil) T
  | NWJ_cons : forall T1 Ts T2 T,
      nway_join E Ts T2 ->
      join2 E T1 T2 T ->
      nway_join E (T1 :: Ts) T.

(** RULE T.Join.Sound (STATUS normative) -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet_and_join.ml#join
    CODE middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join
    VERIFIED 14-validation/n_way_join_null.md
    VERIFIED 14-validation/n_way_join_preserves_null.md
    Premise: E |- T1 \/ ... \/ Tn = T.  Conclusion: gamma_E(T)
    contains the union of the gamma_E(Ti) -- no value possible on any
    incoming edge may be lost; join produces no extension and has no
    Bottom result for a non-empty join. *)
Theorem T_Join_Sound :
  forall E rho H Ts T Ti v,
    nway_join E Ts T ->
    consistent E rho H ->
    In Ti Ts ->
    gamma E rho H Ti v ->
    gamma E rho H T v.
Admitted.

(** RULE T.Join.ConstAgreement (STATUS conjectured) -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet_and_join.ml#join
    CODE middle_end/flambda2/types/env/aliases.mli#find_best
    CODE middle_end/flambda2/types/env/typing_env.ml#alias_is_bound_strictly_earlier
    CODE middle_end/flambda2/types/env/binding_time.ml#consts
    Premises in doc order: Flambda_features.join_points() = true (not
    the default; at default flags the merge does not fold); a join at
    a merge point (either algorithm); every branch whose expanded head
    is not Bottom canonicalizes to the SAME constant c.  Conclusion:
    the join result is exactly (= c), not merely an
    over-approximation.
    ENCODING NOTE: the join_points() flag gate is pass plumbing
    outside the model (the statement is the flag-on behavior); the
    per-branch environments are collapsed to the single E (see
    J_shared_alias).  A dead branch ("expanded head is Bottom") is
    transcribed as provable bottomness, meet E Ti Ti Meet_bottom,
    which looks through aliases to a Bottom stored type (via
    M_alias_alias), unlike the syntactic ftype_is_bottom. *)
Theorem T_Join_ConstAgreement :
  forall E Ts T (c : simple),
    (2 <= length Ts)%nat ->
    simple_is_const c = true ->
    canonical E c = c ->
    (forall Ti, In Ti Ts ->
       meet E Ti Ti Meet_bottom \/ can_alias E Ti = Some c) ->
    (exists Ti, In Ti Ts /\ ~ meet E Ti Ti Meet_bottom) ->
    nway_join E Ts T ->
    ftype_alias_simple T = Some c.
Admitted.

(** RULE T.Join.Head (STATUS descriptive) -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet_and_join.ml#join_expanded_head
    CODE middle_end/flambda2/types/meet_and_join.ml#join_head_of_kind_value_non_null
    Premise: no shared alias.  Conclusion: the join is the join of the
    expanded heads -- Bottom is identity, Unknown absorbs, Value and
    naked-number heads join structurally (set union for finite sets,
    componentwise for products, disjunct-wise for row-like); relation
    variables are kept only if syntactically equal, else dropped.
    ENCODING NOTE: the identity/absorption and set-union clauses are
    modeled (J_bottom, J_unknown, J_naked / naked_set_join); the
    structural Value-head join is algorithmic detail left unrelated
    (Unknown over-approximates it soundly); recorded as a documented
    anchor for the rest. *)
Definition T_Join_Head_documented : Prop := True.

(** RULE T.Join.Cutoff (STATUS descriptive) -- 08-meet-join.md
    CODE middle_end/flambda2/types/meet_and_join.ml#join
    CODE middle_end/flambda2/ui/flambda_features.ml#join_depth
    Premise: join is already recursively joining the pair (s1, s2), or
    the join depth is exceeded.  Conclusion: E |- T1 \/ T2 = Unknown
    (already_joining / now_joining and -flambda2-join-depth tame
    recursive types by returning Unknown rather than diverging).
    ENCODING NOTE: quantifies over the in-progress-pair state of the
    join loop, not modeled; Unknown results are already admitted by
    T.Join.Sound; recorded as a documented anchor. *)
Definition T_Join_Cutoff_documented : Prop := True.

(** RULE T.Join.Levels (STATUS descriptive) -- 08-meet-join.md
    CODE middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join
    CODE middle_end/flambda2/types/env/join_env.ml#cut_and_n_way_join0
    Premise: fork env E0; branches E0 extended by eps_i, each cut
    after scope cut_after.  Conclusion: the result is E0 extended by
    the join of the eps_i, computed over the levels created since the
    fork; Binary mode folds pairwise, N_way joins all n at once.
    ENCODING NOTE: the level stack is abstracted behind the sanctioned
    opaque Parameter cut_as_extension (TypeGrammar.v); the equation
    "result = E0 + join of cuts" is the intended consumption of that
    Parameter and is deferred with the level model; recorded as a
    documented anchor. *)
Definition T_Join_Levels_documented : Prop := True.

(** RULE T.Join.Existentials (STATUS descriptive) -- 08-meet-join.md
    CODE middle_end/flambda2/types/env/join_env.ml#cut_and_n_way_join0
    CODE middle_end/flambda2/types/join_levels_old.ml#cut_and_n_way_join
    Premise: a name x defined after the fork survives the join.
    Conclusion: x is introduced into the result as an existential
    (In_types name mode); continuation parameters are the exception
    (real program variables, never existentially quantified).
    ENCODING NOTE: existential introduction is a level-stack operation
    (see T.Join.Levels); its observable residue, the In_types mode
    floor, is modeled by scoped_name_mode in TypeGrammar.v and
    enforced at the prover boundary (T.Prove.SimpleModeBoundary
    below); recorded as a documented anchor. *)
Definition T_Join_Existentials_documented : Prop := True.

(** RULE T.Join.RecursiveParamsUnknown (STATUS descriptive)
    -- 08-meet-join.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler
    CODE middle_end/flambda2/simplify/env/downwards_env.ml#add_parameters_with_unknown_types
    Premise: the handler is recursive.  Conclusion: each handler's own
    (variant) parameters are added to its handler env with Unknown
    types, not the join of the argument types -- there is no join over
    the back edge; invariant parameters do get a single-pass n-way
    join.  Fuller treatment in ch. 09 (S.Struct.Rec.InvariantVsVariant).
    ENCODING NOTE: a statement about Simplify's handler-environment
    construction (dacc plumbing), out of scope here; recorded as a
    documented anchor. *)
Definition T_Join_RecursiveParamsUnknown_documented : Prop := True.

(* --------------------------------------------------------------------- *)
(* Provers                                                                *)
(* --------------------------------------------------------------------- *)

(* FROZEN result flavours (provers.mli). *)
Inductive proof_of_property (A : Type) : Type :=
  | Proved (a : A)
  | Unknown.
Arguments Proved {A}.
Arguments Unknown {A}.

Inductive meet_shortcut (A : Type) : Type :=
  | Known_result (a : A)
  | Need_meet
  | Invalid.
Arguments Known_result {A}.
Arguments Need_meet {A}.
Arguments Invalid {A}.

(* prove_is_int: discrimination between the immediates and blocks arms
   of a variant (T.Prove.Sound's representative prover).  Unknown is
   always derivable (a pure read may fail); Proved answers need the
   arm on the other side empty and, per gen_value_to_proof, Not_null
   nullability. *)
Inductive prove_is_int (E : tenv)
  : ftype -> proof_of_property bool -> Prop :=
  | PII_unknown : forall T, prove_is_int E T Unknown
  | PII_imm_only : forall ii imms gt rl ext u,
      rlfb_empty rl ->
      prove_is_int E
        (FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok (HV_variant ii imms gt (Ou_known rl) ext u))
          Not_null))))
        (Proved true)
  | PII_blocks_only : forall ii timms gt bl ext u,
      ftype_is_bottom timms = true ->
      prove_is_int E
        (FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok (HV_variant ii (Ou_known timms) gt bl ext u))
          Not_null))))
        (Proved false).

(* prove_get_tag: the set of tags a block of this type may carry.
   Universal over the BLOCK values only, so the immediates arm and the
   nullability are unconstrained; other_tags must be Bottom for the
   known-tags map to be exhaustive.  The In-premise admits any
   SUPERSET ts of the map's domain, where the code returns exactly
   all_tags (type_grammar.ml#all_tags) -- sound-direction slack: a
   larger proved tag set constrains consumers less. *)
Inductive prove_get_tag (E : tenv)
  : ftype -> proof_of_property (list tag) -> Prop :=
  | PGT_unknown : forall T, prove_get_tag E T Unknown
  | PGT_known : forall ii imms gt kt am ext u isnull ts,
      (forall t, (exists c, kt t = Some c) -> In t ts) ->
      prove_get_tag E
        (FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok (HV_variant ii imms gt
            (Ou_known (Mk_row_like_for_blocks kt Ob_bottom am))
            ext u))
          isnull))))
        (Proved ts).

(* meet_naked_immediates: the finite set of possible naked immediates
   (per-kind meet_naked_* family; used by constant folding and by
   Switch arm pruning in ch. 10). *)
Inductive meet_naked_immediates (E : tenv)
  : ftype -> meet_shortcut (list Z) -> Prop :=
  | MNI_need : forall T, meet_naked_immediates E T Need_meet
  | MNI_known : forall h s m,
      hni_view h = (Ou_known s, m) ->
      head_nonempty s ->
      meet_naked_immediates E
        (FT_naked_immediate (Oub_ok (No_alias h))) (Known_result s)
  | MNI_invalid : forall T,
      ftype_is_bottom T = true ->
      kind_of_ftype T = K_naked_immediate ->
      meet_naked_immediates E T Invalid.

(* meet_equals_tagged_immediates: the possible tagged-immediate values
   of a Value type.  The isnull argument of METI_imm is a wildcard --
   the answer is computed from the non-null head alone, which is
   exactly the gen_value_to_meet behavior that
   T.Prove.MeetShortcut.NullPremise pins (sound under the intersection
   reading of T.Prove.MeetShortcut, since every shape is null-free). *)
Inductive meet_equals_tagged_immediates (E : tenv)
  : ftype -> meet_shortcut (list Z) -> Prop :=
  | METI_need : forall T,
      meet_equals_tagged_immediates E T Need_meet
  | METI_imm : forall ii h s m gt bl ext u isnull,
      hni_view h = (Ou_known s, m) ->
      meet_equals_tagged_immediates E
        (FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok (HV_variant ii
            (Ou_known (FT_naked_immediate (Oub_ok (No_alias h))))
            gt bl ext u))
          isnull))))
        (Known_result s)
  | METI_invalid : forall isnull0,
      meet_equals_tagged_immediates E
        (FT_value
          (Oub_ok (No_alias (Mk_head_value Oub_bottom isnull0))))
        Invalid.

(* prove_equals_to_simple_of_kind: the canonical simple a type is
   equal to, filtered by the Name_mode.normal floor (the disjunction:
   constants always pass; a name passes iff its binding time is not
   below E's min_binding_time, i.e. it is not an existential). *)
Inductive prove_equals_to_simple_of_kind (E : tenv) (k : kind)
  : ftype -> proof_of_property simple -> Prop :=
  | PES_unknown : forall T,
      prove_equals_to_simple_of_kind E k T Unknown
  | PES_alias : forall T s s',
      kind_of_ftype T = k ->
      ftype_alias_simple T = Some s ->
      canonical E s = s' ->
      (simple_is_const s' = true \/
       (exists n, simple_name s' = Some n /\
          Nat.ltb (binding_time_of_name E n) (te_min_binding_time E)
            = false)) ->
      prove_equals_to_simple_of_kind E k T (Proved s').

(* prove_single_closures_entry: the sole function slot and closure
   contents of a known closure (used by ch. 10 to turn an indirect
   call into a direct one; interface committed to RewritesControl.v).
   ENCODING NOTE: models the success case of the
   meet_single_closures_entry / prove_single_closures_entry pair on
   the stored type of the canonical: a Closures head whose row-like
   knows the function slot (other_closures Bottom, documented as
   always Bottom in TypeGrammar.v); the nullability is a wildcard per
   the gen_value_to_meet behavior pinned by
   T.Prove.MeetShortcut.NullPremise. *)
Inductive prove_single_closures_entry (E : tenv)
  : simple -> function_slot -> closures_entry -> Prop :=
  | PSCE_intro : forall s n fs kc ce idx eps am isnull,
      simple_name (canonical E s) = Some n ->
      tenv_find E n K_value =
        FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok (HV_closures
            (Mk_row_like_for_closures kc Ob_bottom) am))
          isnull))) ->
      kc fs = Some (Mk_row_like_closures_case ce idx eps) ->
      prove_single_closures_entry E s fs ce.

(** RULE T.Prove.Sound (STATUS normative) -- 08-meet-join.md
    CODE middle_end/flambda2/types/provers.mli#proof_of_property
    CODE middle_end/flambda2/types/provers.ml#prove_is_int
    Premise: E |- prove_X(T) gives Proved(r).  Conclusion: every v in
    gamma_E(T) satisfies property X with witness r; Unknown claims
    nothing.  Stated on the representative prover prove_is_int. *)
Theorem T_Prove_Sound :
  forall E rho H T b v,
    prove_is_int E T (Proved b) ->
    consistent E rho H ->
    gamma E rho H T v ->
    is_int_flag v = (if b then 1 else 0).
Admitted.

(** RULE T.Prove.MeetShortcut (STATUS normative) -- 08-meet-join.md
    CODE middle_end/flambda2/types/provers.mli#meet_shortcut
    CODE middle_end/flambda2/types/provers.ml#meet_equals_tagged_immediates
    Premise: E |- meet_X(T) gives r'.  Conclusions in doc order:
    Known_result(r) -- every v in gamma_E(T) intersected with the
    shape's concretization has property X with witness r (universal
    over the INTERSECTION, not all of gamma_E(T); here the shape of
    meet_equals_tagged_immediates is the tagged immediates, so the
    intersection reading is the restriction to v = a tagged
    immediate); Invalid -- the intersection is empty (no tagged
    immediate inhabits T); Need_meet -- no claim. *)
Theorem T_Prove_MeetShortcut :
  forall E rho H T r v,
    meet_equals_tagged_immediates E T r ->
    consistent E rho H ->
    gamma E rho H T v ->
    (forall s i,
        r = Known_result s -> v = V_tagged_imm i -> In i s) /\
    (r = Invalid -> forall i, v <> V_tagged_imm i).
Admitted.

(** RULE T.Prove.MeetShortcut.NullPremise (STATUS conjectured)
    -- 08-meet-join.md
    CODE middle_end/flambda2/types/provers.ml#gen_value_to_meet
    CODE middle_end/flambda2/types/provers.ml#gen_value_to_proof
    CODE middle_end/flambda2/types/provers.ml#meet_equals_tagged_immediates
    Premises in doc order: t is a Value type with is_null = Maybe_null
    (Null inhabits gamma_E(t)); meet_X is a meet-shortcut prover built
    on gen_value_to_meet.  Conclusion: meet_X(t) computes its answer
    from the NON-NULL head alone (gen_value_to_meet discards is_null),
    so the naive "Known_result universal over gamma" reading is
    refuted; the corrected intersection reading of T.Prove.MeetShortcut
    holds, and the parallel gen_value_to_proof returns Unknown on
    Maybe_null.  Stated on the envelope as the existence of a
    Maybe_null head answered Known_result, proved outright (Qed). *)
Theorem T_Prove_MeetShortcut_NullPremise :
  forall E,
    exists h,
      hv_is_null h = Maybe_null None /\
      meet_equals_tagged_immediates E
        (FT_value (Oub_ok (No_alias h))) (Known_result (1 :: nil)).
Proof.
  intros E.
  exists (Mk_head_value
    (Oub_ok (HV_variant None
      (Ou_known (FT_naked_immediate (Oub_ok (No_alias
        (Naked_immediates (1 :: nil))))))
      None Ou_unknown No_extensions false))
    (Maybe_null None)).
  split.
  - reflexivity.
  - eapply METI_imm. reflexivity.
Qed.

(** RULE T.Prove.SimpleModeBoundary (STATUS conjectured)
    -- 08-meet-join.md
    CODE middle_end/flambda2/types/provers.ml#prove_equals_to_simple_of_kind
    CODE middle_end/flambda2/types/provers.ml#meet_block_field_simple
    CODE middle_end/flambda2/types/env/typing_env.ml#get_canonical_simple_exn
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_value_slot
    Premise: a prover returns a Simple s intended for TERM
    substitution.  Conclusion: s is canonical at or above the
    requested min_name_mode (Name_mode.normal here): a join-introduced
    existential -- whose mode is forced to In_types by
    T.Env.Scope.Existential -- cannot be returned for substitution
    into a Normal-mode term; the mode floor filters it and the prover
    degrades to Unknown.  Stated on prove_equals_to_simple_of_kind's
    mode-floor disjunction and proved outright (Qed). *)
Theorem T_Prove_SimpleModeBoundary :
  forall E k T s n,
    prove_equals_to_simple_of_kind E k T (Proved s) ->
    simple_name s = Some n ->
    scoped_name_mode E (binding_time_of_name E n) NM_normal
      = NM_normal.
Proof.
  intros E k T s n Hp Hn.
  inversion Hp; subst.
  match goal with
  | Hd : _ \/ _ |- _ => destruct Hd as [Hc | [n' [Hn' Hbt]]]
  end.
  - unfold simple_is_const in Hc. rewrite Hn in Hc.
    discriminate Hc.
  - rewrite Hn in Hn'. injection Hn' as Heq; subst.
    unfold scoped_name_mode. rewrite Hbt. reflexivity.
Qed.

(** RULE T.Prove.GetTag (STATUS normative) -- 08-meet-join.md
    CODE middle_end/flambda2/types/provers.ml#prove_get_tag
    Premise: E |- prove_get_tag(T) gives Proved(tags).  Conclusion:
    every block v in gamma_E(T) has its tag in tags. *)
Theorem T_Prove_GetTag :
  forall E rho H T ts v a t sz,
    prove_get_tag E T (Proved ts) ->
    consistent E rho H ->
    gamma E rho H T v ->
    v = V_ptr a ->
    block_tag_size H a t sz ->
    In t ts.
Admitted.

(* --------------------------------------------------------------------- *)
(* expand_head                                                            *)
(* --------------------------------------------------------------------- *)

(* Resolve the outermost alias to the canonical's stored (concrete)
   head.  ENCODING NOTE: a constant canonical is excluded (its exact
   type lives on the concretization side; stored_type would
   over-approximate it to Unknown, breaking the iff below), so
   expansion of constant-canonical aliases is left unrelated. *)
Inductive expand_head (E : tenv) : ftype -> ftype -> Prop :=
  | EH_concrete : forall T,
      ftype_alias_simple T = None ->
      expand_head E T T
  | EH_alias : forall T s k,
      ftype_alias_simple T = Some s ->
      kind_of_ftype T = k ->
      simple_is_const (canonical E s) = false ->
      expand_head E T (stored_type E k (canonical E s)).

(** RULE T.Expand.Head (STATUS normative) -- 08-meet-join.md
    CODE middle_end/flambda2/types/expand_head.ml#expand_head
    CODE middle_end/flambda2/types/expand_head.ml#expand_head0
    Premise: E |- T expands to H.  Conclusion: gamma_E(T) = gamma_E(H)
    (expansion preserves concretization).  One-step termination rests
    on the path-compressed aliases domain (T.Env.AliasesAuthoritative)
    and the no-Equals-on-canonical invariant guarantees the
    canonical's stored type is concrete; phantom or absent canonicals
    expand to Unknown, which is sound.
    ENCODING NOTE: the doc's equality is between the SET forms
    gamma_E(T) and gamma_E(H) (values admitted under SOME consistent
    rho).  At a fixed rho only the forward inclusion holds: expansion
    discards the alias's singleton constraint { rho(s) }, so the
    backward direction is false pointwise (KF-052).  Stated as the
    per-rho forward implication; the set-form equality is deferred
    with the proofs.  The stored-canonical case rests on consistent's
    stored-equation clause plus its canonical-agreement clause, and
    the defaulted-symbol case (any_value) on its symbol-kind clause
    together with G_equals's kind premise (Concretization.v). *)
Theorem T_Expand_Head :
  forall E rho H T U v,
    expand_head E T U ->
    consistent E rho H ->
    gamma E rho H T v ->
    gamma E rho H U v.
Admitted.

(* --------------------------------------------------------------------- *)
(* reify                                                                  *)
(* --------------------------------------------------------------------- *)

(* The static shapes reify can request lifting for. *)
Inductive to_lift : Type :=
  | TL_immutable_block (t : tag) (shape : block_shape)
      (fields : list simple)
  | TL_boxed_number (k : kind) (contents : simple)
  | TL_immutable_array (fields : list simple).

Definition lift_fields (l : to_lift) : list simple :=
  match l with
  | TL_immutable_block _ _ fs => fs
  | TL_boxed_number _ c => c :: nil
  | TL_immutable_array fs => fs
  end.

(* reification_result (reify.ml), constructors prefixed R_ to avoid
   colliding with the frozen simple/meet_shortcut names. *)
Inductive reification_result : Type :=
  | R_lift (l : to_lift)
  | R_simple (s : simple)
  | R_cannot_reify
  | R_invalid.

(* ENCODING NOTE: the Lift-producing clauses (unique tag+size block
   with reifiable fields, boxed number, immutable array) depend on
   the field-variable admissibility analysis of
   T.Reify.LiftLocalGuard, whose supporting provers are not modeled;
   R_lift is therefore underivable in this envelope, and the lift
   clause of T.Reify.Sound and the guard rule below are stated over
   the (currently empty) set of derivable lifts.  Contract for any
   future extension: a constructor deriving R_lift must land
   together with the Lift clause added to T_Reify_Sound's statement
   (nothing else forces it to satisfy that clause). *)
Inductive reify (E : tenv) : ftype -> reification_result -> Prop :=
  | RF_cannot : forall T, reify E T R_cannot_reify
  (* reify applies the Name_mode.normal floor to every simple it
     returns (reify.ml's get_alias_then_canonical_simple_exn
     ~min_name_mode and its try_canonical_simple fallback) -- the
     same disjunction as PES_alias above; without it R_simple could
     hand an In_types existential to term substitution, the leak
     T.Prove.SimpleModeBoundary precludes (KF-029). *)
  | RF_alias : forall T s,
      can_alias E T = Some s ->
      (simple_is_const s = true \/
       (exists n, simple_name s = Some n /\
          Nat.ltb (binding_time_of_name E n) (te_min_binding_time E)
            = false)) ->
      reify E T (R_simple s)
  | RF_single_imm : forall ii gt rl ext u i,
      rlfb_empty rl ->
      reify E
        (FT_value (Oub_ok (No_alias (Mk_head_value
          (Oub_ok (HV_variant ii
            (Ou_known (FT_naked_immediate (Oub_ok (No_alias
              (Naked_immediates (i :: nil))))))
            gt (Ou_known rl) ext u))
          Not_null))))
        (R_simple (Simple_const (Const_tagged_immediate i)))
  | RF_invalid : forall T,
      ftype_is_bottom T = true ->
      reify E T R_invalid.

(** RULE T.Reify.Sound (STATUS normative) -- 08-meet-join.md
    CODE middle_end/flambda2/types/reify.ml#reify
    Premise: E |- reify(T) gives r.  Conclusions in doc order:
    r = Simple s implies every v in gamma_E(T) equals the value of s;
    r = Lift l implies every such v equals the statically-allocated
    value of l (vacuous here, R_lift underivable -- see the note on
    reify); r = Invalid implies gamma_E(T) is empty; Cannot_reify
    claims nothing. *)
Theorem T_Reify_Sound :
  forall E rho H T r v,
    reify E T r ->
    consistent E rho H ->
    gamma E rho H T v ->
    (forall s, r = R_simple s -> simple_eval rho s = Some v) /\
    (r = R_invalid -> False).
Admitted.

(** RULE T.Reify.LiftLocalGuard (STATUS normative) -- 08-meet-join.md
    CODE middle_end/flambda2/types/reify.ml#reify
    CODE middle_end/flambda2/types/provers.ml#never_holds_locally_allocated_values
    Premise: reify considers lifting a block/array with fields that
    are variables.  Conclusion: a field variable x is allowed only if
    x is defined in E at min_name_mode (the membership conjunct,
    transcribed below) AND (x is a symbol projection OR (x is defined
    at toplevel AND, for a Local / Heap_or_local alloc mode, x
    provably never holds locally-allocated values)).
    ENCODING NOTE: the symbol-projection and
    never_holds_locally_allocated_values conjuncts quantify over
    provers and toplevel-ness not modeled here; only the membership
    conjunct is transcribed, and the statement is currently vacuous
    (R_lift underivable), kept as the interface the lift clauses must
    satisfy if added. *)
Theorem T_Reify_LiftLocalGuard :
  forall E T l,
    reify E T (R_lift l) ->
    forall x,
      In (Simple_name (Name_var x) Coercion_id) (lift_fields l) ->
      name_bound_in E (Name_var x).
Admitted.

(* --------------------------------------------------------------------- *)
(* Pilot lemmas (Tier 1 / Tier 2)                                         *)
(* --------------------------------------------------------------------- *)

(* Tier 1: the empty extension is always satisfied. *)
Lemma sat_ext_empty :
  forall E rho H, sat_ext E rho H fempty.
Proof.
  intros. constructor. intros n T Hn.
  unfold fempty in Hn. discriminate.
Qed.

(* Tier 1: Unknown is a left identity for meet. *)
Lemma meet_unknown_naked_float_l :
  forall E T,
    meet E (FT_naked_float Oub_unknown) T (Meet_ok T fempty).
Proof.
  intros. apply M_unknown_l. reflexivity.
Qed.

(* Tier 1: a singleton naked-immediate head answers its own set. *)
Lemma meet_naked_immediates_singleton :
  forall E i,
    meet_naked_immediates E
      (FT_naked_immediate
        (Oub_ok (No_alias (Naked_immediates (i :: nil)))))
      (Known_result (i :: nil)).
Proof.
  intros. eapply MNI_known.
  - reflexivity.
  - unfold head_nonempty. discriminate.
Qed.

(* Tier 2: the naked-float instance of T.Meet.Sound's gamma
   containment, for the set-intersection clause. *)
Lemma meet_sound_naked_float :
  forall E rho H (s1 s2 s : head_naked_float) f,
    inter_spec s1 s2 s ->
    gamma E rho H (FT_naked_float (Oub_ok (No_alias s1)))
      (V_naked_float f) ->
    gamma E rho H (FT_naked_float (Oub_ok (No_alias s2)))
      (V_naked_float f) ->
    gamma E rho H (FT_naked_float (Oub_ok (No_alias s)))
      (V_naked_float f).
Proof.
  intros E rho H s1 s2 s f Hint Hg1 Hg2.
  inversion Hg1; subst; cbn in *; try discriminate.
  inversion Hg2; subst; cbn in *; try discriminate.
  apply G_naked_float.
  destruct (Hint f) as [_ Hrl].
  apply Hrl. split; assumption.
Qed.
