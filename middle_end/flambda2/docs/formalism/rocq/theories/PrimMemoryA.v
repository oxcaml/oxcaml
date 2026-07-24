(* PrimMemoryA.v -- 06-primitives-memory.md, part A (owner: Curry).
   Covers 36 rule ids (list at end of file): mixed-block shape
   arithmetic, the effects/coeffects classification apparatus,
   allocation (Make_block / Make_array / static mixed blocks), region
   delimiters, Block_load / Block_set, the duplication family, and
   Obj_dup / Opaque_identity / Make_lazy / Int_as_pointer.

   Interfaces per the 2026-07-17 finalizations:
   - Base: mutability := Immutable | Immutable_unique | Mutable;
     mixed_block_shape record with projections value_prefix_size /
     flat_suffix; mixed_block_field_kinds; or_unknown; float/vec op
     Parameters (shared-ops ruling).
   - Syntax (Church's circulated inventory): operator sum prim_op :=
     Op_nullary .. Op_variadic over the per-arity *_primitive types;
     UP_/BP_/VP_ constructor spellings; effects/coeffects enums are
     DEFINED IN Syntax.v (call_kind's C_call payload needs them at
     wave 1) and reused here -- the rule-id comments for
     P.Effects.NoEffects / OnlyGenerative / Arbitrary / Coeffects sit
     on those constructors in Syntax.v (rule-placement per catalog
     entry 6 precedent).
   - Values (Plotkin, final): V_* value constructors; HO_* heap
     objects; heap lookups by direct application
     (H (HK_addr a) = Some o); heap_upd; alloc o H l H' (fresh_for +
     heap_upd packaged); region_stack := list region_handle,
     youngest at the head; prim_result / prim_result_r; denotation /
     denotation_r retyped over prim_op.

   All previously-assumed names are now resolved against the files on
   disk: alloc_mode_alloc := Alloc_heap | Alloc_local (region) and
   scannable_block_shape := Value_only | Mixed_record (sigma) (Base);
   kind_of_flat_suffix_element and mixed_block_field_kinds are Base's;
   SC_block and duplicate_array_kind are as encoded below (Syntax);
   value_kind : value -> kind is TOTAL (Values.v: premises
   "kind(v) = kappa" are value_kind v = kappa), and
   list_set : nat -> A -> list A -> option (list A) (implicit A).

   ENCODING NOTE (undef policy, coordinator ruling; mirrored in
   PrimMemoryB.v and Opsem.v): Only the doc's crisply-stated undef
   conditions appear as explicit PR_undef clauses (out-of-range
   indices, writes to immutable objects, misaligned `aligned`
   accesses, reaching Invalid) -- in this file, the out-of-range
   Block_load / Block_set clauses; representation-mismatch inputs
   are simply not related (no clause), so in the machine they
   surface as STUCK non-final configurations. See Opsem.v's
   goes-wrong classification: OS.Unit.Final classifies "reaching
   OS.Invalid or a stuck state" as undefined behaviour, so nothing
   is lost. *)

From Stdlib Require Import ZArith List Bool.
From Flambda2 Require Import Base Syntax Values.
Import ListNotations.
Open Scope Z_scope.

(* ===================================================================
   Mixed-block shape arithmetic
   =================================================================== *)

(** RULE P.MixedShape.FieldKinds (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.from_prefix_size_and_suffix_elements
    CODE middle_end/flambda2/kinds/flambda_kind.mli#Mixed_block_shape.field_kinds *)
(* ENCODING NOTE: the defining function mixed_block_field_kinds lives
   in Base.v (ch. 03 derives it from the shape) and is literally the
   doc equation, so this rule id sits on its restatement, provable by
   reflexivity (Qed, not Admitted -- the artifact for this defining
   clause is Base's Definition; this theorem pins it to the doc). The
   doc's indexed clauses (field i is Value for i < p, kind(e_{i-p})
   for p <= i < p+m) follow by list arithmetic. *)
Theorem P_MixedShape_FieldKinds :
  forall s : mixed_block_shape,
    mixed_block_field_kinds s
    = repeat K_value (value_prefix_size s)
      ++ map kind_of_flat_suffix_element (flat_suffix s).
Proof. intros s. reflexivity. Qed.

Definition zsum (xs : list Z) : Z := fold_right Z.add 0 xs.

(** RULE P.MixedShape.Offset (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.offset_in_words
    CODE middle_end/flambda2/kinds/flambda_kind.ml#Flat_suffix_element0.size_in_words
    CODE middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.size_in_words *)
(* Three equations under one rule id, in doc order. The doc's two
   offset clauses agree at i = p (empty sum), so the boundary is
   drawn with i <= p as in the doc. This word-offset arithmetic is a
   to_cmm concern; the machine below indexes MixedBlock by logical
   field. *)
Definition fse_size_in_words (e : flat_suffix_element) : Z :=
  match e with
  | FS_naked_vec128 => 2
  | FS_naked_vec256 => 4
  | FS_naked_vec512 => 8
  | _ => 1
  end.

Definition offset_in_words (s : mixed_block_shape) (i : nat) : Z :=
  if (i <=? value_prefix_size s)%nat then Z.of_nat i
  else
    Z.of_nat (value_prefix_size s)
    + zsum (map fse_size_in_words
              (firstn (i - value_prefix_size s) (flat_suffix s))).

Definition suffix_size_in_words (s : mixed_block_shape) : Z :=
  zsum (map fse_size_in_words (flat_suffix s)).

Definition size_in_words (s : mixed_block_shape) : Z :=
  Z.of_nat (value_prefix_size s) + suffix_size_in_words s.

(* ===================================================================
   Effects and coeffects
   ===================================================================
   The effects and coeffects enums themselves (No_effects |
   Only_generative_effects (mu : mutability) | Arbitrary_effects;
   No_coeffects | Has_coeffects) are defined in Syntax.v -- call_kind
   carries them at wave 1 -- and carry the rule-id comments for
   P.Effects.NoEffects, P.Effects.OnlyGenerative, P.Effects.Arbitrary
   and P.Effects.Coeffects there. The remaining two axes and the
   quadruple live here. *)

(** RULE P.Effects.Placement (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/placement.ml#t *)
Inductive placement : Type :=
  | Delay
  | Strict.

(* NOTES of P.Effects.Placement: join(Delay, Strict) = Strict. *)
Definition placement_join (p1 p2 : placement) : placement :=
  match p1, p2 with
  | Delay, Delay => Delay
  | _, _ => Strict
  end.

(** RULE P.Effects.Validity (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/validity.ml#t *)
(* The doc's Can't_move_before_any_branch is spelled without the
   apostrophe. *)
Inductive validity : Type :=
  | Can_move_anywhere
  | Cant_move_before_any_branch
  | Control_flow_point.

(* NOTES of P.Effects.Validity: join is the max in the order
   Can_move_anywhere < Cant_move_before_any_branch
   < Control_flow_point. *)
Definition validity_join (v1 v2 : validity) : validity :=
  match v1, v2 with
  | Control_flow_point, _ | _, Control_flow_point =>
      Control_flow_point
  | Cant_move_before_any_branch, _ | _, Cant_move_before_any_branch =>
      Cant_move_before_any_branch
  | Can_move_anywhere, Can_move_anywhere => Can_move_anywhere
  end.

(** RULE P.Effects.Classification (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/effects_and_coeffects.ml *)
(* The quadruple (Effects, Coeffects, Placement, Validity). The
   conclusion effects_and_coeffects(p) = (E, C, P, V) is carried by
   the classification function effects_of below. *)
Record ece : Type := Mk_ece
  { ece_effects : effects;
    ece_coeffects : coeffects;
    ece_placement : placement;
    ece_validity : validity }.

(* ===================================================================
   Named classifications
   =================================================================== *)

(** RULE P.Effects.Pure (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/effects_and_coeffects.ml#pure
    CODE middle_end/flambda2/terms/effects_and_coeffects.ml#pure_can_be_duplicated *)
(* Four named combinations under one rule id, in doc order. The code
   names them pure / pure_can_be_duplicated / all / read; the EC_
   prefix avoids claiming those very generic global names. *)
Definition EC_pure : ece :=
  {| ece_effects := No_effects;
     ece_coeffects := No_coeffects;
     ece_placement := Strict;
     ece_validity := Cant_move_before_any_branch |}.

Definition EC_pure_can_be_duplicated : ece :=
  {| ece_effects := No_effects;
     ece_coeffects := No_coeffects;
     ece_placement := Delay;
     ece_validity := Cant_move_before_any_branch |}.

Definition EC_all : ece :=
  {| ece_effects := Arbitrary_effects;
     ece_coeffects := Has_coeffects;
     ece_placement := Strict;
     ece_validity := Control_flow_point |}.

Definition EC_read : ece :=
  {| ece_effects := No_effects;
     ece_coeffects := Has_coeffects;
     ece_placement := Strict;
     ece_validity := Cant_move_before_any_branch |}.

(** RULE P.Effects.ReadingFromBlock (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/flambda_primitive.ml#reading_from_a_block *)
(* The premises (mu Immutable/Immutable_unique => No_coeffects,
   mu Mutable => Has_coeffects) are the equations of
   coeffects_of_block_mutability. Per NOTES, reading_from_an_array
   and reading_from_a_string_or_bigstring are defined identically. *)
Definition coeffects_of_block_mutability (mu : mutability)
  : coeffects :=
  match mu with
  | Immutable | Immutable_unique => No_coeffects
  | Mutable => Has_coeffects
  end.

Definition reading_from_a_block (mu : mutability) : ece :=
  {| ece_effects := No_effects;
     ece_coeffects := coeffects_of_block_mutability mu;
     ece_placement := Strict;
     ece_validity := Cant_move_before_any_branch |}.

(** RULE P.Effects.Writing (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/flambda_primitive.ml#writing_to_a_block *)
Definition writing_to_a_block : ece :=
  {| ece_effects := Arbitrary_effects;
     ece_coeffects := No_coeffects;
     ece_placement := Strict;
     ece_validity := Cant_move_before_any_branch |}.

(** RULE P.Effects.Allocation (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_variadic_primitive
    CODE middle_end/flambda2/terms/flambda_primitive.ml#coeffects_of_mode *)
(* Premises (Heap => No_coeffects, Local => Has_coeffects) are the
   equations of coeffects_of_mode; the conclusion is
   ece_allocation. *)
Definition coeffects_of_mode (mode : alloc_mode_alloc) : coeffects :=
  match mode with
  | Alloc_heap => No_coeffects
  | Alloc_local _ => Has_coeffects
  end.

Definition ece_allocation (mu : mutability) (mode : alloc_mode_alloc)
  : ece :=
  {| ece_effects := Only_generative_effects mu;
     ece_coeffects := coeffects_of_mode mode;
     ece_placement := Strict;
     ece_validity := Cant_move_before_any_branch |}.

(* Flambda_features flags referenced by the ch. 05/06
   classifications: float_const_prop () and classic_mode (). *)
Record eff_flags : Type := Mk_eff_flags
  { ef_float_const_prop : bool;
    ef_classic_mode : bool }.

(* P.Effects.FloatRoundingMode (ch. 05; its rule id sits on a
   characterization theorem in PrimScalar.v): float arithmetic and
   comparison read the globally mutable FP rounding mode, so they
   are pure only under the float_const_prop flag and otherwise get
   the read combination. *)
Definition ece_float_rounding (fl : eff_flags) : ece :=
  if ef_float_const_prop fl then EC_pure else EC_read.

(* Shared classification of the atomics (Atomic_load_field and the
   ternary/quaternary atomic read-modify-writes): (Arbitrary_effects,
   Has_coeffects, Strict, Can't_move_before_any_branch), so they are
   never reordered or removed — capturing the memory-ordering
   guarantees this model does not otherwise represent
   (P.Binary.AtomicLoadField NOTES). *)
Definition ece_atomic : ece :=
  {| ece_effects := Arbitrary_effects;
     ece_coeffects := Has_coeffects;
     ece_placement := Strict;
     ece_validity := Cant_move_before_any_branch |}.

(* P.Binary.BigarrayLoad NOTES (reading_from_a_bigarray): bigarray
   storage is always mutable, so loads always have coeffects and are
   never CSE-able; the complex kinds additionally allocate the boxed
   result, hence Only_generative_effects Immutable. *)
Definition reading_from_a_bigarray (bk : bigarray_kind) : ece :=
  match bk with
  | BGK_complex32 | BGK_complex64 =>
      {| ece_effects := Only_generative_effects Immutable;
         ece_coeffects := Has_coeffects;
         ece_placement := Strict;
         ece_validity := Cant_move_before_any_branch |}
  | BGK_float16 | BGK_float32 | BGK_float32_t | BGK_float64
  | BGK_sint8 | BGK_uint8 | BGK_sint16 | BGK_uint16
  | BGK_int32 | BGK_int64 | BGK_int_width_int
  | BGK_targetint_width_int => EC_read
  end.

(* The total classification function: the conclusion
   effects_and_coeffects(p) = (E, C, P, V) of
   P.Effects.Classification (rule id above, on the ece record).
   Each arm transcribes the classification stated in the NOTES of
   the corresponding rule block in ch. 05/06; where the doc elides
   placement/validity with an ellipsis, the elided components are
   Strict / Can't_move_before_any_branch (the CODE anchors'
   uniform choice), except for the Delay cases called out in
   P.Effects.DelayDuplicable. *)
Definition effects_of (fl : eff_flags) (op : prim_op) : ece :=
  match op with
  | Op_nullary np =>
      (* P.Nullary.StateAccessors + P.Nullary.ControlBarriers.
         This grouping is shared verbatim with PrimMemoryB.v's
         effects_of_nullary; the anti-drift theorem
         effects_of_nullary_agrees there checks it by
         reflexivity. *)
      match np with
      | NP_poll | NP_cpu_relax | NP_probe_is_enabled _ _
      | NP_enter_inlined_apply | NP_invalid _ =>
          {| ece_effects := Arbitrary_effects;
             ece_coeffects := Has_coeffects;
             ece_placement := Strict;
             ece_validity := Cant_move_before_any_branch |}
      | NP_optimised_out _ => EC_pure
      | NP_dls_get | NP_tls_get | NP_domain_index => EC_read
      end
  | Op_unary up =>
      match up with
      | UP_block_load _ mut _ => reading_from_a_block mut
      | UP_duplicate_block _ | UP_obj_dup =>
          (* P.Unary.DuplicateBlock NOTES: the source's fields must
             be assumed possibly-mutable, hence Has_coeffects and a
             Mutable generative effect. P.Unary.ObjDup NOTES give
             Obj_dup the identical classification. *)
          {| ece_effects := Only_generative_effects Mutable;
             ece_coeffects := Has_coeffects;
             ece_placement := Strict;
             ece_validity := Cant_move_before_any_branch |}
      | UP_duplicate_array _ mu_s mu_d =>
          (* P.Unary.DuplicateArray NOTES: generative at the
             destination's mutability; coeffects from the
             source's. *)
          {| ece_effects := Only_generative_effects mu_d;
             ece_coeffects := coeffects_of_block_mutability mu_s;
             ece_placement := Strict;
             ece_validity := Cant_move_before_any_branch |}
      | UP_is_int _ | UP_is_null | UP_get_tag | UP_get_header
      | UP_array_length _ | UP_string_length _
      | UP_is_boxed_float | UP_is_flat_float_array =>
          (* Tags, headers and lengths are immutable
             (P.Unary.GetTag / GetHeader / ArrayLength /
             StringLength / IsBoxedFloat NOTES; Is_int and Is_null
             per the CODE anchor). *)
          EC_pure
      | UP_bigarray_length _ =>
          (* P.Unary.BigarrayLength NOTES: dimensions are read from
             a mutable descriptor field, unlike a plain array
             length. *)
          reading_from_a_block Mutable
      | UP_int_as_pointer am =>
          (* P.Unary.IntAsPointer NOTES: No_effects; coeffects from
             the mode; only the Heap case is CSE-eligible. *)
          {| ece_effects := No_effects;
             ece_coeffects := coeffects_of_mode am;
             ece_placement := Strict;
             ece_validity := Cant_move_before_any_branch |}
      | UP_opaque_identity _ _ | UP_end_region _ | UP_end_try_region _
      | UP_peek _ =>
          (* Pinned in place: P.Unary.OpaqueIdentity, EndRegion
             (deliberately NOT Only_generative_effects) and Peek
             NOTES all state (Arbitrary_effects, Has_coeffects,
             Strict, Can't_move_before_any_branch). *)
          {| ece_effects := Arbitrary_effects;
             ece_coeffects := Has_coeffects;
             ece_placement := Strict;
             ece_validity := Cant_move_before_any_branch |}
      | UP_int_arith _ _ | UP_num_conv _ _ | UP_boolean_not
      | UP_reinterpret_64_bit_word _ | UP_reinterpret_boxed_vector
      | UP_unbox_number _ | UP_untag_immediate | UP_tag_immediate =>
          (* P.Effects.PureScalars (ch. 05; rule id on a
             characterization theorem in PrimScalar.v). *)
          EC_pure
      | UP_float_arith _ _ => ece_float_rounding fl
      | UP_box_number _ am =>
          (* P.Effects.BoxNumber (ch. 05; rule id in PrimScalar.v):
             Delay exactly for a heap allocation in classic mode. *)
          {| ece_effects := Only_generative_effects Immutable;
             ece_coeffects := coeffects_of_mode am;
             ece_placement :=
               match am with
               | Alloc_heap =>
                   if ef_classic_mode fl then Delay else Strict
               | Alloc_local _ => Strict
               end;
             ece_validity := Cant_move_before_any_branch |}
      | UP_project_function_slot _ _ | UP_project_value_slot _ _ =>
          (* Pure projections, sunk to their use sites: two of the
             three Delay producers witnessed in
             P.Effects.DelayDuplicable's NOTES. *)
          EC_pure_can_be_duplicated
      | UP_make_lazy _ =>
          (* P.Unary.MakeLazy NOTES. *)
          {| ece_effects := Only_generative_effects Mutable;
             ece_coeffects := No_coeffects;
             ece_placement := Strict;
             ece_validity := Cant_move_before_any_branch |}
      end
  | Op_binary bp =>
      match bp with
      | BP_block_set _ _ _ => writing_to_a_block
      | BP_array_load _ _ mut =>
          (* reading_from_an_array is defined identically to
             reading_from_a_block (P.Effects.ReadingFromBlock
             NOTES). *)
          reading_from_a_block mut
      | BP_string_or_bigstring_load SLV_string _ =>
          (* P.Binary.StringOrBigstringLoad NOTES: a String load is
             Immutable, Bytes/Bigstring loads are Mutable. *)
          reading_from_a_block Immutable
      | BP_string_or_bigstring_load (SLV_bytes | SLV_bigstring) _ =>
          reading_from_a_block Mutable
      | BP_bigarray_load _ bk _ => reading_from_a_bigarray bk
      | BP_phys_equal _ | BP_int_arith _ _ | BP_int_shift _ _
      | BP_int_comp _ _ =>
          (* P.Effects.PureScalars (ch. 05), which lists Phys_equal
             for completeness. *)
          EC_pure
      | BP_float_arith _ _ | BP_float_comp _ _ =>
          ece_float_rounding fl
      | BP_bigarray_get_alignment _ =>
          (* P.Binary.BigarrayGetAlignment NOTES: the data pointer
             of a given bigstring never changes. *)
          EC_pure
      | BP_atomic_load_field _ => ece_atomic
      | BP_poke _ =>
          (* P.Binary.Poke NOTES: pinned raw store — the same
             quadruple as writing_to_a_block. *)
          writing_to_a_block
      | BP_read_offset _ mf =>
          (* P.Binary.ReadOffset NOTES: No_effects; coeffects from
             the mutable flag. *)
          {| ece_effects := No_effects;
             ece_coeffects :=
               match mf with
               | MF_immutable => No_coeffects
               | MF_mutable => Has_coeffects
               end;
             ece_placement := Strict;
             ece_validity := Cant_move_before_any_branch |}
      end
  | Op_ternary tp =>
      match tp with
      | TP_array_set _ _ | TP_bytes_or_bigstring_set _ _ =>
          (* writing_to_an_array / writing_to_bytes_or_bigstring =
             writing_to_a_block (P.Effects.Writing NOTES). *)
          writing_to_a_block
      | TP_bigarray_set _ _ _ =>
          (* P.Ternary.BigarraySet NOTES: (Arbitrary_effects,
             No_coeffects, ...) for every kind, complex included. *)
          writing_to_a_block
      | TP_atomic_field_int_arith _ | TP_atomic_set_field _
      | TP_atomic_exchange_field _ =>
          (* P.Ternary.AtomicSetField NOTES (covers the exchange and
             fetch-and-op variants). *)
          ece_atomic
      | TP_write_offset _ _ _ =>
          (* P.Ternary.WriteOffset NOTES: writing_to_a_block
             classification. *)
          writing_to_a_block
      end
  | Op_quaternary qp =>
      match qp with
      | QP_atomic_compare_and_set_field _
      | QP_atomic_compare_exchange_field _ _ =>
          (* P.Quaternary.AtomicCompareAndSetField NOTES. *)
          ece_atomic
      end
  | Op_variadic vp =>
      match vp with
      | VP_begin_region _ | VP_begin_try_region _ =>
          (* P.Variadic.BeginRegion NOTES: must not move
             (Has_coeffects) but may be deleted if the region is
             never used. *)
          {| ece_effects := Only_generative_effects Mutable;
             ece_coeffects := Has_coeffects;
             ece_placement := Strict;
             ece_validity := Cant_move_before_any_branch |}
      | VP_make_block _ mut am | VP_make_array _ mut am =>
          (* P.Effects.Allocation. *)
          ece_allocation mut am
      end
  end.

(** RULE P.Effects.DelayDuplicable (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects
    CODE middle_end/flambda2/to_cmm/to_cmm_effects.ml#classify_let_binding *)
(* Premise: placement(p) = Delay. Conclusion: coeffects(p) =
   No_coeffects and effects(p) in { No_effects,
   Only_generative_effects Immutable }, licensing to_cmm's
   duplicate-at-each-use lowering. Doc NOTES witness the Delay
   producers exhaustively: the two closure projections (No_effects)
   and Box_number Heap in classic mode (Only_generative_effects
   Immutable); the third producer, Ece.pure_can_be_duplicated, is
   minted inside to_cmm and is pure by construction. Enforced by no
   assertion in the code, hence conjectured. *)
Theorem P_Effects_DelayDuplicable :
  forall (fl : eff_flags) (op : prim_op),
    ece_placement (effects_of fl op) = Delay ->
    ece_coeffects (effects_of fl op) = No_coeffects /\
    (ece_effects (effects_of fl op) = No_effects \/
     ece_effects (effects_of fl op) =
       Only_generative_effects Immutable).
Admitted.

(* Static mixed blocks: the rule P.Static.MixedBlock (installation of
   a Mixed_record-shaped static Block by OS.Let.Static) lives on the
   Mixed_record case of Opsem.v's shared static_const_object install
   relation (Plotkin), so that there is exactly one install semantics
   and no import edge in either direction. Its layout premises are
   stated against Base's mixed_block_field_kinds, which
   P.MixedShape.FieldKinds above pins to the doc equation. *)

(* ===================================================================
   Helpers for the denotations
   =================================================================== *)

(* Consistency against the or_unknown refinements carried on a
   Block_access_kind (part of the "kind and mu consistent with the
   block" premise of P.Unary.BlockLoad). *)
Definition ou_matches {A : Type} (ou : or_unknown A) (x : A) : Prop :=
  match ou with
  | Ou_unknown => True
  | Ou_known y => y = x
  end.

(* from_block_shape (Scannable (Mixed_record sigma), i), per the
   NOTES of P.Unary.BlockLoad.Mixed: Value_prefix bfk when i < p
   (any bfk) and Flat_suffix e_{i-p} when i >= p. Used as the fk
   premise of the .Mixed load/set rules. *)
Inductive fk_matches_shape (s : mixed_block_shape)
  : nat -> mixed_block_access_field_kind -> Prop :=
  | Fk_value_prefix :
      forall i bfk,
        (i < value_prefix_size s)%nat ->
        fk_matches_shape s i (MBAFK_value_prefix bfk)
  | Fk_flat_suffix :
      forall i e,
        (value_prefix_size s <= i)%nat ->
        nth_error (flat_suffix s) (i - value_prefix_size s)
          = Some e ->
        fk_matches_shape s i (MBAFK_flat_suffix e).

(* "o a block matching kind" of P.Unary.DuplicateBlock. The tag and
   length payloads of Duplicate_block_kind are used only for
   printing (mli), so they do not constrain the object; the Mixed
   case has its own rule. *)
Inductive dup_kind_matches
  : duplicate_block_kind -> heap_object -> Prop :=
  | Dup_values :
      forall t len t' mu vs,
        dup_kind_matches (DBK_values t len) (HO_Block t' mu vs)
  | Dup_naked_floats :
      forall len mu fs,
        dup_kind_matches (DBK_naked_floats len) (HO_FloatBlock mu fs).

(* "ak matching kind" of P.Unary.DuplicateArray: the array_kind
   described by a duplicate_array_kind (length payloads are
   printing-only, as for Duplicate_block_kind). *)
Definition array_kind_of_dak (d : duplicate_array_kind)
  : array_kind :=
  match d with
  | DAK_immediates => AK_immediates
  | DAK_values => AK_values
  | DAK_naked_floats _ => AK_naked_floats
  | DAK_naked_float32s _ => AK_naked_float32s
  | DAK_naked_ints _ => AK_naked_ints
  | DAK_naked_int8s _ => AK_naked_int8s
  | DAK_naked_int16s _ => AK_naked_int16s
  | DAK_naked_int32s _ => AK_naked_int32s
  | DAK_naked_int64s _ => AK_naked_int64s
  | DAK_naked_nativeints _ => AK_naked_nativeints
  | DAK_naked_vec128s _ => AK_naked_vec128s
  | DAK_naked_vec256s _ => AK_naked_vec256s
  | DAK_naked_vec512s _ => AK_naked_vec512s
  end.

(* The per-element kinds of an array_kind, for P.Variadic.MakeArray's
   "(unarized elements conforming to ak)" premise: one kind for the
   scalar kinds, and the flattened component kinds for unboxed
   products (elements then conform groupwise; NOTES: "for ak =
   Unboxed_product [k1...km] the logical length is n/m"). Inner
   fixpoint for guardedness (Base.v's unarize_component precedent,
   catalog entry 4). *)
Fixpoint ak_elem_kinds (ak : array_kind) : list kind :=
  match ak with
  | AK_immediates | AK_gc_ignorable_values | AK_values => [K_value]
  | AK_naked_floats => [K_naked_float]
  | AK_naked_float32s => [K_naked_float32]
  | AK_naked_ints => [K_naked_immediate]
  | AK_naked_int8s => [K_naked_int8]
  | AK_naked_int16s => [K_naked_int16]
  | AK_naked_int32s => [K_naked_int32]
  | AK_naked_int64s => [K_naked_int64]
  | AK_naked_nativeints => [K_naked_nativeint]
  | AK_naked_vec128s => [K_naked_vec128]
  | AK_naked_vec256s => [K_naked_vec256]
  | AK_naked_vec512s => [K_naked_vec512]
  | AK_unboxed_product aks =>
      (fix go (l : list array_kind) : list kind :=
         match l with
         | [] => []
         | a :: tl => ak_elem_kinds a ++ go tl
         end) aks
  end.

(* vbar is a concatenation of logical elements, each one group of
   values at the element kinds. *)
Inductive elems_conform (ks : list kind) : list value -> Prop :=
  | Ec_nil : elems_conform ks []
  | Ec_group :
      forall grp rest,
        map value_kind grp = ks ->
        elems_conform ks rest ->
        elems_conform ks (grp ++ rest).

(* ENCODING NOTE (main's ruling, for P.Unary.IntAsPointer): the doc
   writes as_pointer(v) in FUNCTION notation -- same input, same
   output -- and Int_as_pointer's Heap form is CSE-eligible, so the
   denotation must be deterministic: an unconstrained result would
   make ch. 10's sharing/CSE rewrites unsound in the model. Hence a
   sanctioned opaque Parameter (raw address computation not
   otherwise modelled), used only by that rule -- unlike
   ReadOffset/WriteOffset, whose nondeterminism is the honest
   encoding of conjectured reads of unmodelled state. *)
Parameter as_pointer : value -> value.

(* ===================================================================
   denot_mem_a : the heap-only denotations
   =================================================================== *)

(* Field indices are Z on the primitives; nth_error vs (Z.to_nat i)
   together with 0 <= i encodes the doc's 0 <= i < n and v_i = v.
   Out-of-range indices are crisply-stated undef (NOTES), so the
   load/set rules carry explicit PR_undef clauses under the same
   rule id. *)
Inductive denot_mem_a
  : prim_op -> list value -> heap -> prim_result -> Prop :=

  (** RULE P.Variadic.MakeBlock.Values (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#Block_kind
      CODE middle_end/flambda2/simplify/simplify_variadic_primitive.ml#simplify_make_block *)
  (* n matching the arity of ks is a kinding invariant (ill-formed,
     not undef), so it is not a premise here. *)
  | P_Variadic_MakeBlock_Values :
      forall t ks mu mode vs l H H',
        alloc (HO_Block t mu vs) H l H' ->
        denot_mem_a
          (Op_variadic (VP_make_block (BK_values t ks) mu mode))
          vs H (PR_ok (V_ptr (Addr_loc l)) H')

  (** RULE P.Variadic.MakeBlock.NakedFloats (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#Block_kind *)
  | P_Variadic_MakeBlock_NakedFloats :
      forall mu mode fs l H H',
        alloc (HO_FloatBlock mu fs) H l H' ->
        denot_mem_a
          (Op_variadic (VP_make_block BK_naked_floats mu mode))
          (map V_naked_float fs) H
          (PR_ok (V_ptr (Addr_loc l)) H')

  (** RULE P.Variadic.MakeBlock.Mixed (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#Block_kind
      CODE middle_end/flambda2/terms/flambda_primitive.ml#args_kind_of_variadic_primitive *)
  | P_Variadic_MakeBlock_Mixed :
      forall t s mu mode vs l H H',
        length vs
          = (value_prefix_size s + length (flat_suffix s))%nat ->
        Forall2 (fun v k => value_kind v = k) vs
          (mixed_block_field_kinds s) ->
        alloc (HO_MixedBlock t mu s vs) H l H' ->
        denot_mem_a
          (Op_variadic (VP_make_block (BK_mixed t s) mu mode))
          vs H (PR_ok (V_ptr (Addr_loc l)) H')

  (** RULE P.Variadic.MakeArray (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive
      CODE middle_end/flambda2/simplify/simplify_variadic_primitive.ml#simplify_make_array *)
  (* vs are the unarized elements; the premise line's "(unarized
     elements conforming to ak)" is elems_conform at the element
     KINDS (groupwise for unboxed products). Sub-kind invariants --
     an Immediates array holds only immediates, and the float-array
     optimisation's "a Values array never holds floats" -- are
     frontend-maintained, "not checked here" (NOTES), matching
     MakeBlock's ill-formed-not-undef reading of kind refinements.
     n = 0 (empty array) is allowed for every kind. *)
  | P_Variadic_MakeArray :
      forall ak mu mode vs l H H',
        elems_conform (ak_elem_kinds ak) vs ->
        alloc (HO_Array ak mu vs) H l H' ->
        denot_mem_a (Op_variadic (VP_make_array ak mu mode)) vs H
          (PR_ok (V_ptr (Addr_loc l)) H')

  (** RULE P.Unary.BlockLoad (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_immutable_block_load *)
  (* ENCODING NOTE (catalog): the doc writes ptr l with H(l) =
     Block(...), but statically-allocated blocks live at symbols
     (P.Static.MixedBlock installs at the bound symbol) and must be
     loadable, so the pointer ranges over addresses. The doc's "kind
     and mu consistent with the block" premise is the BAK_values /
     HO_Block case match plus the ou_matches refinements on the
     access kind's known tag/size; the field_kind refinement (bfk)
     is a typing hint, not checked operationally. The mut payload
     drives only the coeffect (P.Effects.ReadingFromBlock). *)
  | P_Unary_BlockLoad :
      forall bt bsz bfk mut i a t mu vs v H,
        H (HK_addr a) = Some (HO_Block t mu vs) ->
        0 <= i ->
        nth_error vs (Z.to_nat i) = Some v ->
        ou_matches bt t ->
        ou_matches bsz (Z.of_nat (length vs)) ->
        denot_mem_a
          (Op_unary (UP_block_load (BAK_values bt bsz bfk) mut i))
          [V_ptr a] H (PR_ok v H)
  (* Undef clause of the same rule: i out of range (NOTES). *)
  | P_Unary_BlockLoad_undef :
      forall bt bsz bfk mut i a t mu vs H,
        H (HK_addr a) = Some (HO_Block t mu vs) ->
        i < 0 \/ Z.of_nat (length vs) <= i ->
        denot_mem_a
          (Op_unary (UP_block_load (BAK_values bt bsz bfk) mut i))
          [V_ptr a] H PR_undef

  (** RULE P.Unary.BlockLoad.NakedFloats (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#Block_access_kind *)
  | P_Unary_BlockLoad_NakedFloats :
      forall bsz mut i a mu fs f H,
        H (HK_addr a) = Some (HO_FloatBlock mu fs) ->
        0 <= i ->
        nth_error fs (Z.to_nat i) = Some f ->
        denot_mem_a
          (Op_unary (UP_block_load (BAK_naked_floats bsz) mut i))
          [V_ptr a] H (PR_ok (V_naked_float f) H)
  | P_Unary_BlockLoad_NakedFloats_undef :
      forall bsz mut i a mu fs H,
        H (HK_addr a) = Some (HO_FloatBlock mu fs) ->
        i < 0 \/ Z.of_nat (length fs) <= i ->
        denot_mem_a
          (Op_unary (UP_block_load (BAK_naked_floats bsz) mut i))
          [V_ptr a] H PR_undef

  (** RULE P.Unary.BlockLoad.Mixed (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#Mixed_block_access_field_kind
      CODE middle_end/flambda2/terms/flambda_primitive.ml#Block_access_kind.from_block_shape *)
  | P_Unary_BlockLoad_Mixed :
      forall bt bsz fk s mut i a t mu vs v H,
        H (HK_addr a) = Some (HO_MixedBlock t mu s vs) ->
        0 <= i ->
        nth_error vs (Z.to_nat i) = Some v ->
        fk_matches_shape s (Z.to_nat i) fk ->
        denot_mem_a
          (Op_unary
             (UP_block_load (BAK_mixed bt bsz fk s) mut i))
          [V_ptr a] H (PR_ok v H)
  | P_Unary_BlockLoad_Mixed_undef :
      forall bt bsz fk s mut i a t mu vs H,
        H (HK_addr a) = Some (HO_MixedBlock t mu s vs) ->
        i < 0 \/ Z.of_nat (length vs) <= i ->
        denot_mem_a
          (Op_unary
             (UP_block_load (BAK_mixed bt bsz fk s) mut i))
          [V_ptr a] H PR_undef

  (** RULE P.Binary.BlockSet (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
      CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_block_set *)
  (* Returns unit (tagged_imm 0). There is no operational mutability
     check: the store proceeds regardless of mu (see NOTES; mu <>
     Immutable for Assignment stores is a frontend invariant). *)
  | P_Binary_BlockSet :
      forall bt bsz bfk init i a t mu vs v vs' H H',
        H (HK_addr a) = Some (HO_Block t mu vs) ->
        0 <= i ->
        list_set (Z.to_nat i) v vs = Some vs' ->
        H' = heap_upd H (HK_addr a) (HO_Block t mu vs') ->
        denot_mem_a
          (Op_binary (BP_block_set (BAK_values bt bsz bfk) init i))
          [V_ptr a; v] H (PR_ok (V_tagged_imm 0) H')
  | P_Binary_BlockSet_undef :
      forall bt bsz bfk init i a t mu vs v H,
        H (HK_addr a) = Some (HO_Block t mu vs) ->
        i < 0 \/ Z.of_nat (length vs) <= i ->
        denot_mem_a
          (Op_binary (BP_block_set (BAK_values bt bsz bfk) init i))
          [V_ptr a; v] H PR_undef

  (** RULE P.Binary.BlockSet.NakedFloats (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#Block_access_kind
      CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_block_set *)
  (* Writes field i of a naked-float block (float records;
     Lsetfloatfield lowers to this), returning unit; f is stored
     unboxed. init and the mu discipline are exactly as in
     P.Binary.BlockSet (no operational mutability check). Mirrors
     P.Unary.BlockLoad.NakedFloats. *)
  | P_Binary_BlockSet_NakedFloats :
      forall bsz init i a mu fs f fs' H H',
        H (HK_addr a) = Some (HO_FloatBlock mu fs) ->
        0 <= i ->
        list_set (Z.to_nat i) f fs = Some fs' ->
        H' = heap_upd H (HK_addr a) (HO_FloatBlock mu fs') ->
        denot_mem_a
          (Op_binary (BP_block_set (BAK_naked_floats bsz) init i))
          [V_ptr a; V_naked_float f] H
          (PR_ok (V_tagged_imm 0) H')
  | P_Binary_BlockSet_NakedFloats_undef :
      forall bsz init i a mu fs f H,
        H (HK_addr a) = Some (HO_FloatBlock mu fs) ->
        i < 0 \/ Z.of_nat (length fs) <= i ->
        denot_mem_a
          (Op_binary (BP_block_set (BAK_naked_floats bsz) init i))
          [V_ptr a; V_naked_float f] H PR_undef

  (** RULE P.Binary.BlockSet.Mixed (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#Mixed_block_access_field_kind
      CODE middle_end/flambda2/terms/flambda_primitive.ml#Block_access_kind.from_block_shape *)
  | P_Binary_BlockSet_Mixed :
      forall bt bsz fk s init i a t mu vs k v vs' H H',
        H (HK_addr a) = Some (HO_MixedBlock t mu s vs) ->
        0 <= i ->
        nth_error (mixed_block_field_kinds s) (Z.to_nat i)
          = Some k ->
        value_kind v = k ->
        fk_matches_shape s (Z.to_nat i) fk ->
        list_set (Z.to_nat i) v vs = Some vs' ->
        H' = heap_upd H (HK_addr a) (HO_MixedBlock t mu s vs') ->
        denot_mem_a
          (Op_binary
             (BP_block_set (BAK_mixed bt bsz fk s) init i))
          [V_ptr a; v] H (PR_ok (V_tagged_imm 0) H')
  | P_Binary_BlockSet_Mixed_undef :
      forall bt bsz fk s init i a t mu vs v H,
        H (HK_addr a) = Some (HO_MixedBlock t mu s vs) ->
        i < 0 \/ Z.of_nat (length vs) <= i ->
        denot_mem_a
          (Op_binary
             (BP_block_set (BAK_mixed bt bsz fk s) init i))
          [V_ptr a; v] H PR_undef

  (** RULE P.Unary.DuplicateBlock (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive *)
  (* Shallow copy; may not change tag or mutability (mli). *)
  | P_Unary_DuplicateBlock :
      forall dbk a o l' H H',
        H (HK_addr a) = Some o ->
        dup_kind_matches dbk o ->
        alloc o H l' H' ->
        denot_mem_a (Op_unary (UP_duplicate_block dbk)) [V_ptr a] H
          (PR_ok (V_ptr (Addr_loc l')) H')

  (** RULE P.Unary.DuplicateBlock.Mixed (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#Duplicate_block_kind
      CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive *)
  (* DBK_mixed carries no payload; the copy takes tag, shape and all
     logical fields wholesale from the source object. *)
  | P_Unary_DuplicateBlock_Mixed :
      forall a t mu s vs l' H H',
        H (HK_addr a) = Some (HO_MixedBlock t mu s vs) ->
        alloc (HO_MixedBlock t mu s vs) H l' H' ->
        denot_mem_a (Op_unary (UP_duplicate_block DBK_mixed))
          [V_ptr a] H (PR_ok (V_ptr (Addr_loc l')) H')

  (** RULE P.Unary.DuplicateArray (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive *)
  (* Shallow array copy; the destination gets mutability mu_d.
     Coeffects depend on mu_s (classification only, not premises). *)
  | P_Unary_DuplicateArray :
      forall dak mu_s mu_d a ak mu vs l' H H',
        H (HK_addr a) = Some (HO_Array ak mu vs) ->
        ak = array_kind_of_dak dak ->
        alloc (HO_Array ak mu_d vs) H l' H' ->
        denot_mem_a (Op_unary (UP_duplicate_array dak mu_s mu_d))
          [V_ptr a] H (PR_ok (V_ptr (Addr_loc l')) H')

  (** RULE P.Unary.ObjDup (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive *)
  | P_Unary_ObjDup :
      forall a o l' H H',
        H (HK_addr a) = Some o ->
        alloc o H l' H' ->
        denot_mem_a (Op_unary UP_obj_dup) [V_ptr a] H
          (PR_ok (V_ptr (Addr_loc l')) H')

  (** RULE P.Unary.OpaqueIdentity (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive *)
  (* Denotationally the identity; the optimisation barrier is its
     (Arbitrary_effects, Has_coeffects, Strict,
     Cant_move_before_any_branch) classification. *)
  | P_Unary_OpaqueIdentity :
      forall meo k v H,
        denot_mem_a (Op_unary (UP_opaque_identity meo k)) [v] H
          (PR_ok v H)

  (** RULE P.Unary.MakeLazy (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive *)
  | P_Unary_MakeLazy :
      forall t v l H H',
        alloc (HO_Lazy t v) H l H' ->
        denot_mem_a (Op_unary (UP_make_lazy t)) [v] H
          (PR_ok (V_ptr (Addr_loc l)) H')

  (** RULE P.Unary.IntAsPointer (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive *)
  (* Result via the sanctioned Parameter as_pointer above (see its
     ENCODING NOTE; main's ruling). *)
  | P_Unary_IntAsPointer :
      forall mode v H,
        denot_mem_a (Op_unary (UP_int_as_pointer mode)) [v] H
          (PR_ok (as_pointer v) H).

(* ===================================================================
   denot_region : the region-augmented denotations
   =================================================================== *)

(* The augmented judgment [[p]](vbar; H, R) = (v, H', R') | undef;
   this is Values.v's denotation_r shape. push(iota, R) is cons
   (youngest at the head, matching the OS machine's convention).
   Freshness of iota is stated against the current stack;
   04-opsem.md owns the global region discipline. *)
Inductive denot_region
  : prim_op -> list value -> heap -> region_stack
    -> prim_result_r -> Prop :=

  (** RULE P.Variadic.BeginRegion (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive
      CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_begin_region *)
  (* The rule's argument list is empty; the optional parent/ghost
     region argument (NOTES) structures nesting only and is not
     modelled. *)
  | P_Variadic_BeginRegion :
      forall ghost iota H R,
        ~ In iota R ->
        denot_region (Op_variadic (VP_begin_region ghost)) [] H R
          (PRr_ok (V_region iota) H (iota :: R))

  (** RULE P.Variadic.BeginTryRegion (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#variadic_primitive *)
  | P_Variadic_BeginTryRegion :
      forall ghost iota H R,
        ~ In iota R ->
        denot_region (Op_variadic (VP_begin_try_region ghost)) [] H R
          (PRr_ok (V_region iota) H (iota :: R))

  (** RULE P.Unary.EndRegion (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive *)
  (* The premise R = push(iota, R') appears as the stack pattern
     iota :: R'. Reclamation is conceptual: H is unchanged
     (CORRESPONDENCE catalog entry 10). *)
  | P_Unary_EndRegion :
      forall ghost iota H R',
        denot_region (Op_unary (UP_end_region ghost))
          [V_region iota] H (iota :: R')
          (PRr_ok (V_tagged_imm 0) H R')

  (** RULE P.Unary.EndTryRegion (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive *)
  | P_Unary_EndTryRegion :
      forall ghost iota H R',
        denot_region (Op_unary (UP_end_try_region ghost))
          [V_region iota] H (iota :: R')
          (PRr_ok (V_tagged_imm 0) H R').

(* ===================================================================
   Rule coverage (36 ids assigned to this file; includes
   P.Binary.BlockSet.NakedFloats, added to the doc 2026-07-18)

   Present in this file (31):
   P.MixedShape.FieldKinds, P.MixedShape.Offset,
   P.Effects.Classification, P.Effects.Placement, P.Effects.Validity,
   P.Effects.Pure, P.Effects.ReadingFromBlock, P.Effects.Writing,
   P.Effects.Allocation, P.Effects.DelayDuplicable,
   P.Variadic.MakeBlock.Values,
   P.Variadic.MakeBlock.NakedFloats, P.Variadic.MakeBlock.Mixed,
   P.Variadic.MakeArray, P.Variadic.BeginRegion,
   P.Variadic.BeginTryRegion, P.Unary.EndRegion, P.Unary.EndTryRegion,
   P.Unary.BlockLoad, P.Unary.BlockLoad.NakedFloats,
   P.Unary.BlockLoad.Mixed, P.Binary.BlockSet,
   P.Binary.BlockSet.NakedFloats, P.Binary.BlockSet.Mixed,
   P.Unary.DuplicateBlock, P.Unary.DuplicateBlock.Mixed,
   P.Unary.DuplicateArray, P.Unary.ObjDup, P.Unary.OpaqueIdentity,
   P.Unary.MakeLazy, P.Unary.IntAsPointer.

   Hosted in Syntax.v at the enum constructors (4; rule-placement
   per catalog entry 6): P.Effects.NoEffects, P.Effects.OnlyGenerative,
   P.Effects.Arbitrary, P.Effects.Coeffects.

   Hosted in Opsem.v on the shared static_const_object install
   relation (1; main-approved unification with OS.Let.Static):
   P.Static.MixedBlock.

   The three ch. 05 effects rule ids (P.Effects.PureScalars,
   P.Effects.FloatRoundingMode, P.Effects.BoxNumber) sit on
   characterization theorems in PrimScalar.v, over this file's
   effects_of. *)
