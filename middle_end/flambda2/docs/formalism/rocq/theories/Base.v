(* Base.v -- shared foundations for the flambda2 formalism mechanization.

   Contents: finite partial maps as function environments, the atom
   (name) types, or_bottom/or_unknown wrappers, the kind/subkind/arity
   apparatus of 03-kinds.md, mutability and modes, machine-integer
   wrapping, and the opaque float / Z-based vector carriers.

   Conventions: rocq/CORRESPONDENCE.md.  Wave 0; owner: Church. *)

From Stdlib Require Import ZArith List Bool Arith.
Import ListNotations.

(* Register the All induction schemes for the container types that
   recursive inductives nest through (list here; option/prod in
   downstream files): Rocq 9.2 requires the registration for
   nested-inductive scheme generation, and it persists through
   Require. *)
Scheme All for list.
Scheme All for option.
Scheme All for prod.

(* ================================================================== *)
(* 1. Finite partial maps (doc notation: X -partial-> Y)              *)
(* ================================================================== *)

Definition fmap (A B : Type) := A -> option B.

Definition fempty {A B : Type} : fmap A B := fun _ => None.

(* m[x |-> v] *)
Definition fupd {A B : Type} (eqb : A -> A -> bool)
    (m : fmap A B) (a : A) (b : B) : fmap A B :=
  fun a' => if eqb a a' then Some b else m a'.

(* m[x1 |-> v1, ..., xn |-> vn], leftmost first (rightmost wins). *)
Definition fupd_list {A B : Type} (eqb : A -> A -> bool)
    (m : fmap A B) (kvs : list (A * B)) : fmap A B :=
  fold_left (fun m' kv => fupd eqb m' (fst kv) (snd kv)) kvs m.

(* "x fresh" with respect to a map. *)
Definition fresh_for {A B : Type} (m : fmap A B) (a : A) : Prop :=
  m a = None.

(* Tier-1 pilot lemmas: fupd lookup laws; freshness after update. *)

Lemma fupd_lookup_hit :
  forall (A B : Type) (eqb : A -> A -> bool) (m : fmap A B) a a' b,
    eqb a a' = true -> fupd eqb m a b a' = Some b.
Proof.
  intros A B eqb m a a' b H. unfold fupd. rewrite H. reflexivity.
Qed.

Lemma fupd_lookup_miss :
  forall (A B : Type) (eqb : A -> A -> bool) (m : fmap A B) a a' b,
    eqb a a' = false -> fupd eqb m a b a' = m a'.
Proof.
  intros A B eqb m a a' b H. unfold fupd. rewrite H. reflexivity.
Qed.

Lemma fresh_for_fupd :
  forall (A B : Type) (eqb : A -> A -> bool) (m : fmap A B) a a' b,
    eqb a a' = false -> fresh_for m a' -> fresh_for (fupd eqb m a b) a'.
Proof.
  intros A B eqb m a a' b Hne Hfresh. unfold fresh_for, fupd.
  rewrite Hne. exact Hfresh.
Qed.

(* ================================================================== *)
(* 2. Atoms: one wrapper inductive per namespace over nat             *)
(*    (CORRESPONDENCE.md: named atoms, not De Bruijn)                 *)
(* ================================================================== *)

Inductive variable := Mk_variable (id : nat).
Inductive symbol := Mk_symbol (id : nat).
Inductive code_id := Mk_code_id (id : nat).
Inductive continuation := Mk_continuation (id : nat).
Inductive function_slot := Mk_function_slot (id : nat).
Inductive value_slot := Mk_value_slot (id : nat).
(* Dynamic heap addresses (runtime locations, 04-opsem.md). *)
Inductive location := Mk_location (id : nat).
Inductive region_handle := Mk_region_handle (id : nat).
(* Block tags (Tag.t). *)
Inductive tag := Mk_tag (n : nat).

Definition variable_eqb (x y : variable) : bool :=
  match x, y with Mk_variable a, Mk_variable b => Nat.eqb a b end.
Definition symbol_eqb (x y : symbol) : bool :=
  match x, y with Mk_symbol a, Mk_symbol b => Nat.eqb a b end.
Definition code_id_eqb (x y : code_id) : bool :=
  match x, y with Mk_code_id a, Mk_code_id b => Nat.eqb a b end.
Definition continuation_eqb (x y : continuation) : bool :=
  match x, y with Mk_continuation a, Mk_continuation b => Nat.eqb a b end.
Definition function_slot_eqb (x y : function_slot) : bool :=
  match x, y with Mk_function_slot a, Mk_function_slot b => Nat.eqb a b end.
Definition value_slot_eqb (x y : value_slot) : bool :=
  match x, y with Mk_value_slot a, Mk_value_slot b => Nat.eqb a b end.
Definition location_eqb (x y : location) : bool :=
  match x, y with Mk_location a, Mk_location b => Nat.eqb a b end.
Definition region_handle_eqb (x y : region_handle) : bool :=
  match x, y with Mk_region_handle a, Mk_region_handle b => Nat.eqb a b end.
Definition tag_eqb (x y : tag) : bool :=
  match x, y with Mk_tag a, Mk_tag b => Nat.eqb a b end.

(* Tier-1 pilot lemmas: correctness of the atom equality tests. *)

Lemma variable_eqb_eq : forall x y, variable_eqb x y = true <-> x = y.
Proof.
  intros [a] [b]. simpl. rewrite Nat.eqb_eq. split; congruence.
Qed.

Lemma symbol_eqb_eq : forall x y, symbol_eqb x y = true <-> x = y.
Proof.
  intros [a] [b]. simpl. rewrite Nat.eqb_eq. split; congruence.
Qed.

Lemma code_id_eqb_eq : forall x y, code_id_eqb x y = true <-> x = y.
Proof.
  intros [a] [b]. simpl. rewrite Nat.eqb_eq. split; congruence.
Qed.

Lemma continuation_eqb_eq :
  forall x y, continuation_eqb x y = true <-> x = y.
Proof.
  intros [a] [b]. simpl. rewrite Nat.eqb_eq. split; congruence.
Qed.

Lemma function_slot_eqb_eq :
  forall x y, function_slot_eqb x y = true <-> x = y.
Proof.
  intros [a] [b]. simpl. rewrite Nat.eqb_eq. split; congruence.
Qed.

Lemma value_slot_eqb_eq : forall x y, value_slot_eqb x y = true <-> x = y.
Proof.
  intros [a] [b]. simpl. rewrite Nat.eqb_eq. split; congruence.
Qed.

Lemma location_eqb_eq : forall x y, location_eqb x y = true <-> x = y.
Proof.
  intros [a] [b]. simpl. rewrite Nat.eqb_eq. split; congruence.
Qed.

Lemma region_handle_eqb_eq :
  forall x y, region_handle_eqb x y = true <-> x = y.
Proof.
  intros [a] [b]. simpl. rewrite Nat.eqb_eq. split; congruence.
Qed.

Lemma tag_eqb_eq : forall x y, tag_eqb x y = true <-> x = y.
Proof.
  intros [a] [b]. simpl. rewrite Nat.eqb_eq. split; congruence.
Qed.

Definition variable_eq_dec (x y : variable) : {x = y} + {x <> y}.
Proof. decide equality. apply Nat.eq_dec. Defined.
Definition symbol_eq_dec (x y : symbol) : {x = y} + {x <> y}.
Proof. decide equality. apply Nat.eq_dec. Defined.
Definition code_id_eq_dec (x y : code_id) : {x = y} + {x <> y}.
Proof. decide equality. apply Nat.eq_dec. Defined.
Definition continuation_eq_dec (x y : continuation) : {x = y} + {x <> y}.
Proof. decide equality. apply Nat.eq_dec. Defined.
Definition function_slot_eq_dec (x y : function_slot) : {x = y} + {x <> y}.
Proof. decide equality. apply Nat.eq_dec. Defined.
Definition value_slot_eq_dec (x y : value_slot) : {x = y} + {x <> y}.
Proof. decide equality. apply Nat.eq_dec. Defined.
Definition location_eq_dec (x y : location) : {x = y} + {x <> y}.
Proof. decide equality. apply Nat.eq_dec. Defined.
Definition region_handle_eq_dec (x y : region_handle) : {x = y} + {x <> y}.
Proof. decide equality. apply Nat.eq_dec. Defined.
Definition tag_eq_dec (x y : tag) : {x = y} + {x <> y}.
Proof. decide equality. apply Nat.eq_dec. Defined.

(* ================================================================== *)
(* 3. Names, addresses, heap keys                                     *)
(* ================================================================== *)

(* Name.t: a variable or a symbol. *)
Inductive name :=
  Name_var (x : variable)
| Name_sym (s : symbol).

(* a ::= l | sym  -- address: heap location or symbol (README/04). *)
Inductive address :=
  Addr_loc (l : location)
| Addr_sym (s : symbol).

(* Domain of the heap H : (address + Code_id) -partial-> o. *)
Inductive heap_key :=
  HK_addr (a : address)
| HK_code (cid : code_id).

Definition name_eqb (n1 n2 : name) : bool :=
  match n1, n2 with
  | Name_var x1, Name_var x2 => variable_eqb x1 x2
  | Name_sym s1, Name_sym s2 => symbol_eqb s1 s2
  | _, _ => false
  end.

Definition address_eqb (a1 a2 : address) : bool :=
  match a1, a2 with
  | Addr_loc l1, Addr_loc l2 => location_eqb l1 l2
  | Addr_sym s1, Addr_sym s2 => symbol_eqb s1 s2
  | _, _ => false
  end.

Definition heap_key_eqb (h1 h2 : heap_key) : bool :=
  match h1, h2 with
  | HK_addr a1, HK_addr a2 => address_eqb a1 a2
  | HK_code c1, HK_code c2 => code_id_eqb c1 c2
  | _, _ => false
  end.

Lemma name_eqb_eq : forall n1 n2, name_eqb n1 n2 = true <-> n1 = n2.
Proof.
  intros [x1|s1] [x2|s2]; simpl.
  - rewrite variable_eqb_eq. split; congruence.
  - split; intro H; discriminate.
  - split; intro H; discriminate.
  - rewrite symbol_eqb_eq. split; congruence.
Qed.

Lemma address_eqb_eq : forall a1 a2, address_eqb a1 a2 = true <-> a1 = a2.
Proof.
  intros [l1|s1] [l2|s2]; simpl.
  - rewrite location_eqb_eq. split; congruence.
  - split; intro H; discriminate.
  - split; intro H; discriminate.
  - rewrite symbol_eqb_eq. split; congruence.
Qed.

Lemma heap_key_eqb_eq :
  forall h1 h2, heap_key_eqb h1 h2 = true <-> h1 = h2.
Proof.
  intros [a1|c1] [a2|c2]; simpl.
  - rewrite address_eqb_eq. split; congruence.
  - split; intro H; discriminate.
  - split; intro H; discriminate.
  - rewrite code_id_eqb_eq. split; congruence.
Qed.

(* ================================================================== *)
(* 4. Or_bottom / Or_unknown wrappers                                 *)
(* ================================================================== *)

Inductive or_bottom (A : Type) :=
  Ob_bottom
| Ob_ok (a : A).
Arguments Ob_bottom {A}.
Arguments Ob_ok {A} a.

Inductive or_unknown (A : Type) :=
  Ou_unknown
| Ou_known (a : A).
Arguments Ou_unknown {A}.
Arguments Ou_known {A} a.

Inductive or_unknown_or_bottom (A : Type) :=
  Oub_unknown
| Oub_bottom
| Oub_ok (a : A).
Arguments Oub_unknown {A}.
Arguments Oub_bottom {A}.
Arguments Oub_ok {A} a.

(* ================================================================== *)
(* 5. Kinds (03-kinds.md section 1)                                   *)
(*    CODE kinds/flambda_kind.mli#t                                   *)
(*    CODE kinds/flambda_kind.mli#Naked_number_kind                   *)
(* ================================================================== *)

Inductive naked_number_kind :=
  NN_naked_immediate
| NN_naked_float32
| NN_naked_float
| NN_naked_int8
| NN_naked_int16
| NN_naked_int32
| NN_naked_int64
| NN_naked_nativeint
| NN_naked_vec128
| NN_naked_vec256
| NN_naked_vec512.

Inductive kind :=
  K_value
| K_naked_number (nnk : naked_number_kind)
| K_region
| K_rec_info.

(* ENCODING NOTE: the kind grammar is two-level, exactly as in
   03-kinds.md section 1 (kappa ::= Value | Naked_number nnk | Region
   | Rec_info).  The team brief's flat constructor names are provided
   as abbreviations below; they work in match patterns too, so
   downstream files may treat the kind type as flat.  The brief's list
   also omitted Naked_nativeint, which the doc includes. *)
Abbreviation K_naked_immediate := (K_naked_number NN_naked_immediate).
Abbreviation K_naked_float32 := (K_naked_number NN_naked_float32).
Abbreviation K_naked_float := (K_naked_number NN_naked_float).
Abbreviation K_naked_int8 := (K_naked_number NN_naked_int8).
Abbreviation K_naked_int16 := (K_naked_number NN_naked_int16).
Abbreviation K_naked_int32 := (K_naked_number NN_naked_int32).
Abbreviation K_naked_int64 := (K_naked_number NN_naked_int64).
Abbreviation K_naked_nativeint := (K_naked_number NN_naked_nativeint).
Abbreviation K_naked_vec128 := (K_naked_number NN_naked_vec128).
Abbreviation K_naked_vec256 := (K_naked_number NN_naked_vec256).
Abbreviation K_naked_vec512 := (K_naked_number NN_naked_vec512).

Definition naked_number_kind_eq_dec (a b : naked_number_kind)
    : {a = b} + {a <> b}.
Proof. decide equality. Defined.

Definition kind_eq_dec (a b : kind) : {a = b} + {a <> b}.
Proof. decide equality. apply naked_number_kind_eq_dec. Defined.

Definition kind_eqb (a b : kind) : bool :=
  if kind_eq_dec a b then true else false.

Lemma kind_eqb_eq : forall a b, kind_eqb a b = true <-> a = b.
Proof.
  intros a b. unfold kind_eqb.
  destruct (kind_eq_dec a b); split; congruence.
Qed.

(* ================================================================== *)
(* 6. Block shapes (03-kinds.md section 2)                            *)
(*    CODE kinds/flambda_kind.mli#Block_shape                         *)
(*    CODE kinds/flambda_kind.mli#Scannable_block_shape               *)
(*    CODE kinds/flambda_kind.mli#Mixed_block_shape                   *)
(*    CODE kinds/flambda_kind.mli#flat_suffix_element                 *)
(* ================================================================== *)

Inductive flat_suffix_element :=
  FS_naked_float
| FS_naked_float32
| FS_naked_int8
| FS_naked_int16
| FS_naked_int32
| FS_naked_int64
| FS_naked_nativeint
| FS_naked_immediate
| FS_naked_vec128
| FS_naked_vec256
| FS_naked_vec512.

(* sigma ::= < value_prefix_size = p, flat_suffix = e1 ... em, ... >.
   The field_kinds component of the doc's sigma is derived from
   (p, ebar) (03-kinds.md section 2 prose); see
   mixed_block_field_kinds below. *)
Record mixed_block_shape := Mk_mixed_block_shape {
  value_prefix_size : nat;
  flat_suffix : list flat_suffix_element
}.

Inductive scannable_block_shape :=
  Value_only
| Mixed_record (sigma : mixed_block_shape).

Inductive block_shape :=
  Scannable (ss : scannable_block_shape)
| Float_record.

Definition kind_of_flat_suffix_element (e : flat_suffix_element) : kind :=
  match e with
  | FS_naked_float => K_naked_float
  | FS_naked_float32 => K_naked_float32
  | FS_naked_int8 => K_naked_int8
  | FS_naked_int16 => K_naked_int16
  | FS_naked_int32 => K_naked_int32
  | FS_naked_int64 => K_naked_int64
  | FS_naked_nativeint => K_naked_nativeint
  | FS_naked_immediate => K_naked_immediate
  | FS_naked_vec128 => K_naked_vec128
  | FS_naked_vec256 => K_naked_vec256
  | FS_naked_vec512 => K_naked_vec512
  end.

(* field_kinds(sigma): Value for each of the p prefix fields, then the
   naked-number kind of each flat-suffix element (03-kinds.md
   section 2).  The layout rules P.MixedShape.FieldKinds /
   P.MixedShape.Offset of 06-primitives-memory.md are stated against
   this function in PrimMemoryA.v. *)
Definition mixed_block_field_kinds (s : mixed_block_shape) : list kind :=
  repeat K_value (value_prefix_size s)
  ++ map kind_of_flat_suffix_element (flat_suffix s).

(* Block_shape.equal treats distinct shapes as incompatible (no
   subshaping) -- 03-kinds.md section 2, relied on by 08-meet-join.md. *)
Definition block_shape_eq_dec (a b : block_shape) : {a = b} + {a <> b}.
Proof. repeat decide equality. Defined.

(* ================================================================== *)
(* 7. Subkinds (03-kinds.md section 2)                                *)
(*    CODE kinds/flambda_kind.mli#With_subkind                        *)
(*    CODE kinds/flambda_kind.mli#With_subkind.Non_null_value_subkind *)
(* ================================================================== *)

Inductive nullability :=
  Nullable
| Non_nullable.

(* kappa-hat ::= (kappa, nullable, vsk); vsk is only meaningful for
   kind Value, other kinds must use VS_anything (03-kinds.md sec. 2). *)
Inductive kind_ws :=
  Mk_kind_ws (k : kind) (null : nullability) (vsk : value_subkind)
with value_subkind :=
  VS_anything
| VS_tagged_immediate
| VS_boxed_float32
| VS_boxed_float
| VS_boxed_int32
| VS_boxed_int64
| VS_boxed_nativeint
| VS_boxed_vec128
| VS_boxed_vec256
| VS_boxed_vec512
  (* ENCODING NOTE: the doc's Variant carries "consts : imm set" and a
     tag_map; the finite set of constant discriminants is encoded as a
     list of target immediates (Z), and the tag map as an fmap. *)
| VS_variant (consts : list Z)
    (non_consts : fmap tag (block_shape * list kind_ws))
| VS_float_block (num_fields : nat)
| VS_float_array
| VS_immediate_array
| VS_value_array
| VS_generic_array
| VS_unboxed_float32_array
| VS_untagged_int_array
| VS_untagged_int8_array
| VS_untagged_int16_array
| VS_unboxed_int32_array
| VS_unboxed_int64_array
| VS_unboxed_nativeint_array
| VS_unboxed_vec128_array
| VS_unboxed_vec256_array
| VS_unboxed_vec512_array
| VS_unboxed_product_array.

Definition ws_kind (kw : kind_ws) : kind :=
  match kw with Mk_kind_ws k _ _ => k end.
Definition ws_nullability (kw : kind_ws) : nullability :=
  match kw with Mk_kind_ws _ n _ => n end.
Definition ws_subkind (kw : kind_ws) : value_subkind :=
  match kw with Mk_kind_ws _ _ v => v end.

(* The erased ("anything") kind-with-subkind at a given kind. *)
Definition ws_of_kind (k : kind) : kind_ws :=
  match k with
  | K_value => Mk_kind_ws K_value Nullable VS_anything
  | _ => Mk_kind_ws k Non_nullable VS_anything
  end.

(** RULE WF.Subkind.Erasable (CLAIM normative) -- 03-kinds.md
    CODE kinds/flambda_kind.ml#With_subkind.erase_subkind
    CODE kinds/flambda_kind.ml#With_subkind.equal_ignoring_subkind
    CODE kinds/flambda_kind.ml#With_subkind.has_useful_subkind_info
    Premises: the two defining equations of erase_subkind.
    Conclusion: kinds-with-subkind that agree after erase_subkind are
    interchangeable for kind-agreement purposes -- encoded as
    equal_ignoring_subkind, the relation the WF checks use
    (WellFormed.v).  Placed in Base.v (not WellFormed.v) because the
    kind/subkind apparatus lives here. *)
Definition erase_subkind (kw : kind_ws) : kind_ws :=
  match ws_kind kw with
  | K_value => Mk_kind_ws K_value Nullable VS_anything
  | _ => kw
  end.

Definition equal_ignoring_subkind (k1 k2 : kind_ws) : Prop :=
  erase_subkind k1 = erase_subkind k2.

Definition equal_ignoring_subkinds (l1 l2 : list kind_ws) : Prop :=
  map erase_subkind l1 = map erase_subkind l2.

(* ================================================================== *)
(* 8. Arities and unarization (03-kinds.md section 3)                 *)
(*    CODE kinds/flambda_arity.mli#t                                  *)
(*    CODE kinds/flambda_arity.mli#Component_for_creation             *)
(* ================================================================== *)

Inductive arity_component :=
  Arity_singleton (k : kind_ws)
| Arity_unboxed_product (cs : list arity_component).

(* A [`Complex] arity: may contain unboxed products. *)
Definition arity := list arity_component.

(* A [`Unarized] arity: only singletons, i.e. a flat kind list. *)
Definition unarized_arity := list kind_ws.

(** RULE WF.Arity.Unarize (CLAIM normative) -- 03-kinds.md
    CODE kinds/flambda_arity.ml#unarize
    CODE kinds/flambda_arity.ml#Component.unarize
    Premises, in doc order:
      unarize(Singleton k)         = [k]
      unarize(Unboxed_product [])  = []      (void: no runtime registers)
      unarize(Unboxed_product cs)  = concat_map unarize cs
      unarize(c1 ... cn)           = unarize(c1) ++ ... ++ unarize(cn)
    The first three are unarize_component (the [] case is an instance
    of the concat_map clause); the last is unarize on arities.
    ENCODING NOTE: the inner fixpoint replaces concat_map for the
    guardedness checker; it is definitionally flat_map. *)
Fixpoint unarize_component (c : arity_component) : list kind_ws :=
  match c with
  | Arity_singleton k => [k]
  | Arity_unboxed_product cs =>
      (fix go (l : list arity_component) : list kind_ws :=
         match l with
         | [] => []
         | c' :: tl => unarize_component c' ++ go tl
         end) cs
  end.

Definition unarize (a : arity) : unarized_arity :=
  flat_map unarize_component a.

(* Derived operations (03-kinds.md section 3 prose). *)
Definition num_params (a : arity) : nat := List.length a.
Definition cardinal_unarized (a : arity) : nat :=
  List.length (unarize a).
Definition unarize_per_parameter (a : arity) : list (list kind_ws) :=
  map unarize_component a.
Definition arity_partially_apply (a : arity)
    (num_non_unarized_params_provided : nat) : arity :=
  skipn num_non_unarized_params_provided a.
Definition arity_concat (a1 a2 : arity) : arity := a1 ++ a2.

(* ================================================================== *)
(* 9. Mutability and modes                                            *)
(* ================================================================== *)

(* Mutability.t (06-primitives-memory.md section 1). *)
Inductive mutability :=
  Immutable
| Immutable_unique
| Mutable.

(* Name modes (02-syntax.md; nominal/name_mode.ml#t): a three-element
   semilattice, Normal above Phantom above In_types. *)
Inductive name_mode :=
  NM_normal
| NM_phantom
| NM_in_types.

(* Alloc_mode.For_allocations.t: Heap or Local {region}
   (02-syntax.md, named/Set_of_closures). *)
Inductive alloc_mode_alloc :=
  Alloc_heap
| Alloc_local (region : variable).

(* Alloc_mode.For_applications.t: Heap or Local {region; ghost_region}
   (02-syntax.md, Apply / Function_params_and_body). *)
Inductive alloc_mode_app :=
  App_heap
| App_local (region : variable) (ghost_region : variable).

(* Alloc_mode.For_types.t: Heap, Local, or Heap_or_local
   (02-syntax.md code metadata; 08-meet-join.md). *)
Inductive alloc_mode_types :=
  AMT_heap
| AMT_local
| AMT_heap_or_local.

(* ================================================================== *)
(* 10. Machine integers                                               *)
(* ================================================================== *)

Local Open Scope Z_scope.

(* Scope is 64-bit little-endian targets (README; 15-cmm.md). *)
Definition machine_width : Z := 64.

(* Target_ocaml_int.t: switch discriminants and tagged-immediate
   payloads; the (machine_width - 1)-bit integer range. *)
Definition target_ocaml_int := Z.

(* Two's-complement wrap-around of z at the given bit width: the
   unique w-bit-representable integer congruent to z mod 2^width,
   in [-2^(width-1), 2^(width-1)). *)
Definition wrap (width z : Z) : Z :=
  (z + 2 ^ (width - 1)) mod 2 ^ width - 2 ^ (width - 1).

(* Integer bit widths per naked-number kind (03-kinds.md section 1:
   Naked_immediate is native width minus one bit, Naked_nativeint is
   full native width; None for the float and vector kinds). *)
Definition nnk_int_width (n : naked_number_kind) : option Z :=
  match n with
  | NN_naked_immediate => Some (machine_width - 1)
  | NN_naked_int8 => Some 8
  | NN_naked_int16 => Some 16
  | NN_naked_int32 => Some 32
  | NN_naked_int64 => Some 64
  | NN_naked_nativeint => Some machine_width
  | _ => None
  end.

(* ================================================================== *)
(* 11. Floats and SIMD vectors                                        *)
(* ================================================================== *)

(* ENCODING NOTE: floats are opaque Parameter types (sanctioned;
   CORRESPONDENCE.md): IEEE internals are not load-bearing for
   statement fidelity.  The shared per-operation Parameter set (the
   union of ch. 05's and ch. 15's needs) also lives here so that
   PrimScalar.v and Cmm.v use literally the same operations
   (CORRESPONDENCE.md catalog entry 5). *)
Parameter float64 : Type.
Parameter float32 : Type.

(* Arithmetic (IEEE double / single). *)
Parameter f64_abs f64_neg : float64 -> float64.
Parameter f64_add f64_sub f64_mul f64_div :
  float64 -> float64 -> float64.
Parameter f32_abs f32_neg : float32 -> float32.
Parameter f32_add f32_sub f32_mul f32_div :
  float32 -> float32 -> float32.

(* IEEE comparisons (false on NaN operands); geb/gtb are the swapped
   forms, IEEE neq is negb of eqb. *)
Parameter f64_eqb f64_ltb f64_leb : float64 -> float64 -> bool.
Parameter f32_eqb f32_ltb f32_leb : float32 -> float32 -> bool.
Parameter f64_is_nan : float64 -> bool.
Parameter f32_is_nan : float32 -> bool.

(* Total order a la OCaml's compare (yields -1/0/1; NaN ordered
   below all non-NaN values, and NaN = NaN). *)
Parameter f64_compare : float64 -> float64 -> Z.
Parameter f32_compare : float32 -> float32 -> Z.

(* Conversions.  f32_of_Z rounds ONCE to single precision (the
   int->float32 double-rounding distinction is load-bearing).
   fNN_to_Z truncates toward zero; None on NaN/out of range. *)
Parameter f64_of_Z : Z -> float64.
Parameter f32_of_Z : Z -> float32.
Parameter f64_to_Z : float64 -> option Z.
Parameter f32_to_Z : float32 -> option Z.
Parameter f64_of_f32 : float32 -> float64.  (* exact widening *)
Parameter f32_of_f64 : float64 -> float32.  (* rounds to single *)

(* IEEE bit patterns (64 / 32 bits, as Z). *)
Parameter f64_to_bits : float64 -> Z.
Parameter f64_of_bits : Z -> float64.
Parameter f32_to_bits : float32 -> Z.
Parameter f32_of_bits : Z -> float32.

(* SIMD vectors as Z bit patterns (128/256/512 bits; word0 least
   significant, per 02-syntax.md on vector constants). *)
Inductive vec128 := Mk_vec128 (bits : Z).
Inductive vec256 := Mk_vec256 (bits : Z).
Inductive vec512 := Mk_vec512 (bits : Z).

Definition vec128_eqb (v1 v2 : vec128) : bool :=
  match v1, v2 with Mk_vec128 a, Mk_vec128 b => Z.eqb a b end.
Definition vec256_eqb (v1 v2 : vec256) : bool :=
  match v1, v2 with Mk_vec256 a, Mk_vec256 b => Z.eqb a b end.
Definition vec512_eqb (v1 v2 : vec512) : bool :=
  match v1, v2 with Mk_vec512 a, Mk_vec512 b => Z.eqb a b end.

(* Bit-pattern casts (vectors are concrete Z wrappers, so these are
   Definitions, unlike the float Parameters above). *)
Definition vec128_to_bits (v : vec128) : Z :=
  match v with Mk_vec128 b => b end.
Definition vec128_of_bits (b : Z) : vec128 := Mk_vec128 b.
Definition vec256_to_bits (v : vec256) : Z :=
  match v with Mk_vec256 b => b end.
Definition vec256_of_bits (b : Z) : vec256 := Mk_vec256 b.
Definition vec512_to_bits (v : vec512) : Z :=
  match v with Mk_vec512 b => b end.
Definition vec512_of_bits (b : Z) : vec512 := Mk_vec512 b.
