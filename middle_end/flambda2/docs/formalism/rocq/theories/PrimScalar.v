(* PrimScalar.v -- ch. 05 scalar primitive denotations (31 rules).
   Part of the flambda2 formalism Rocq mechanization.
   Owner: Curry.  Wave 3.
   Imports: Base, Syntax, Values, PrimMemoryA.
   Compile AFTER PrimMemoryA.v (the effects axes, effects_of and the
   mixed-shape helpers live there).

   Interface notes (reconciliation pass, 2026-07-18):
   - The float/vector carriers AND the per-operation Parameter set
     are Base.v's (shared-ops ruling; CORRESPONDENCE catalog entry
     5). This file declares NO float Parameters of its own.
   - Floats are by bit pattern (Numeric_types.Float_by_bit_pattern):
     Base's fXX_eqb is IEEE equality (+0. = -0., NaN <> NaN),
     fXX_ltb/fXX_leb are the IEEE orders (false on NaN operands),
     fXX_compare is the total order of Stdlib.compare (NaN below
     everything, NaN = NaN). Rocq's Leibniz equality on the carrier
     plays the doc's bit-pattern equality.
   - The denotation relation is typed over the operator sum prim_op
     (approved frozen-interface amendment); the rule ids sit on the
     constructors of denot_scalar below. *)

From Stdlib Require Import ZArith List Bool.
From Flambda2 Require Import Base Syntax Values PrimMemoryA.
Import ListNotations.
Open Scope Z_scope.

(* ------------------------------------------------------------------ *)
(* Preliminaries: number domains (ch. 05 "Preliminaries")              *)
(* ------------------------------------------------------------------ *)

(* Ch. 05's width meta-notation: W = machine_width - 1 (the tagged /
   naked-immediate width, 63) and Wn = machine_width (64). Local so
   the single-letter names are not exported to importers. *)
Local Abbreviation W := (machine_width - 1).
Local Abbreviation Wn := machine_width.

(* Z_w membership: two's-complement integers of width w. *)
Definition in_width (w a : Z) : Prop :=
  - (2 ^ (w - 1)) <= a /\ a < 2 ^ (w - 1).

(* [a]_w: the unsigned reading of the w-bit representation of a. *)
Definition to_unsigned (w a : Z) : Z :=
  if a <? 0 then a + 2 ^ w else a.

(* compare-function result in {-1, 0, 1} *)
Definition zcmp (a b : Z) : Z :=
  match Z.compare a b with
  | Lt => -1
  | Eq => 0
  | Gt => 1
  end.

Definition z_of_bool (b : bool) : Z := if b then 1 else 0.

(* Width w(kappa) of each Standard_int kind (ch. 05 kind table). *)
Definition si_width (si : standard_int) : Z :=
  match si with
  | SI_tagged_immediate => W
  | SI_naked_immediate => W
  | SI_naked_int8 => 8
  | SI_naked_int16 => 16
  | SI_naked_int32 => 32
  | SI_naked_int64 => 64
  | SI_naked_nativeint => Wn
  end.

(* val_kappa: kind-indexed injection of Z_{w(kappa)} into values. *)
Definition val_si (si : standard_int) (i : Z) : value :=
  match si with
  | SI_tagged_immediate => V_tagged_imm i
  | SI_naked_immediate => V_naked_imm i
  | SI_naked_int8 => V_naked_int8 i
  | SI_naked_int16 => V_naked_int16 i
  | SI_naked_int32 => V_naked_int32 i
  | SI_naked_int64 => V_naked_int64 i
  | SI_naked_nativeint => V_naked_nativeint i
  end.

(* The Standard_int kinds embedded into Standard_int_or_float
   (Flambda_kind.Standard_int_or_float is the flat 9-way enum). *)
Definition sif_of_standard_int (si : standard_int)
  : standard_int_or_float :=
  match si with
  | SI_tagged_immediate => SIF_tagged_immediate
  | SI_naked_immediate => SIF_naked_immediate
  | SI_naked_int8 => SIF_naked_int8
  | SI_naked_int16 => SIF_naked_int16
  | SI_naked_int32 => SIF_naked_int32
  | SI_naked_int64 => SIF_naked_int64
  | SI_naked_nativeint => SIF_naked_nativeint
  end.

(* Values of float kinds are injected directly via V_naked_float /
   V_naked_float32 in the rules; no val_sif helper is needed. *)

(* ------------------------------------------------------------------ *)
(* Byte-swap semantics (P.Unary.IntArith.SwapByteEndianness NOTES)     *)
(* ------------------------------------------------------------------ *)

(* Swap the two bytes of a 16-bit quantity (given as 0 <= u < 2^16). *)
Definition swap16_bits (u : Z) : Z :=
  Z.lor (Z.shiftl (Z.land u 255) 8) (Z.land (Z.shiftr u 8) 255).

(* Reverse the n low bytes of the (unsigned) bit pattern u. *)
Fixpoint byte_rev (n : nat) (u : Z) : Z :=
  match n with
  | O => 0
  | S n' =>
    Z.lor (Z.shiftl (Z.land u 255) (8 * Z.of_nat n'))
          (byte_rev n' (Z.shiftr u 8))
  end.

Definition swap_si (si : standard_int) (a : Z) : Z :=
  match si with
  (* low 16 bits, byte-swapped, zero-extended to W bits *)
  | SI_tagged_immediate | SI_naked_immediate =>
    swap16_bits (Z.land a 65535)
  (* For_int8s.swap_byte_endianness t = t *)
  | SI_naked_int8 => a
  | SI_naked_int16 => wrap 16 (swap16_bits (to_unsigned 16 a))
  | SI_naked_int32 => wrap 32 (byte_rev 4 (to_unsigned 32 a))
  | SI_naked_int64 => wrap 64 (byte_rev 8 (to_unsigned 64 a))
  | SI_naked_nativeint =>
    wrap Wn (byte_rev (Z.to_nat (Wn / 8)) (to_unsigned Wn a))
  end.

(* ------------------------------------------------------------------ *)
(* Integer arithmetic / shift / comparison semantics                   *)
(* ------------------------------------------------------------------ *)

Definition int_arith_op_sem (w : Z) (op : binary_int_arith_op)
    (a b : Z) : Z :=
  match op with
  | BIA_add => wrap w (a + b)
  | BIA_sub => wrap w (a - b)
  | BIA_mul => wrap w (a * b)
  (* The Div/Mod rows are unreachable (total_int_op gates them out of
     IntArith.Total; the DivMod rules inline the same formulas). *)
  | BIA_div => wrap w (Z.quot a b)   (* toward zero, Stdlib.(/) *)
  | BIA_mod => wrap w (Z.rem a b)
  | BIA_and => wrap w (Z.land a b)
  | BIA_or => wrap w (Z.lor a b)
  | BIA_xor => wrap w (Z.lxor a b)
  end.

(* op in {Add, Sub, Mul, And, Or, Xor}: the six total operators. *)
Definition total_int_op (op : binary_int_arith_op) : Prop :=
  match op with
  | BIA_div | BIA_mod => False
  | _ => True
  end.

Definition shift_sem (w : Z) (op : int_shift_op) (a s : Z) : Z :=
  match op with
  | IS_lsl => wrap w (Z.shiftl a s)
  | IS_lsr => wrap w (Z.shiftr (to_unsigned w a) s)
  | IS_asr => wrap w (Z.shiftr a s)
  end.

(* Signed/unsigned view of an operand for comparisons. *)
Definition su_view (w : Z) (su : signed_or_unsigned) (a : Z) : Z :=
  match su with
  | Signed => a
  | Unsigned => to_unsigned w a
  end.

(* [[c]](a, b) for the boolean integer comparisons
   (P.Binary.IntComp.Bool premise table). *)
Definition int_comp_holds (w : Z)
    (c : prim_comparison signed_or_unsigned) (a b : Z) : bool :=
  match c with
  | PC_eq => a =? b
  | PC_neq => negb (a =? b)
  | PC_lt su => su_view w su a <? su_view w su b
  | PC_gt su => su_view w su b <? su_view w su a
  | PC_le su => su_view w su a <=? su_view w su b
  | PC_ge su => su_view w su b <=? su_view w su a
  end.

(* ------------------------------------------------------------------ *)
(* Float operation selection (over Base.v's shared Parameters)         *)
(* ------------------------------------------------------------------ *)

Definition f64_unop (op : unary_float_arith_op) : float64 -> float64 :=
  match op with
  | UFA_abs => f64_abs
  | UFA_neg => f64_neg
  end.

Definition f32_unop (op : unary_float_arith_op) : float32 -> float32 :=
  match op with
  | UFA_abs => f32_abs
  | UFA_neg => f32_neg
  end.

Definition f64_binop (op : binary_float_arith_op)
    : float64 -> float64 -> float64 :=
  match op with
  | BFA_add => f64_add
  | BFA_sub => f64_sub
  | BFA_mul => f64_mul
  | BFA_div => f64_div
  end.

Definition f32_binop (op : binary_float_arith_op)
    : float32 -> float32 -> float32 :=
  match op with
  | BFA_add => f32_add
  | BFA_sub => f32_sub
  | BFA_mul => f32_mul
  | BFA_div => f32_div
  end.

(* Boolean float comparison (P.Binary.FloatComp.Bool premise table):
   IEEE equality for Eq/Neq; Base's fXX_ltb/fXX_leb are the IEEE
   orders, false whenever either operand is NaN; Gt/Ge are the
   swapped forms. *)
Definition f64_comp_bool (c : prim_comparison unit)
    (b1 b2 : float64) : bool :=
  match c with
  | PC_eq => f64_eqb b1 b2
  | PC_neq => negb (f64_eqb b1 b2)
  | PC_lt _ => f64_ltb b1 b2
  | PC_gt _ => f64_ltb b2 b1
  | PC_le _ => f64_leb b1 b2
  | PC_ge _ => f64_leb b2 b1
  end.

Definition f32_comp_bool (c : prim_comparison unit)
    (b1 b2 : float32) : bool :=
  match c with
  | PC_eq => f32_eqb b1 b2
  | PC_neq => negb (f32_eqb b1 b2)
  | PC_lt _ => f32_ltb b1 b2
  | PC_gt _ => f32_ltb b2 b1
  | PC_le _ => f32_leb b1 b2
  | PC_ge _ => f32_leb b2 b1
  end.

(* ------------------------------------------------------------------ *)
(* Num_conv auxiliary definition (prose bullet list under              *)
(* P.Unary.NumConv; ENCODING NOTE: the doc defines conv_{src->dst} in  *)
(* prose bullets keyed by destination; each bullet is one constructor  *)
(* here. The int -> float32 clause goes THROUGH A DOUBLE               *)
(* (f32_of_f64 (f64_of_Z a)) exactly as the doc specifies -- this is   *)
(* the documented double-rounding behaviour of the folder (see ch. 05  *)
(* open question 3 and 14-validation/float32_double_round.md); do not  *)
(* "fix" it to Base's single-rounding f32_of_Z.                        *)

(* undef boundary u(dst) for float -> int conversions
   (P.Unary.NumConv.FloatToInt.OutOfRange premise table).
   For Naked_int8/16 the boundary is the HOST int width, 63. *)
Definition numconv_bound (dst : standard_int) : Z :=
  match dst with
  | SI_tagged_immediate | SI_naked_immediate => Wn
  | SI_naked_int8 | SI_naked_int16 => 63
  | SI_naked_int32 => 32
  | SI_naked_int64 => 64
  | SI_naked_nativeint => Wn
  end.

(* Truncation toward zero of a float-kinded value; None on NaN/inf.
   Relates the source kind, the argument value, and the result. *)
Inductive sif_trunc
    : standard_int_or_float -> value -> option Z -> Prop :=
| ST_f64 : forall b,
    sif_trunc SIF_naked_float (V_naked_float b) (f64_to_Z b)
| ST_f32 : forall b,
    sif_trunc SIF_naked_float32 (V_naked_float32 b) (f32_to_Z b).

Inductive num_conv_defined
    : standard_int_or_float -> standard_int_or_float
      -> value -> value -> Prop :=
(* int -> int: wrap_{w(dst)} of the signed value (identity, sign
   extension, or truncation as the widths dictate) *)
| NC_int_to_int : forall s d a,
    in_width (si_width s) a ->
    num_conv_defined (sif_of_standard_int s) (sif_of_standard_int d)
      (val_si s a) (val_si d (wrap (si_width d) a))
(* int -> Naked_float: IEEE round-to-nearest double of the value *)
| NC_int_to_float64 : forall s a,
    in_width (si_width s) a ->
    num_conv_defined (sif_of_standard_int s) SIF_naked_float
      (val_si s a) (V_naked_float (f64_of_Z a))
(* int -> Naked_float32: taken to a DOUBLE first, then rounded to
   single (may double-round; documented folder behaviour) *)
| NC_int_to_float32 : forall s a,
    in_width (si_width s) a ->
    num_conv_defined (sif_of_standard_int s) SIF_naked_float32
      (val_si s a) (V_naked_float32 (f32_of_f64 (f64_of_Z a)))
(* float -> int: truncate toward zero; defined iff the integer part
   is within u(dst); result wraps at the destination width *)
| NC_float_to_int : forall s d v n,
    sif_trunc s v (Some n) ->
    in_width (numconv_bound d) n ->
    num_conv_defined s (sif_of_standard_int d)
      v (val_si d (wrap (si_width d) n))
(* float identities and precision changes *)
| NC_f64_to_f64 : forall b,
    num_conv_defined SIF_naked_float SIF_naked_float
      (V_naked_float b) (V_naked_float b)
| NC_f32_to_f32 : forall b,
    num_conv_defined SIF_naked_float32 SIF_naked_float32
      (V_naked_float32 b) (V_naked_float32 b)
| NC_f32_to_f64 : forall b,
    num_conv_defined SIF_naked_float32 SIF_naked_float
      (V_naked_float32 b) (V_naked_float (f64_of_f32 b))
| NC_f64_to_f32 : forall b,
    num_conv_defined SIF_naked_float SIF_naked_float32
      (V_naked_float b) (V_naked_float32 (f32_of_f64 b)).

(* ------------------------------------------------------------------ *)
(* Box_number helpers                                                  *)
(* ------------------------------------------------------------------ *)

(* The kind stored in an HO_Boxed object for each boxable number
   (Values.v's HO_Boxed carries a kind, not a boxable_number). *)
Definition kind_of_boxable (bn : boxable_number) : kind :=
  match bn with
  | BN_naked_float32 => K_naked_float32
  | BN_naked_float => K_naked_float
  | BN_naked_int32 => K_naked_int32
  | BN_naked_int64 => K_naked_int64
  | BN_naked_nativeint => K_naked_nativeint
  | BN_naked_vec128 => K_naked_vec128
  | BN_naked_vec256 => K_naked_vec256
  | BN_naked_vec512 => K_naked_vec512
  end.

(* Contents check: nv is a naked value of boxable kind bn. *)
Definition boxable_contents (bn : boxable_number) (v : value) : Prop :=
  match bn, v with
  | BN_naked_float, V_naked_float _ => True
  | BN_naked_float32, V_naked_float32 _ => True
  | BN_naked_int32, V_naked_int32 _ => True
  | BN_naked_int64, V_naked_int64 _ => True
  | BN_naked_nativeint, V_naked_nativeint _ => True
  | BN_naked_vec128, V_naked_vec128 _ => True
  | BN_naked_vec256, V_naked_vec256 _ => True
  | BN_naked_vec512, V_naked_vec512 _ => True
  | _, _ => False
  end.

(* The vector kinds, for P.Unary.ReinterpretBoxedVector's boxed
   variant. *)
Definition vector_kind (k : kind) : Prop :=
  match k with
  | K_naked_vec128 | K_naked_vec256 | K_naked_vec512 => True
  | _ => False
  end.

(** RULE P.Contract.NoRaiseNoControl (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/terms/flambda_primitive.mli#t
    ENCODING NOTE: the contract is that a primitive denotes either a
    (value, heap) pair or undef, never an exception/trap/control
    transfer. In this mechanization it holds by construction of
    prim_result, whose only constructors are PR_ok and PR_undef; we
    state it over prim_result so it covers every primitive family.
    Proved outright by case analysis (free Qed; reflexivity-Qed
    precedent, catalog 34). *)
Theorem P_Contract_NoRaiseNoControl :
  forall r : prim_result,
    r = PR_undef \/ exists v H', r = PR_ok v H'.
Proof.
  destruct r.
  - right. eexists. eexists. reflexivity.
  - left. reflexivity.
Qed.

(* ------------------------------------------------------------------ *)
(* The scalar denotation relation                                      *)
(* ------------------------------------------------------------------ *)

Inductive denot_scalar
    : prim_op -> list value -> heap -> prim_result -> Prop :=

(** RULE P.Unary.IntArith.SwapByteEndianness (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Unary_int_arith
    CODE middle_end/flambda2/numbers/target_ocaml_int.ml#get_least_significant_16_bits_then_byte_swap *)
| P_Unary_IntArith_SwapByteEndianness : forall k i H,
    in_width (si_width k) i ->
    denot_scalar (Op_unary (UP_int_arith k UIA_swap_byte_endianness))
      [val_si k i] H
      (PR_ok (val_si k (swap_si k i)) H)

(** RULE P.Unary.FloatArith (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_float_arith_op
    ENCODING NOTE: one constructor per bitwidth because float64 and
    float32 are distinct opaque types. *)
| P_Unary_FloatArith_f64 : forall op b H,
    denot_scalar (Op_unary (UP_float_arith FBW_float64 op))
      [V_naked_float b] H
      (PR_ok (V_naked_float (f64_unop op b)) H)
| P_Unary_FloatArith_f32 : forall op b H,
    denot_scalar (Op_unary (UP_float_arith FBW_float32 op))
      [V_naked_float32 b] H
      (PR_ok (V_naked_float32 (f32_unop op b)) H)

(** RULE P.Unary.NumConv (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Make_simplify_int_conv
    CODE middle_end/flambda2/simplify/number_adjuncts.ml#Num_common
    ENCODING NOTE: conv_{src->dst} is the num_conv_defined relation
    above. The rule's [if conv undefined] line is realized by
    P.Unary.NumConv.FloatToInt.OutOfRange below -- float -> int is the
    only partial conversion family. *)
| P_Unary_NumConv : forall src dst v v' H,
    num_conv_defined src dst v v' ->
    denot_scalar (Op_unary (UP_num_conv src dst)) [v] H (PR_ok v' H)

(** RULE P.Unary.NumConv.FloatToInt.OutOfRange (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/numbers/target_ocaml_int.ml#of_float
    CODE middle_end/flambda2/numbers/numeric_types.ml#Short_int
    CODE middle_end/flambda2/simplify/number_adjuncts.ml#Num_common *)
| P_Unary_NumConv_FloatToInt_OutOfRange : forall src dst v r H,
    sif_trunc src v r ->
    (r = None \/
     exists n, r = Some n /\ ~ in_width (numconv_bound dst) n) ->
    denot_scalar
      (Op_unary (UP_num_conv src (sif_of_standard_int dst)))
      [v] H PR_undef

(** RULE P.Unary.NumConv.Int32ToInt64.SignExtend (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/number_adjuncts.ml#For_int32s
    CODE middle_end/flambda2/z3/sign_extension.py
    (Derivable from P.Unary.NumConv since wrap 64 a = a for a in
    Z_32; stated separately as in the doc.) *)
| P_Unary_NumConv_Int32ToInt64_SignExtend : forall a H,
    in_width 32 a ->
    denot_scalar
      (Op_unary (UP_num_conv SIF_naked_int32 SIF_naked_int64))
      [V_naked_int32 a] H
      (PR_ok (V_naked_int64 a) H)

(** RULE P.Unary.BooleanNot (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_boolean_not *)
| P_Unary_BooleanNot_def : forall i H,
    (i = 0 \/ i = 1) ->
    denot_scalar (Op_unary UP_boolean_not) [V_tagged_imm i] H
      (PR_ok (V_tagged_imm (1 - i)) H)
| P_Unary_BooleanNot_undef : forall i H,
    ~ (i = 0 \/ i = 1) ->
    denot_scalar (Op_unary UP_boolean_not) [V_tagged_imm i] H
      PR_undef

(** RULE P.Unary.TagImmediate (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_tag_immediate *)
| P_Unary_TagImmediate : forall i H,
    in_width W i ->
    denot_scalar (Op_unary UP_tag_immediate) [V_naked_imm i] H
      (PR_ok (V_tagged_imm i) H)

(** RULE P.Unary.UntagImmediate (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_untag_immediate *)
| P_Unary_UntagImmediate : forall i H,
    in_width W i ->
    denot_scalar (Op_unary UP_untag_immediate) [V_tagged_imm i] H
      (PR_ok (V_naked_imm i) H)

(** RULE P.Unary.Reinterpret64.Int64AsFloat64 (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_int64_as_unboxed_float64 *)
| P_Unary_Reinterpret64_Int64AsFloat64 : forall b64 H,
    in_width 64 b64 ->
    denot_scalar
      (Op_unary (UP_reinterpret_64_bit_word
                   R64_unboxed_int64_as_unboxed_float64))
      [V_naked_int64 b64] H
      (PR_ok (V_naked_float (f64_of_bits b64)) H)

(** RULE P.Unary.Reinterpret64.Float64AsInt64 (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_float64_as_unboxed_int64 *)
| P_Unary_Reinterpret64_Float64AsInt64 : forall b H,
    denot_scalar
      (Op_unary (UP_reinterpret_64_bit_word
                   R64_unboxed_float64_as_unboxed_int64))
      [V_naked_float b] H
      (PR_ok (V_naked_int64 (f64_to_bits b)) H)

(** RULE P.Unary.Reinterpret64.Int64AsTaggedInt63 (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_unboxed_int64_as_tagged_int63 *)
| P_Unary_Reinterpret64_Int64AsTaggedInt63 : forall w0 H,
    in_width 64 w0 ->
    denot_scalar
      (Op_unary (UP_reinterpret_64_bit_word
                   R64_unboxed_int64_as_tagged_int63))
      [V_naked_int64 w0] H
      (PR_ok (V_tagged_imm
                (wrap W (Z.shiftr (to_unsigned 64 w0) 1))) H)

(** RULE P.Unary.Reinterpret64.TaggedInt63AsInt64 (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#Simplify_reinterpret_tagged_int63_as_unboxed_int64 *)
| P_Unary_Reinterpret64_TaggedInt63AsInt64 : forall i H,
    in_width W i ->
    denot_scalar
      (Op_unary (UP_reinterpret_64_bit_word
                   R64_tagged_int63_as_unboxed_int64))
      [V_tagged_imm i] H
      (PR_ok (V_naked_int64 (2 * i + 1)) H)

(** RULE P.Unary.ReinterpretBoxedVector (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_reinterpret_boxed_vector
    ENCODING NOTE: two constructors, one per admissible layout of the
    argument (boxed vector | all-flat mixed block of 16/32/64 bytes);
    both conclude with the identity on the runtime value.  The
    chapter's wrong-layout undef sentence is left unrelated (stuck)
    per the undef policy, catalog 23. *)
| P_Unary_ReinterpretBoxedVector_Boxed : forall a k b H,
    H (HK_addr a) = Some (HO_Boxed k b) ->
    vector_kind k ->
    denot_scalar (Op_unary UP_reinterpret_boxed_vector) [V_ptr a] H
      (PR_ok (V_ptr a) H)
| P_Unary_ReinterpretBoxedVector_Mixed : forall a t mu s vs H,
    H (HK_addr a) = Some (HO_MixedBlock t mu s vs) ->
    t = Mk_tag 0%nat ->
    value_prefix_size s = 0%nat ->
    (8 * suffix_size_in_words s = 16 \/
     8 * suffix_size_in_words s = 32 \/
     8 * suffix_size_in_words s = 64) ->
    denot_scalar (Op_unary UP_reinterpret_boxed_vector) [V_ptr a] H
      (PR_ok (V_ptr a) H)

(** RULE P.Unary.BoxNumber (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_box_number
    CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive *)
| P_Unary_BoxNumber : forall bn am nv l H H',
    boxable_contents bn nv ->
    alloc (HO_Boxed (kind_of_boxable bn) nv) H l H' ->
    denot_scalar (Op_unary (UP_box_number bn am)) [nv] H
      (PR_ok (V_ptr (Addr_loc l)) H')

(** RULE P.Unary.UnboxNumber (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_unbox_number *)
(* The wrong-kind undef sentence is left unrelated (stuck) per the
   undef policy, catalog 23; contrast BooleanNot, whose crisp
   value condition IS modeled as PR_undef. *)
| P_Unary_UnboxNumber : forall bn a nv H,
    H (HK_addr a) = Some (HO_Boxed (kind_of_boxable bn) nv) ->
    denot_scalar (Op_unary (UP_unbox_number bn)) [V_ptr a] H
      (PR_ok nv H)

(** RULE P.Binary.IntArith.Total (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith
    CODE middle_end/flambda2/simplify/number_adjuncts.ml#Num_common *)
| P_Binary_IntArith_Total : forall k op a b H,
    in_width (si_width k) a ->
    in_width (si_width k) b ->
    total_int_op op ->
    denot_scalar (Op_binary (BP_int_arith k op))
      [val_si k a; val_si k b] H
      (PR_ok (val_si k (int_arith_op_sem (si_width k) op a b)) H)

(** RULE P.Binary.IntArith.DivMod (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith
    CODE middle_end/flambda2/numbers/target_ocaml_int.ml#div *)
| P_Binary_IntArith_DivMod_Div : forall k a b H,
    in_width (si_width k) a ->
    in_width (si_width k) b ->
    b <> 0 ->
    denot_scalar (Op_binary (BP_int_arith k BIA_div))
      [val_si k a; val_si k b] H
      (PR_ok (val_si k (wrap (si_width k) (Z.quot a b))) H)
| P_Binary_IntArith_DivMod_Mod : forall k a b H,
    in_width (si_width k) a ->
    in_width (si_width k) b ->
    b <> 0 ->
    denot_scalar (Op_binary (BP_int_arith k BIA_mod))
      [val_si k a; val_si k b] H
      (PR_ok (val_si k (a - Z.quot a b * b)) H)

(** RULE P.Binary.IntArith.DivModByZero (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_arith
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_zero_division *)
| P_Binary_IntArith_DivModByZero_Div : forall k a H,
    in_width (si_width k) a ->
    denot_scalar (Op_binary (BP_int_arith k BIA_div))
      [val_si k a; val_si k 0] H PR_undef
| P_Binary_IntArith_DivModByZero_Mod : forall k a H,
    in_width (si_width k) a ->
    denot_scalar (Op_binary (BP_int_arith k BIA_mod))
      [val_si k a; val_si k 0] H PR_undef

(** RULE P.Binary.IntArith.DivMinIntByMinusOne (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/numbers/target_ocaml_int.ml#div
    CODE backend/cmm_helpers.ml#div_int
    (Derivable from P.Binary.IntArith.DivMod; stated separately as in
    the doc.) *)
| P_Binary_IntArith_DivMinIntByMinusOne_Div : forall k H,
    denot_scalar (Op_binary (BP_int_arith k BIA_div))
      [val_si k (- (2 ^ (si_width k - 1)));
       val_si k (-1)] H
      (PR_ok (val_si k (- (2 ^ (si_width k - 1)))) H)
| P_Binary_IntArith_DivMinIntByMinusOne_Mod : forall k H,
    denot_scalar (Op_binary (BP_int_arith k BIA_mod))
      [val_si k (- (2 ^ (si_width k - 1)));
       val_si k (-1)] H
      (PR_ok (val_si k 0) H)

(** RULE P.Binary.IntShift (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift
    CODE middle_end/flambda2/simplify/number_adjuncts.ml#with_shift *)
| P_Binary_IntShift : forall k op a s H,
    in_width (si_width k) a ->
    in_width W s ->
    0 <= s < si_width k ->
    denot_scalar (Op_binary (BP_int_shift k op))
      [val_si k a; V_naked_imm s] H
      (PR_ok (val_si k (shift_sem (si_width k) op a s)) H)
| P_Binary_IntShift_undef : forall k op a s H,
    in_width (si_width k) a ->
    (s < 0 \/ s >= si_width k) ->
    denot_scalar (Op_binary (BP_int_shift k op))
      [val_si k a; V_naked_imm s] H PR_undef

(* On out-of-range shifts the constant folder picks a specific legal
   refinement of the undef above; see the FolderPicksZero anchor after
   this Inductive. *)

(** RULE P.Binary.IntShift.ByZero (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_shift
    (Derivable from P.Binary.IntShift since 0 in [0, w); stated
    separately as in the doc.) *)
| P_Binary_IntShift_ByZero : forall k op a H,
    in_width (si_width k) a ->
    denot_scalar (Op_binary (BP_int_shift k op))
      [val_si k a; V_naked_imm 0] H
      (PR_ok (val_si k a) H)

(** RULE P.Binary.IntComp.Bool (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_comp
    CODE middle_end/flambda2/simplify/number_adjuncts.ml#compare_unsigned_generic *)
| P_Binary_IntComp_Bool : forall k c a b H,
    in_width (si_width k) a ->
    in_width (si_width k) b ->
    denot_scalar (Op_binary (BP_int_comp k (CB_yielding_bool c)))
      [val_si k a; val_si k b] H
      (PR_ok (V_naked_imm
                (z_of_bool (int_comp_holds (si_width k) c a b))) H)

(** RULE P.Binary.IntComp.CompareFunction (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Int_ops_for_binary_comp *)
| P_Binary_IntComp_CompareFunction : forall k su a b H,
    in_width (si_width k) a ->
    in_width (si_width k) b ->
    denot_scalar
      (Op_binary
         (BP_int_comp k (CB_yielding_int_like_compare_functions su)))
      [val_si k a; val_si k b] H
      (PR_ok (V_naked_imm
                (zcmp (su_view (si_width k) su a)
                      (su_view (si_width k) su b))) H)

(** RULE P.Binary.FloatArith (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_arith_gen
    CODE middle_end/flambda2/numbers/numeric_types.ml#IEEE_semantics
    ENCODING NOTE: one constructor per bitwidth (distinct opaque float
    types). Float32 arithmetic is genuine single precision. *)
| P_Binary_FloatArith_f64 : forall op b1 b2 H,
    denot_scalar (Op_binary (BP_float_arith FBW_float64 op))
      [V_naked_float b1; V_naked_float b2] H
      (PR_ok (V_naked_float (f64_binop op b1 b2)) H)
| P_Binary_FloatArith_f32 : forall op b1 b2 H,
    denot_scalar (Op_binary (BP_float_arith FBW_float32 op))
      [V_naked_float32 b1; V_naked_float32 b2] H
      (PR_ok (V_naked_float32 (f32_binop op b1 b2)) H)

(** RULE P.Binary.FloatComp.Bool (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_comp_gen
    CODE middle_end/flambda2/numbers/numeric_types.ml#IEEE_semantics *)
| P_Binary_FloatComp_Bool_f64 : forall c b1 b2 H,
    denot_scalar
      (Op_binary (BP_float_comp FBW_float64 (CB_yielding_bool c)))
      [V_naked_float b1; V_naked_float b2] H
      (PR_ok (V_naked_imm (z_of_bool (f64_comp_bool c b1 b2))) H)
| P_Binary_FloatComp_Bool_f32 : forall c b1 b2 H,
    denot_scalar
      (Op_binary (BP_float_comp FBW_float32 (CB_yielding_bool c)))
      [V_naked_float32 b1; V_naked_float32 b2] H
      (PR_ok (V_naked_imm (z_of_bool (f32_comp_bool c b1 b2))) H)

(** RULE P.Binary.FloatComp.CompareFunction (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#Float_ops_for_binary_comp_gen
    (Base's fXX_compare is the TOTAL order of Stdlib.compare on
    floats: NaN below everything, NaN = NaN.) *)
| P_Binary_FloatComp_CompareFunction_f64 : forall b1 b2 H,
    denot_scalar
      (Op_binary
         (BP_float_comp FBW_float64
            (CB_yielding_int_like_compare_functions tt)))
      [V_naked_float b1; V_naked_float b2] H
      (PR_ok (V_naked_imm (f64_compare b1 b2)) H)
| P_Binary_FloatComp_CompareFunction_f32 : forall b1 b2 H,
    denot_scalar
      (Op_binary
         (BP_float_comp FBW_float32
            (CB_yielding_int_like_compare_functions tt)))
      [V_naked_float32 b1; V_naked_float32 b2] H
      (PR_ok (V_naked_imm (f32_compare b1 b2)) H)
.

(** RULE P.Binary.IntShift.OutOfRange.FolderPicksZero (STATUS descriptive)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/simplify/number_adjuncts.ml#with_shift
    Descriptive of the constant folder: on undef shift inputs the
    folder picks val_kappa(0) (a legal refinement of undef; for Asr
    too, not -1). The folder's own cut-off is integer_bit_width, which
    is 64 for Tagged_immediate (not 63) -- see ch. 05 open question 2.
    Documented, not modeled. *)
Definition P_Binary_IntShift_OutOfRange_FolderPicksZero_documented
  : Prop := True.

(* ------------------------------------------------------------------ *)
(* Effects and coeffects of the scalar primitives (ch. 05 tail).       *)
(* effects_of and the axis types are PrimMemoryA.v's.                  *)
(* ------------------------------------------------------------------ *)

(* The primitive set of P.Effects.PureScalars (includes Phys_equal,
   listed there for completeness). *)
Definition scalar_pure_prim (op : prim_op) : Prop :=
  match op with
  | Op_unary (UP_int_arith _ _)
  | Op_unary (UP_num_conv _ _)
  | Op_unary UP_boolean_not
  | Op_unary (UP_reinterpret_64_bit_word _)
  | Op_unary UP_reinterpret_boxed_vector
  | Op_unary UP_tag_immediate
  | Op_unary UP_untag_immediate
  | Op_unary (UP_unbox_number _)
  | Op_binary (BP_int_arith _ _)
  | Op_binary (BP_int_shift _ _)
  | Op_binary (BP_int_comp _ _)
  | Op_binary (BP_phys_equal _) => True
  | _ => False
  end.

Definition scalar_float_prim (op : prim_op) : Prop :=
  match op with
  | Op_unary (UP_float_arith _ _)
  | Op_binary (BP_float_arith _ _)
  | Op_binary (BP_float_comp _ _) => True
  | _ => False
  end.

(** RULE P.Effects.PureScalars (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
    CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_binary_primitive *)
Theorem P_Effects_PureScalars :
  forall fl op,
    scalar_pure_prim op ->
    effects_of fl op = EC_pure.
Admitted.

(** RULE P.Effects.FloatRoundingMode (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
    CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_binary_primitive
    ENCODING NOTE: Flambda_features.float_const_prop () is modeled as
    the ef_float_const_prop field of the eff_flags parameter. *)
Theorem P_Effects_FloatRoundingMode :
  forall fl op,
    scalar_float_prim op ->
    effects_of fl op =
      (if ef_float_const_prop fl then EC_pure else EC_read).
Admitted.

(** RULE P.Effects.BoxNumber (STATUS normative)
    -- 05-primitives-scalar.md
    CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive
    ENCODING NOTE: the doc's conclusion fixes the effects and
    coeffects components and (in NOTES) the placement -- Delay for
    heap allocations in classic mode, Strict for local; the validity
    component is elided ("...") in the doc and left unconstrained
    here. Classic mode is the ef_classic_mode flag. *)
Theorem P_Effects_BoxNumber :
  forall fl bn am,
    ece_effects (effects_of fl (Op_unary (UP_box_number bn am)))
      = Only_generative_effects Immutable
    /\ ece_coeffects (effects_of fl (Op_unary (UP_box_number bn am)))
      = coeffects_of_mode am
    /\ ece_placement (effects_of fl (Op_unary (UP_box_number bn am)))
      = match am with
        | Alloc_heap =>
          if ef_classic_mode fl then Delay else Strict
        | Alloc_local _ => Strict
        end.
Admitted.

(* ------------------------------------------------------------------ *)
(* Rule coverage for this file (31 ch. 05 rules):                      *)
(*   P.Contract.NoRaiseNoControl                                       *)
(*   P.Unary.IntArith.SwapByteEndianness  P.Unary.FloatArith           *)
(*   P.Unary.NumConv  P.Unary.NumConv.FloatToInt.OutOfRange            *)
(*   P.Unary.NumConv.Int32ToInt64.SignExtend  P.Unary.BooleanNot       *)
(*   P.Unary.TagImmediate  P.Unary.UntagImmediate                      *)
(*   P.Unary.Reinterpret64.Int64AsFloat64                              *)
(*   P.Unary.Reinterpret64.Float64AsInt64                              *)
(*   P.Unary.Reinterpret64.Int64AsTaggedInt63                          *)
(*   P.Unary.Reinterpret64.TaggedInt63AsInt64                          *)
(*   P.Unary.ReinterpretBoxedVector  P.Unary.BoxNumber                 *)
(*   P.Unary.UnboxNumber  P.Binary.IntArith.Total                      *)
(*   P.Binary.IntArith.DivMod  P.Binary.IntArith.DivModByZero          *)
(*   P.Binary.IntArith.DivMinIntByMinusOne  P.Binary.IntShift          *)
(*   P.Binary.IntShift.OutOfRange.FolderPicksZero (descriptive)        *)
(*   P.Binary.IntShift.ByZero  P.Binary.IntComp.Bool                   *)
(*   P.Binary.IntComp.CompareFunction  P.Binary.FloatArith             *)
(*   P.Binary.FloatComp.Bool  P.Binary.FloatComp.CompareFunction       *)
(*   P.Effects.PureScalars  P.Effects.FloatRoundingMode                *)
(*   P.Effects.BoxNumber                                               *)
(* ------------------------------------------------------------------ *)
