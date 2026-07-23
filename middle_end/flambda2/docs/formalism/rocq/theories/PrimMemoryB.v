(** * PrimMemoryB.v — memory primitives, second half of ch. 06

    Mechanizes the "B" half of 06-primitives-memory.md (split of record
    with PrimMemoryA.v, Curry): observers (is_int/is_null/tags/headers),
    lengths, closure projections, array/string/bigarray loads and
    stores, phys_equal, the atomic and raw-memory primitives (anchors),
    nullary primitives, and the unchecked-access contract.  33 rules:
    23 defining clauses of [denot_mem_b] (19 normative, 4 conjectured),
    9 documented anchors, and P.Nullary.ControlBarriers (effects table
    plus its data clauses).

    Owner: Girard (wave 3).  Imports: Base (incl. the shared float
    Parameter set, catalog entry 5), Syntax, Values, PrimMemoryA
    (effects-axes types). *)

From Stdlib Require Import ZArith List Bool.
From Flambda2 Require Import Base Syntax Values PrimMemoryA.
Import ListNotations.

Open Scope Z_scope.

(* ================================================================== *)
(* 1. Helpers                                                         *)
(* ================================================================== *)

(** The raw machine address of a bigstring's off-heap data.
    ENCODING NOTE (sanctioned Parameter, coordinator ruling): raw data
    addresses are not representable in [heap := fmap heap_key
    heap_object].  [data_ptr] is an opaque assignment of byte addresses
    to locations, used ONLY by P.Binary.BigarrayGetAlignment — whose
    entire content is the alignment formula on this address.  Being a
    function, it gives address consistency across uses for free. *)
Parameter data_ptr : location -> Z.

(** "v1 and v2 are the same machine word" (P.Binary.PhysEqual): same
    immediate, same pointer, or both null.
    ENCODING NOTE: stated on value forms, not representation words.
    Cross-form comparisons (e.g. [V_ptr] vs [V_clos] into the same
    set-of-closures block) yield [false]; whether such pairs can alias
    as machine words is a representation concern (ch. 17), invisible at
    this level. *)
Definition phys_same_word (v1 v2 : value) : bool :=
  match v1, v2 with
  | V_tagged_imm a, V_tagged_imm b => Z.eqb a b
  | V_ptr a1, V_ptr a2 => address_eqb a1 a2
  | V_clos l1 f1, V_clos l2 f2 =>
      location_eqb l1 l2 && function_slot_eqb f1 f2
  | V_null, V_null => true
  | _, _ => false
  end.

(* ------------------------------------------------------------------ *)
(* 1b. Byte-level helpers (string/bytes/bigstring accesses)           *)
(* ------------------------------------------------------------------ *)

(** Little-endian byte assembly: [le_bytes_to_Z [b0; ...; b_{w-1}]] =
    b0 + 256*b1 + ... — "word0 least significant"
    (P.Binary.StringOrBigstringLoad). *)
Fixpoint le_bytes_to_Z (bs : list Z) : Z :=
  match bs with
  | [] => 0
  | b :: bs' => b + 256 * le_bytes_to_Z bs'
  end.

(** The w-byte little-endian encoding of n (low bits, two's
    complement). *)
Fixpoint Z_to_le_bytes (w : nat) (n : Z) : list Z :=
  match w with
  | O => []
  | S w' => (n mod 256) :: Z_to_le_bytes w' (n / 256)
  end.

(** bbar[j .. j+|new|-1] := new (functional splice; also used for
    array element runs in the vector-store rules). *)
Definition list_splice {A : Type} (j : nat) (new : list A)
  (l : list A) : list A :=
  firstn j l ++ new ++ skipn (j + length new)%nat l.

(** bbar[j .. j+w-1]. *)
Definition bytes_slice (j w : nat) (bs : list Z) : list Z :=
  firstn w (skipn j bs).

(** w = byte_width_of_string_accessor_width width
    (P.Binary.StringOrBigstringLoad; taxonomy, ch. 06 SS
    "Access-kind taxonomy"). *)
Definition byte_width_of_string_accessor_width
  (w : string_accessor_width) : Z :=
  match w with
  | SAW_eight | SAW_eight_signed => 1
  | SAW_sixteen | SAW_sixteen_signed => 2
  | SAW_thirty_two | SAW_single => 4
  | SAW_sixty_four => 8
  | SAW_one_twenty_eight _ => 16
  | SAW_two_fifty_six _ => 32
  | SAW_five_twelve _ => 64
  end.

(** The `aligned` flag of the vector widths (alignment-undef premise
    of the aligned variants). *)
Definition saw_aligned (w : string_accessor_width) : bool :=
  match w with
  | SAW_one_twenty_eight a | SAW_two_fifty_six a | SAW_five_twelve a =>
      a
  | _ => false
  end.

(** float32 <-> IEEE bit patterns (f32_of_bits / f32_to_bits) come
    from Base.v's shared float Parameter set, as do the widths'
    conversions used below (f64_of_f32 exact widening, f32_of_f64
    rounding). *)

(** decode_width(bbar[j .. j+w-1]) from the assembled little-endian
    integer u (taxonomy: Eight/Sixteen zero-extend to
    naked_immediate; Eight_signed/Sixteen_signed are naked_int8/16;
    Thirty_two -> naked_int32; Single -> naked_float32; Sixty_four ->
    naked_int64; vector widths -> naked_vec128/256/512, word0 least
    significant). *)
Definition decode_saw (w : string_accessor_width) (u : Z) : value :=
  match w with
  | SAW_eight | SAW_sixteen => V_naked_imm u
  | SAW_eight_signed => V_naked_int8 (wrap 8 u)
  | SAW_sixteen_signed => V_naked_int16 (wrap 16 u)
  | SAW_thirty_two => V_naked_int32 (wrap 32 u)
  | SAW_single => V_naked_float32 (f32_of_bits u)
  | SAW_sixty_four => V_naked_int64 (wrap 64 u)
  | SAW_one_twenty_eight _ => V_naked_vec128 (Mk_vec128 u)
  | SAW_two_fifty_six _ => V_naked_vec256 (Mk_vec256 u)
  | SAW_five_twelve _ => V_naked_vec512 (Mk_vec512 u)
  end.

(** The w-byte little-endian encoding of v
    (P.Ternary.BytesOrBigstringSet: "the payload kind follows the
    width as for loads"); [None] on a payload of the wrong kind. *)
Definition encode_saw (w : string_accessor_width) (v : value)
  : option Z :=
  match w, v with
  | SAW_eight, V_naked_imm n => Some (n mod 256)
  | SAW_eight_signed, V_naked_int8 n => Some (n mod 256)
  | SAW_sixteen, V_naked_imm n => Some (n mod 65536)
  | SAW_sixteen_signed, V_naked_int16 n => Some (n mod 65536)
  | SAW_thirty_two, V_naked_int32 n => Some (n mod 2 ^ 32)
  | SAW_single, V_naked_float32 f => Some (f32_to_bits f)
  | SAW_sixty_four, V_naked_int64 n => Some (n mod 2 ^ 64)
  | SAW_one_twenty_eight _, V_naked_vec128 (Mk_vec128 u) => Some u
  | SAW_two_fifty_six _, V_naked_vec256 (Mk_vec256 u) => Some u
  | SAW_five_twelve _, V_naked_vec512 (Mk_vec512 u) => Some u
  | _, _ => None
  end.

(* ------------------------------------------------------------------ *)
(* 1c. Bigarray element decode/encode (taxonomy table, ch. 06)        *)
(* ------------------------------------------------------------------ *)

(** Rounding of a naked_float to 16-bit precision (widened back).
    ENCODING NOTE: there is no 16-bit float carrier type, so
    BGK_float16 elements are stored as the WIDENED naked_float,
    rounded to float16 precision on store by this opaque Parameter.
    The taxonomy's "widen/narrow on access" is thus modeled with the
    narrowing at store time and the load a no-op.  Declared locally
    (only the bigarray denotations use it); if ch. 19's Cmm-side
    bigarray accesses ever need float16 rounding it must move to
    Base.v's shared set (catalog entry 5). *)
Parameter f16_round : float64 -> float64.

(** decode_bk: stored element value -> loaded value
    (P.Binary.BigarrayLoad; taxonomy table).  Float16/Float32 widen
    to naked_float (Float16 stored pre-rounded, see f16_round;
    Float32 stored as naked_float32, widened here); Sint8/16 are
    stored sign-extended and Uint8/16 zero-extended (the extension
    is performed at store time by [bigarray_encode]); the remaining
    kinds read at their stored width.  [None] on a stored value of
    the wrong shape, and on Complex32/64 — the complex read is the
    separate allocating clause of the rule. *)
Definition bigarray_decode (bk : bigarray_kind) (sv : value)
  : option value :=
  match bk, sv with
  | BGK_float16, V_naked_float f => Some (V_naked_float f)
  | BGK_float32, V_naked_float32 f =>
      Some (V_naked_float (f64_of_f32 f))
  | BGK_float32_t, V_naked_float32 f => Some (V_naked_float32 f)
  | BGK_float64, V_naked_float f => Some (V_naked_float f)
  | BGK_sint8, V_naked_imm n => Some (V_naked_imm n)
  | BGK_uint8, V_naked_imm n => Some (V_naked_imm n)
  | BGK_sint16, V_naked_imm n => Some (V_naked_imm n)
  | BGK_uint16, V_naked_imm n => Some (V_naked_imm n)
  | BGK_int32, V_naked_int32 n => Some (V_naked_int32 n)
  | BGK_int64, V_naked_int64 n => Some (V_naked_int64 n)
  | BGK_int_width_int, V_naked_imm n => Some (V_naked_imm n)
  | BGK_targetint_width_int, V_naked_nativeint n =>
      Some (V_naked_nativeint n)
  | _, _ => None
  end.

(** encode_bk: payload value (of kind element_kind(bk)) -> stored
    element value (P.Ternary.BigarraySet: Float16/Float32 round the
    naked_float to 16-/32-bit precision, Sint8/Uint8/Sint16/Uint16
    truncate to the low bits — stored here as the sign-/zero-
    extended representative so that [bigarray_decode] is the
    identity on it).  [None] on kind mismatch or Complex32/64 (the
    complex store is the separate pair-storing clause). *)
Definition bigarray_encode (bk : bigarray_kind) (v : value)
  : option value :=
  match bk, v with
  | BGK_float16, V_naked_float f => Some (V_naked_float (f16_round f))
  | BGK_float32, V_naked_float f =>
      Some (V_naked_float32 (f32_of_f64 f))
  | BGK_float32_t, V_naked_float32 f => Some (V_naked_float32 f)
  | BGK_float64, V_naked_float f => Some (V_naked_float f)
  | BGK_sint8, V_naked_imm n => Some (V_naked_imm (wrap 8 n))
  | BGK_uint8, V_naked_imm n => Some (V_naked_imm (n mod 256))
  | BGK_sint16, V_naked_imm n => Some (V_naked_imm (wrap 16 n))
  | BGK_uint16, V_naked_imm n => Some (V_naked_imm (n mod 65536))
  | BGK_int32, V_naked_int32 n => Some (V_naked_int32 n)
  | BGK_int64, V_naked_int64 n => Some (V_naked_int64 n)
  | BGK_int_width_int, V_naked_imm n => Some (V_naked_imm n)
  | BGK_targetint_width_int, V_naked_nativeint n =>
      Some (V_naked_nativeint n)
  | _, _ => None
  end.

(** Complex halves as stored (catalog entry 9: Complex32/64 elements
    are unarized scalar pairs at STORED width — V_naked_float32
    halves for Complex32, V_naked_float halves for Complex64).
    [complex_half_stored] encodes a boxed-complex part for storage
    (P.Ternary.BigarraySet: rounding to single precision for
    Complex32); [complex_half_load] widens a stored half back to the
    naked_float of the boxed complex (P.Binary.BigarrayLoad).  [None]
    on non-complex kinds or a stored value of the wrong shape. *)
Definition complex_half_stored (bk : bigarray_kind) (f : float64)
  : option value :=
  match bk with
  | BGK_complex32 => Some (V_naked_float32 (f32_of_f64 f))
  | BGK_complex64 => Some (V_naked_float f)
  | _ => None
  end.

Definition complex_half_load (bk : bigarray_kind) (sv : value)
  : option float64 :=
  match bk, sv with
  | BGK_complex32, V_naked_float32 f => Some (f64_of_f32 f)
  | BGK_complex64, V_naked_float f => Some f
  | _, _ => None
  end.

(* ------------------------------------------------------------------ *)
(* 1d. Packed-layout bits for SIMD reinterpret array accesses         *)
(*     (P.Binary.ArrayLoad.Vector / P.Ternary.ArraySet.Vector;        *)
(*     packed little-endian layout per ch. 17 R.Obj.Array)            *)
(* ------------------------------------------------------------------ *)

(** w_e = the element byte width of ak (w(ak), R.Obj.Array).  [None]
    for Values/Gc_ignorable_values (vector access is a fatal error at
    to_cmm) and for unboxed products other than the all-vec128 case
    (the 2-or-4-component restriction is a rule premise). *)
Definition array_elem_byte_width (ak : array_kind) : option Z :=
  match ak with
  | AK_immediates => Some 8
  | AK_naked_floats => Some 8
  | AK_naked_float32s => Some 4
  | AK_naked_ints => Some 8
  | AK_naked_int8s => Some 1
  | AK_naked_int16s => Some 2
  | AK_naked_int32s => Some 4
  | AK_naked_int64s => Some 8
  | AK_naked_nativeints => Some 8
  | AK_naked_vec128s => Some 16
  | AK_naked_vec256s => Some 32
  | AK_naked_vec512s => Some 64
  | AK_unboxed_product ks =>
      if forallb
           (fun k => match k with
                     | AK_naked_vec128s => true
                     | _ => false
                     end) ks
      then Some 16
      else None
  | AK_values | AK_gc_ignorable_values => None
  end.

(** The unsigned bit pattern of one (unarized) element of an ak
    array under the packed layout.  Immediates arrays store TAGGED
    words (2n+1, R.Obj.Array); naked numbers store their two's-
    complement/IEEE bits at w(ak); vectors store their pattern. *)
Definition array_scalar_bits (ak : array_kind) (v : value)
  : option Z :=
  match ak, v with
  | AK_immediates, V_tagged_imm n => Some ((2 * n + 1) mod 2 ^ 64)
  | AK_naked_floats, V_naked_float f => Some (f64_to_bits f)
  | AK_naked_float32s, V_naked_float32 f => Some (f32_to_bits f)
  | AK_naked_ints, V_naked_imm n => Some (n mod 2 ^ 64)
  | AK_naked_int8s, V_naked_int8 n => Some (n mod 2 ^ 8)
  | AK_naked_int16s, V_naked_int16 n => Some (n mod 2 ^ 16)
  | AK_naked_int32s, V_naked_int32 n => Some (n mod 2 ^ 32)
  | AK_naked_int64s, V_naked_int64 n => Some (n mod 2 ^ 64)
  | AK_naked_nativeints, V_naked_nativeint n => Some (n mod 2 ^ 64)
  | AK_naked_vec128s, V_naked_vec128 (Mk_vec128 u) => Some u
  | AK_naked_vec256s, V_naked_vec256 (Mk_vec256 u) => Some u
  | AK_naked_vec512s, V_naked_vec512 (Mk_vec512 u) => Some u
  | AK_unboxed_product _, V_naked_vec128 (Mk_vec128 u) => Some u
  | _, _ => None
  end.

(** The packed bits of a run of consecutive elements (first element
    least significant — little-endian, side by side). *)
Fixpoint packed_scalar_bits (ak : array_kind) (vs : list value)
  : option Z :=
  match vs with
  | [] => Some 0
  | v :: vs' =>
      match array_scalar_bits ak v, array_elem_byte_width ak,
            packed_scalar_bits ak vs' with
      | Some b, Some w, Some rest => Some (b + 2 ^ (8 * w) * rest)
      | _, _, _ => None
      end
  end.

(* ------------------------------------------------------------------ *)
(* 1e. Object and access-descriptor helpers                           *)
(* ------------------------------------------------------------------ *)

(** "ANY pointer (block, array, closure, boxed number, ...)" — the
    pointer-shaped value forms (P.Unary.IsInt.Pointer NOTES; "null is
    a pointer-shaped value"). *)
Definition pointer_shaped (v : value) : bool :=
  match v with
  | V_ptr _ | V_clos _ _ | V_null => true
  | _ => false
  end.

(** The runtime tag of a tag-readable heap object (P.Unary.GetTag:
    "Block(t, mu, vbar) (or FloatBlock/MixedBlock with runtime tag
    t)").  A FloatBlock's runtime tag is Double_array_tag = 254. *)
Definition block_runtime_tag (o : heap_object) : option Z :=
  match o with
  | HO_Block (Mk_tag n) _ _ => Some (Z.of_nat n)
  | HO_FloatBlock _ _ => Some 254
  | HO_MixedBlock (Mk_tag n) _ _ _ => Some (Z.of_nat n)
  | _ => None
  end.

Definition no_scan_tag : Z := 251.

(** Whether o is stored with Double_array_tag
    (P.Unary.IsBoxedFloat NOTES: Is_flat_float_array "test[s] for a
    Double_array_tag (FloatBlock) array"; a Naked_floats array also
    carries Double_array_tag at runtime, ch. 17 R.Obj.Array). *)
Definition flat_float_array_obj (o : heap_object) : bool :=
  match o with
  | HO_FloatBlock _ _ => true
  | HO_Array AK_naked_floats _ _ => true
  | _ => false
  end.

(** "ak consistent with akl" (P.Unary.ArrayLength):
    Float_array_opt_dynamic length reads apply to any array (the
    dynamic float-array-optimisation case). *)
Inductive akl_consistent
  : array_kind_for_length -> array_kind -> Prop :=
  | Akl_exact : forall ak, akl_consistent (AKL_array_kind ak) ak
  | Akl_dynamic : forall ak,
      akl_consistent AKL_float_array_opt_dynamic ak.

(** The load kind naming the element kind of a SCALAR (non-product)
    array kind. *)
Definition alk_of_scalar_array_kind (ak : array_kind)
  : option array_load_kind :=
  match ak with
  | AK_immediates => Some ALK_immediates
  | AK_gc_ignorable_values => Some ALK_gc_ignorable_values
  | AK_values => Some ALK_values
  | AK_naked_floats => Some ALK_naked_floats
  | AK_naked_float32s => Some ALK_naked_float32s
  | AK_naked_ints => Some ALK_naked_ints
  | AK_naked_int8s => Some ALK_naked_int8s
  | AK_naked_int16s => Some ALK_naked_int16s
  | AK_naked_int32s => Some ALK_naked_int32s
  | AK_naked_int64s => Some ALK_naked_int64s
  | AK_naked_nativeints => Some ALK_naked_nativeints
  | AK_naked_vec128s => Some ALK_naked_vec128s
  | AK_naked_vec256s => Some ALK_naked_vec256s
  | AK_naked_vec512s => Some ALK_naked_vec512s
  | AK_unboxed_product _ => None
  end.

(** "lk = the (unarized) element kind of ak" at scalar index j
    (P.Binary.ArrayLoad): for an Unboxed_product array "an
    Unboxed_product element occupies several consecutive scalar
    indices" (NOTES), so the cell kind is that of component
    j mod width. *)
Inductive array_cell_load_kind
  : array_kind -> nat -> array_load_kind -> Prop :=
  | Cell_scalar : forall ak j lk,
      alk_of_scalar_array_kind ak = Some lk ->
      array_cell_load_kind ak j lk
  | Cell_product : forall ks j akj lk,
      nth_error ks (Nat.modulo j (length ks)) = Some akj ->
      alk_of_scalar_array_kind akj = Some lk ->
      array_cell_load_kind (AK_unboxed_product ks) j lk.

(** Set kinds name element kinds exactly as load kinds do
    (P.Ternary.ArraySet: "sk = the (unarized) element kind of ak");
    the Values payload (Init_or_assign) drives only the GC write
    barrier. *)
Definition alk_of_ask (sk : array_set_kind) : array_load_kind :=
  match sk with
  | ASK_immediates => ALK_immediates
  | ASK_gc_ignorable_values => ALK_gc_ignorable_values
  | ASK_values _ => ALK_values
  | ASK_naked_floats => ALK_naked_floats
  | ASK_naked_float32s => ALK_naked_float32s
  | ASK_naked_ints => ALK_naked_ints
  | ASK_naked_int8s => ALK_naked_int8s
  | ASK_naked_int16s => ALK_naked_int16s
  | ASK_naked_int32s => ALK_naked_int32s
  | ASK_naked_int64s => ALK_naked_int64s
  | ASK_naked_nativeints => ALK_naked_nativeints
  | ASK_naked_vec128s => ALK_naked_vec128s
  | ASK_naked_vec256s => ALK_naked_vec256s
  | ASK_naked_vec512s => ALK_naked_vec512s
  end.

(** w_v of a vector load/set kind (P.Binary.ArrayLoad.Vector). *)
Definition vector_width_bytes (lk : array_load_kind) : option Z :=
  match lk with
  | ALK_naked_vec128s => Some 16
  | ALK_naked_vec256s => Some 32
  | ALK_naked_vec512s => Some 64
  | _ => None
  end.

(** The naked_vecN value of the packed bits b, per the vector
    load/set kind. *)
Definition vec_value_of_bits (lk : array_load_kind) (b : Z)
  : option value :=
  match lk with
  | ALK_naked_vec128s => Some (V_naked_vec128 (Mk_vec128 b))
  | ALK_naked_vec256s => Some (V_naked_vec256 (Mk_vec256 b))
  | ALK_naked_vec512s => Some (V_naked_vec512 (Mk_vec512 b))
  | _ => None
  end.

(** The Unboxed_product restriction of P.Binary.ArrayLoad.Vector:
    products are excluded "except 2 or 4 x Naked_vec128s,
    lk = Naked_vec128s" (the element-width filtering itself is
    array_elem_byte_width). *)
Definition vector_access_kind_ok (ak : array_kind)
  (lk : array_load_kind) : Prop :=
  match ak with
  | AK_unboxed_product ks =>
      (length ks = 2%nat \/ length ks = 4%nat)
      /\ lk = ALK_naked_vec128s
  | _ => True
  end.

(** The byte contents of the string-like object at a, per slv
    (P.Binary.StringOrBigstringLoad's object premise: Bytes for the
    String/Bytes tags, Bigstring off-heap). *)
Inductive string_like_bytes (H : heap) (a : address)
  : string_like_value -> list Z -> Prop :=
  | Slb_string : forall mu bs,
      heap_get_addr H a = Some (HO_Bytes mu bs) ->
      string_like_bytes H a SLV_string bs
  | Slb_bytes : forall mu bs,
      heap_get_addr H a = Some (HO_Bytes mu bs) ->
      string_like_bytes H a SLV_bytes bs
  | Slb_bigstring : forall bs,
      heap_get_addr H a = Some (HO_Bigstring bs) ->
      string_like_bytes H a SLV_bigstring bs.

(** Same for the write side (P.Ternary.BytesOrBigstringSet: the
    Bytes case requires Mutable), plus the write-back of the new
    byte contents. *)
Inductive bytes_like_bytes (H : heap) (a : address)
  : bytes_like_value -> list Z -> Prop :=
  | Blb_bytes : forall bs,
      heap_get_addr H a = Some (HO_Bytes Mutable bs) ->
      bytes_like_bytes H a BLV_bytes bs
  | Blb_bigstring : forall bs,
      heap_get_addr H a = Some (HO_Bigstring bs) ->
      bytes_like_bytes H a BLV_bigstring bs.

Inductive bytes_like_update (H : heap) (a : address)
  : bytes_like_value -> list Z -> heap -> Prop :=
  | Blu_bytes : forall bs',
      bytes_like_update H a BLV_bytes bs'
        (heap_upd H (HK_addr a) (HO_Bytes Mutable bs'))
  | Blu_bigstring : forall bs',
      bytes_like_update H a BLV_bigstring bs'
        (heap_upd H (HK_addr a) (HO_Bigstring bs')).

Definition zprod (xs : list Z) : Z := fold_right Z.mul 1 xs.

(** Ceiling division, for m = ceil(w_v / w_e). *)
Definition zceil_div (a b : Z) : Z := (a + b - 1) / b.

(** The base constraint of Write_offset's wk payload
    (P.Ternary.WriteOffset: Into_block requires a block pointer;
    Into_block_or_off_heap additionally allows NULL). *)
Definition wok_base_ok (wk : write_offset_kind) (b : value) : Prop :=
  match wk with
  | WOK_into_block => exists a, b = V_ptr a
  | WOK_into_block_or_off_heap =>
      (exists a, b = V_ptr a) \/ b = V_null
  end.

(* ================================================================== *)
(* 2. denot_mem_b — the defining clauses                              *)
(* ================================================================== *)

(* ENCODING NOTE (undef policy, coordinator ruling; mirrored in
   Opsem.v): Only the doc's crisply-stated undef conditions appear
   as explicit PR_undef clauses (out-of-range indices, writes to
   immutable objects, misaligned `aligned` accesses, reaching
   Invalid); representation-mismatch inputs are simply not related
   (no clause), so in the machine they surface as STUCK non-final
   configurations.  See Opsem.v's goes-wrong classification:
   OS.Unit.Final classifies "reaching OS.Invalid or a stuck state"
   as undefined behaviour, so nothing is lost. *)

Inductive denot_mem_b
  : prim_op -> list value -> heap -> prim_result -> Prop :=

  (** RULE P.Unary.IsInt.Immediate (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int *)
  (* variant_only is a frontend promise driving only Simplify's
     is-int relation; it does not change the runtime result. *)
  | P_Unary_IsInt_Immediate :
      forall vo n H,
        denot_mem_b (Op_unary (UP_is_int vo)) [V_tagged_imm n] H
          (PR_ok (V_naked_imm 1) H)

  (** RULE P.Unary.IsInt.Pointer (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_is_int *)
  (* The doc's premise is v = ptr l; per the NOTES "Is_int on ANY
     pointer (block, array, closure, boxed number, ...) yields 0"
     and null is pointer-shaped, hence pointer_shaped (V_ptr,
     V_clos, V_null). *)
  | P_Unary_IsInt_Pointer :
      forall vo v H,
        pointer_shaped v = true ->
        denot_mem_b (Op_unary (UP_is_int vo)) [v] H
          (PR_ok (V_naked_imm 0) H)

  (** RULE P.Unary.IsNull (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive *)
  | P_Unary_IsNull_Null :
      forall H,
        denot_mem_b (Op_unary UP_is_null) [V_null] H
          (PR_ok (V_naked_imm 1) H)
  | P_Unary_IsNull_NonNull :
      forall v H,
        v <> V_null ->
        denot_mem_b (Op_unary UP_is_null) [v] H
          (PR_ok (V_naked_imm 0) H)

  (** RULE P.Unary.GetTag (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_get_tag *)
  (* The undef prose (objects whose tag is not readable this way;
     immediate argument, itself only conjectured undef) is left as
     stuckness per the policy note above. *)
  | P_Unary_GetTag :
      forall a o t H,
        heap_get_addr H a = Some o ->
        block_runtime_tag o = Some t ->
        denot_mem_b (Op_unary UP_get_tag) [V_ptr a] H
          (PR_ok (V_naked_imm t) H)

  (** RULE P.Unary.GetHeader (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive *)
  (* hdr(o) is target-specific and "not modelled precisely" (NOTES):
     kind-constrained nondeterministic result (coordinator ruling,
     same treatment as ReadOffset) — any naked nativeint.
     Width disclosure (coordinator ruling): hdr is a raw Z with no
     word-width bound; a global value well-formedness predicate is
     deferred as proof-phase debt (see catalog). *)
  | P_Unary_GetHeader :
      forall a o t hdr H,
        heap_get_addr H a = Some o ->
        block_runtime_tag o = Some t ->
        t <= no_scan_tag ->
        denot_mem_b (Op_unary UP_get_header) [V_ptr a] H
          (PR_ok (V_naked_nativeint hdr) H)

  (** RULE P.Unary.ArrayLength (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_array_length *)
  (* n is the UNARIZED length: |elems| (HO_Array stores the unarized
     element sequence). *)
  | P_Unary_ArrayLength :
      forall akl ak a mu vs H,
        heap_get_addr H a = Some (HO_Array ak mu vs) ->
        akl_consistent akl ak ->
        denot_mem_b (Op_unary (UP_array_length akl)) [V_ptr a] H
          (PR_ok (V_tagged_imm (Z.of_nat (length vs))) H)

  (** RULE P.Unary.StringLength (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_string_length *)
  | P_Unary_StringLength :
      forall sb a mu bs H,
        heap_get_addr H a = Some (HO_Bytes mu bs) ->
        denot_mem_b (Op_unary (UP_string_length sb)) [V_ptr a] H
          (PR_ok (V_naked_imm (Z.of_nat (length bs))) H)

  (** RULE P.Unary.BigarrayLength (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_unary_primitive *)
  (* 1 <= d <= n: nth_error at d-1 supplies the upper bound.
     Classified reading_from_a_block(Mutable) — NOT freely CSE-able
     (NOTES). *)
  | P_Unary_BigarrayLength :
      forall d a bk layout dims elems dd H,
        heap_get_addr H a
          = Some (HO_Bigarray bk layout dims elems) ->
        (1 <= d)%nat ->
        nth_error dims (d - 1)%nat = Some dd ->
        denot_mem_b (Op_unary (UP_bigarray_length d)) [V_ptr a] H
          (PR_ok (V_naked_imm dd) H)

  (** RULE P.Unary.ProjectFunctionSlot (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_function_slot *)
  (* ENCODING NOTE: the doc writes ptr l for both argument and
     result, but a closure value in this model is V_clos l f (the
     doc's clos l f, 04-opsem.md §1.1) — "the closure for move_to
     within the same set" identifies a closure by its slot, which a
     bare ptr cannot.  Static sets of closures also live at a fresh
     LOCATION (OS.Let.Static binds sym_j to clos l f_j), so l ranges
     over locations only. *)
  | P_Unary_ProjectFunctionSlot :
      forall mf mt l funs venv H,
        H (HK_addr (Addr_loc l)) = Some (HO_Closures funs venv) ->
        funs mf <> None ->
        funs mt <> None ->
        denot_mem_b (Op_unary (UP_project_function_slot mf mt))
          [V_clos l mf] H (PR_ok (V_clos l mt) H)

  (** RULE P.Unary.ProjectValueSlot (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive
      CODE middle_end/flambda2/simplify/simplify_unary_primitive.ml#simplify_project_value_slot *)
  (* env(l, project_from)(w) = v is the venv lookup; the set's value
     environment is shared, reached via the closure for
     project_from.  Same V_clos encoding as ProjectFunctionSlot. *)
  | P_Unary_ProjectValueSlot :
      forall pf w l funs venv v H,
        H (HK_addr (Addr_loc l)) = Some (HO_Closures funs venv) ->
        funs pf <> None ->
        venv w = Some v ->
        denot_mem_b (Op_unary (UP_project_value_slot pf w))
          [V_clos l pf] H (PR_ok v H)

  (** RULE P.Unary.IsBoxedFloat (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive *)
  (* Four clauses under one rule id: the NOTES define
     Is_flat_float_array as the analogous Double_array_tag test.
     Both are only valid under the float-array optimisation. *)
  | P_Unary_IsBoxedFloat_Yes :
      forall a v H,
        heap_get_addr H a = Some (HO_Boxed K_naked_float v) ->
        denot_mem_b (Op_unary UP_is_boxed_float) [V_ptr a] H
          (PR_ok (V_naked_imm 1) H)
  | P_Unary_IsBoxedFloat_No :
      forall a o H,
        heap_get_addr H a = Some o ->
        (forall v, o <> HO_Boxed K_naked_float v) ->
        denot_mem_b (Op_unary UP_is_boxed_float) [V_ptr a] H
          (PR_ok (V_naked_imm 0) H)
  | P_Unary_IsFlatFloatArray_Yes :
      forall a o H,
        heap_get_addr H a = Some o ->
        flat_float_array_obj o = true ->
        denot_mem_b (Op_unary UP_is_flat_float_array) [V_ptr a] H
          (PR_ok (V_naked_imm 1) H)
  | P_Unary_IsFlatFloatArray_No :
      forall a o H,
        heap_get_addr H a = Some o ->
        flat_float_array_obj o = false ->
        denot_mem_b (Op_unary UP_is_flat_float_array) [V_ptr a] H
          (PR_ok (V_naked_imm 0) H)

  (** RULE P.Binary.ArrayLoad (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
      CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load *)
  (* mu ties the primitive's payload to the array's mutability as in
     the doc (it drives only the coeffect).  nth_error carries the
     upper half of 0 <= j < n together with v_j.  A vector lk over a
     different element kind is the .Vector clause below;
     representation mismatch is stuck (policy note). *)
  | P_Binary_ArrayLoad :
      forall ak lk mu a vs j v H,
        heap_get_addr H a = Some (HO_Array ak mu vs) ->
        0 <= j ->
        nth_error vs (Z.to_nat j) = Some v ->
        array_cell_load_kind ak (Z.to_nat j) lk ->
        denot_mem_b (Op_binary (BP_array_load ak lk mu))
          [V_ptr a; V_tagged_imm j] H (PR_ok v H)
  (* Companion: "j out of range is undef". *)
  | P_Binary_ArrayLoad_OutOfRange :
      forall ak lk mu a vs j H,
        heap_get_addr H a = Some (HO_Array ak mu vs) ->
        j < 0 \/ Z.of_nat (length vs) <= j ->
        denot_mem_b (Op_binary (BP_array_load ak lk mu))
          [V_ptr a; V_tagged_imm j] H PR_undef

  (** RULE P.Binary.ArrayLoad.Vector (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_load
      CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#array_vector_access_validity_condition
      CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_array_load *)
  (* b = "the w_v bytes of elements v_j ... v_{j+m-1} under the
     array's packed little-endian layout" is packed_scalar_bits of
     the m-element slice (§1d).  No alignment requirement (NOTES:
     arrays are only word-aligned; to_cmm emits unaligned loads). *)
  | P_Binary_ArrayLoad_Vector :
      forall ak lk mu wv we m a vs j b vv H,
        vector_width_bytes lk = Some wv ->
        array_elem_byte_width ak = Some we ->
        vector_access_kind_ok ak lk ->
        m = zceil_div wv we ->
        heap_get_addr H a = Some (HO_Array ak mu vs) ->
        0 <= j ->
        j + m <= Z.of_nat (length vs) ->
        packed_scalar_bits ak
          (firstn (Z.to_nat m) (skipn (Z.to_nat j) vs)) = Some b ->
        vec_value_of_bits lk b = Some vv ->
        denot_mem_b (Op_binary (BP_array_load ak lk mu))
          [V_ptr a; V_tagged_imm j] H (PR_ok vv H)
  (* Companion: "undef unless all m elements are in range". *)
  | P_Binary_ArrayLoad_Vector_OutOfRange :
      forall ak lk mu wv we m a vs j H,
        vector_width_bytes lk = Some wv ->
        array_elem_byte_width ak = Some we ->
        vector_access_kind_ok ak lk ->
        m = zceil_div wv we ->
        heap_get_addr H a = Some (HO_Array ak mu vs) ->
        j < 0 \/ Z.of_nat (length vs) < j + m ->
        denot_mem_b (Op_binary (BP_array_load ak lk mu))
          [V_ptr a; V_tagged_imm j] H PR_undef

  (** RULE P.Binary.StringOrBigstringLoad (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
      CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_string_or_bigstring_load *)
  (* decode_saw / le_bytes_to_Z transcribe
     decode_width(b[j ... j+w-1]), word0 least significant. *)
  | P_Binary_StringOrBigstringLoad :
      forall slv width w a bs j H,
        w = byte_width_of_string_accessor_width width ->
        string_like_bytes H a slv bs ->
        0 <= j ->
        j + w <= Z.of_nat (length bs) ->
        denot_mem_b
          (Op_binary (BP_string_or_bigstring_load slv width))
          [V_ptr a; V_naked_nativeint j] H
          (PR_ok
             (decode_saw width
                (le_bytes_to_Z
                   (bytes_slice (Z.to_nat j) (Z.to_nat w) bs))) H)
  (* Companions: out of range is undef; for the `aligned` vector
     widths a misaligned access is undef — the model does not track
     byte addresses, so the aligned variants may NONDETERMINISTICALLY
     be undef (this clause deliberately overlaps the ok clause; cf.
     Opsem.v keeping undef_next separate from stuckness). *)
  | P_Binary_StringOrBigstringLoad_OutOfRange :
      forall slv width w a bs j H,
        w = byte_width_of_string_accessor_width width ->
        string_like_bytes H a slv bs ->
        j < 0 \/ Z.of_nat (length bs) < j + w ->
        denot_mem_b
          (Op_binary (BP_string_or_bigstring_load slv width))
          [V_ptr a; V_naked_nativeint j] H PR_undef
  | P_Binary_StringOrBigstringLoad_Misaligned :
      forall slv width a bs j H,
        saw_aligned width = true ->
        string_like_bytes H a slv bs ->
        denot_mem_b
          (Op_binary (BP_string_or_bigstring_load slv width))
          [V_ptr a; V_naked_nativeint j] H PR_undef

  (** RULE P.Binary.PhysEqual (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
      CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_phys_equal *)
  (* REVISED per 13 s4 item 8 (resolution 2026-07-22; CORRESPONDENCE
     entry 75; formerly a deterministic word-equality on all
     operands): RELATIONAL on iota-operands.  Non-iota clauses stay
     deterministic on phys_same_word (§1: "the same machine word --
     same immediate, or same pointer, or both null"); if either
     operand is an iota-operand (is_iota, Values.v -- immutable heap
     objects the compiler may share, duplicate, or lift), result 0 is
     ALWAYS derivable (even for identical pointers: the
     implementation may have duplicated or re-boxed the object) and
     result 1 is derivable exactly up to fold-equality (fold_eq,
     Values.v -- the diagonal instance of 13 s1's folding), so
     result 1 still implies structural equality.  The two iota
     clauses deliberately OVERLAP on fold-equal operands (fold_eq is
     reflexive, fold_eq_refl); Neq is the boolean negation (NOTES).
     The kind-Value premises are the doc's "Only for values of kind
     Value (stated in the mli)" contract: off-kind operands are
     stuck, not compared -- without the guard, phys_same_word's
     catch-all arm would give them a DEFINED wrong answer (identical
     naked immediates "differing"). *)
  | P_Binary_PhysEqual_Eq_Same :
      forall v1 v2 H,
        value_kind v1 = K_value ->
        value_kind v2 = K_value ->
        ~ is_iota H v1 ->
        ~ is_iota H v2 ->
        phys_same_word v1 v2 = true ->
        denot_mem_b (Op_binary (BP_phys_equal EC_eq)) [v1; v2] H
          (PR_ok (V_naked_imm 1) H)
  | P_Binary_PhysEqual_Eq_Diff :
      forall v1 v2 H,
        value_kind v1 = K_value ->
        value_kind v2 = K_value ->
        ~ is_iota H v1 ->
        ~ is_iota H v2 ->
        phys_same_word v1 v2 = false ->
        denot_mem_b (Op_binary (BP_phys_equal EC_eq)) [v1; v2] H
          (PR_ok (V_naked_imm 0) H)
  | P_Binary_PhysEqual_Eq_IotaZero :
      forall v1 v2 H,
        value_kind v1 = K_value ->
        value_kind v2 = K_value ->
        is_iota H v1 \/ is_iota H v2 ->
        denot_mem_b (Op_binary (BP_phys_equal EC_eq)) [v1; v2] H
          (PR_ok (V_naked_imm 0) H)
  | P_Binary_PhysEqual_Eq_IotaOne :
      forall v1 v2 H,
        value_kind v1 = K_value ->
        value_kind v2 = K_value ->
        is_iota H v1 \/ is_iota H v2 ->
        fold_eq H v1 v2 ->
        denot_mem_b (Op_binary (BP_phys_equal EC_eq)) [v1; v2] H
          (PR_ok (V_naked_imm 1) H)
  | P_Binary_PhysEqual_Neq_Same :
      forall v1 v2 H,
        value_kind v1 = K_value ->
        value_kind v2 = K_value ->
        ~ is_iota H v1 ->
        ~ is_iota H v2 ->
        phys_same_word v1 v2 = true ->
        denot_mem_b (Op_binary (BP_phys_equal EC_neq)) [v1; v2] H
          (PR_ok (V_naked_imm 0) H)
  | P_Binary_PhysEqual_Neq_Diff :
      forall v1 v2 H,
        value_kind v1 = K_value ->
        value_kind v2 = K_value ->
        ~ is_iota H v1 ->
        ~ is_iota H v2 ->
        phys_same_word v1 v2 = false ->
        denot_mem_b (Op_binary (BP_phys_equal EC_neq)) [v1; v2] H
          (PR_ok (V_naked_imm 1) H)
  | P_Binary_PhysEqual_Neq_IotaOne :
      forall v1 v2 H,
        value_kind v1 = K_value ->
        value_kind v2 = K_value ->
        is_iota H v1 \/ is_iota H v2 ->
        denot_mem_b (Op_binary (BP_phys_equal EC_neq)) [v1; v2] H
          (PR_ok (V_naked_imm 1) H)
  | P_Binary_PhysEqual_Neq_IotaZero :
      forall v1 v2 H,
        value_kind v1 = K_value ->
        value_kind v2 = K_value ->
        is_iota H v1 \/ is_iota H v2 ->
        fold_eq H v1 v2 ->
        denot_mem_b (Op_binary (BP_phys_equal EC_neq)) [v1; v2] H
          (PR_ok (V_naked_imm 0) H)

  (** RULE P.Binary.BigarrayLoad (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
      CODE middle_end/flambda2/terms/flambda_primitive.ml#reading_from_a_bigarray
      CODE middle_end/flambda2/simplify/simplify_binary_primitive.ml#simplify_bigarray_load *)
  (* The offset argument is a TAGGED immediate (bigarray_index_kind
     = value), unlike String_or_bigstring_load. *)
  | P_Binary_BigarrayLoad :
      forall ndims bk layout a dims elems j sv v H,
        heap_get_addr H a
          = Some (HO_Bigarray bk layout dims elems) ->
        length dims = ndims ->
        0 <= j -> j < zprod dims ->
        bk <> BGK_complex32 -> bk <> BGK_complex64 ->
        nth_error elems (Z.to_nat j) = Some sv ->
        bigarray_decode bk sv = Some v ->
        denot_mem_b (Op_binary (BP_bigarray_load ndims bk layout))
          [V_ptr a; V_tagged_imm j] H (PR_ok v H)
  (* Complex clause: e[j] = (elems[2j], elems[2j+1]) at stored width
     (catalog entry 9); the read ALLOCATES the boxed complex (a
     FloatBlock, Complex.t being a float record) — hence
     Only_generative_effects Immutable. *)
  | P_Binary_BigarrayLoad_Complex :
      forall ndims bk layout a dims elems j sre sim re im l H H',
        heap_get_addr H a
          = Some (HO_Bigarray bk layout dims elems) ->
        length dims = ndims ->
        0 <= j -> j < zprod dims ->
        bk = BGK_complex32 \/ bk = BGK_complex64 ->
        nth_error elems (2 * Z.to_nat j)%nat = Some sre ->
        nth_error elems (2 * Z.to_nat j + 1)%nat = Some sim ->
        complex_half_load bk sre = Some re ->
        complex_half_load bk sim = Some im ->
        alloc (HO_FloatBlock Immutable [re; im]) H l H' ->
        denot_mem_b (Op_binary (BP_bigarray_load ndims bk layout))
          [V_ptr a; V_tagged_imm j] H
          (PR_ok (V_ptr (Addr_loc l)) H')
  (* Companion: "undef on out-of-range j". *)
  | P_Binary_BigarrayLoad_OutOfRange :
      forall ndims bk layout a dims elems j H,
        heap_get_addr H a
          = Some (HO_Bigarray bk layout dims elems) ->
        length dims = ndims ->
        j < 0 \/ zprod dims <= j ->
        denot_mem_b (Op_binary (BP_bigarray_load ndims bk layout))
          [V_ptr a; V_tagged_imm j] H PR_undef

  (** RULE P.Binary.BigarrayGetAlignment (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive
      CODE backend/cmm_helpers.ml#bigstring_get_alignment *)
  (* data_ptr (§1) is the sanctioned opaque data-pointer address;
     the conclusion is the alignment bit-mask formula. *)
  | P_Binary_BigarrayGetAlignment :
      forall n l bs j H,
        (exists k, 0 <= k /\ n = 2 ^ k) ->
        H (HK_addr (Addr_loc l)) = Some (HO_Bigstring bs) ->
        denot_mem_b (Op_binary (BP_bigarray_get_alignment n))
          [V_ptr (Addr_loc l); V_naked_imm j] H
          (PR_ok (V_naked_imm (Z.land (data_ptr l + j) (n - 1))) H)

  (** RULE P.Binary.ReadOffset (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive *)
  (* Kind-constrained nondeterministic result (coordinator ruling:
     no new Parameters for raw addressing): any value of kind
     kappa-hat may be read, and the access may also be undef (the
     model cannot see whether b + delta is valid).  These clauses
     deliberately relate both PR_ok and PR_undef for the same
     inputs.
     Width disclosure (coordinator ruling): v's numeric payload is
     a raw Z with no word-width bound; a global value
     well-formedness predicate is deferred as proof-phase debt
     (see catalog). *)
  | P_Binary_ReadOffset :
      forall kw mut b d v H,
        value_kind v = ws_kind kw ->
        denot_mem_b (Op_binary (BP_read_offset kw mut)) [b; d] H
          (PR_ok v H)
  | P_Binary_ReadOffset_Undef :
      forall kw mut b d H,
        denot_mem_b (Op_binary (BP_read_offset kw mut)) [b; d] H
          PR_undef

  (** RULE P.Ternary.ArraySet (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive
      CODE middle_end/flambda2/simplify/simplify_ternary_primitive.ml#simplify_array_set *)
  (* sk's element-kind premise reuses array_cell_load_kind through
     alk_of_ask.  H' is the doc's splice
     [v_0 ... v_{j-1}, v, v_{j+1} ...].  A vector sk over a
     different element kind is the .Vector clause below. *)
  | P_Ternary_ArraySet :
      forall ak sk a vs j v H H',
        array_cell_load_kind ak (Z.to_nat j) (alk_of_ask sk) ->
        heap_get_addr H a = Some (HO_Array ak Mutable vs) ->
        0 <= j -> j < Z.of_nat (length vs) ->
        H' = heap_upd H (HK_addr a)
               (HO_Array ak Mutable
                  (list_splice (Z.to_nat j) [v] vs)) ->
        denot_mem_b (Op_ternary (TP_array_set ak sk))
          [V_ptr a; V_tagged_imm j; v] H
          (PR_ok (V_tagged_imm 0) H')
  (* Companions: "j out of range is undef, as is storing into an
     immutable array" (the immutable clause also covers the .Vector
     form). *)
  | P_Ternary_ArraySet_OutOfRange :
      forall ak sk a vs j v H,
        heap_get_addr H a = Some (HO_Array ak Mutable vs) ->
        j < 0 \/ Z.of_nat (length vs) <= j ->
        denot_mem_b (Op_ternary (TP_array_set ak sk))
          [V_ptr a; V_tagged_imm j; v] H PR_undef
  | P_Ternary_ArraySet_Immutable :
      forall ak sk a mu vs j v H,
        heap_get_addr H a = Some (HO_Array ak mu vs) ->
        mu <> Mutable ->
        denot_mem_b (Op_ternary (TP_array_set ak sk))
          [V_ptr a; V_tagged_imm j; v] H PR_undef

  (** RULE P.Ternary.ArraySet.Vector (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#array_set0
      CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#array_vector_access_validity_condition *)
  (* The replacement run is stated via the FORWARD packing function:
     ws are "the m elements whose packed bytes are b" (no inverse
     bits function; an unrepresentable pattern relates to nothing,
     consistent with the mismatch-is-stuck policy).  Unlike the
     load, products are excluded entirely (ak <> Unboxed_product _,
     no 2/4-vec128 exception). *)
  | P_Ternary_ArraySet_Vector :
      forall ak sk lk wv we m a vs j b vv ws H H',
        lk = alk_of_ask sk ->
        vector_width_bytes lk = Some wv ->
        array_elem_byte_width ak = Some we ->
        (forall ks, ak <> AK_unboxed_product ks) ->
        m = zceil_div wv we ->
        heap_get_addr H a = Some (HO_Array ak Mutable vs) ->
        0 <= j -> j + m <= Z.of_nat (length vs) ->
        vec_value_of_bits lk b = Some vv ->
        length ws = Z.to_nat m ->
        packed_scalar_bits ak ws = Some b ->
        H' = heap_upd H (HK_addr a)
               (HO_Array ak Mutable
                  (list_splice (Z.to_nat j) ws vs)) ->
        denot_mem_b (Op_ternary (TP_array_set ak sk))
          [V_ptr a; V_tagged_imm j; vv] H
          (PR_ok (V_tagged_imm 0) H')
  (* Companion: "undef out of range" (immutable arrays are covered
     by P_Ternary_ArraySet_Immutable above). *)
  | P_Ternary_ArraySet_Vector_OutOfRange :
      forall ak sk lk wv we m a vs j v H,
        lk = alk_of_ask sk ->
        vector_width_bytes lk = Some wv ->
        array_elem_byte_width ak = Some we ->
        m = zceil_div wv we ->
        heap_get_addr H a = Some (HO_Array ak Mutable vs) ->
        j < 0 \/ Z.of_nat (length vs) < j + m ->
        denot_mem_b (Op_ternary (TP_array_set ak sk))
          [V_ptr a; V_tagged_imm j; v] H PR_undef

  (** RULE P.Ternary.BytesOrBigstringSet (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive
      CODE middle_end/flambda2/simplify/simplify_ternary_primitive.ml#simplify_bytes_or_bigstring_set *)
  (* encode_saw is "the w-byte little-endian encoding of v"; the
     payload kind follows the width as for loads. *)
  | P_Ternary_BytesOrBigstringSet :
      forall blv width w a bs j v u bs' H H',
        w = byte_width_of_string_accessor_width width ->
        bytes_like_bytes H a blv bs ->
        0 <= j -> j + w <= Z.of_nat (length bs) ->
        encode_saw width v = Some u ->
        bs' = list_splice (Z.to_nat j)
                (Z_to_le_bytes (Z.to_nat w) u) bs ->
        bytes_like_update H a blv bs' H' ->
        denot_mem_b
          (Op_ternary (TP_bytes_or_bigstring_set blv width))
          [V_ptr a; V_naked_nativeint j; v] H
          (PR_ok (V_tagged_imm 0) H')
  (* Companions: "out-of-range or (for aligned vector widths)
     misaligned is undef" (misalignment nondeterministic, as for
     loads); writing to immutable bytes is undef (the rule requires
     Bytes(Mutable, _)). *)
  | P_Ternary_BytesOrBigstringSet_OutOfRange :
      forall blv width w a bs j v H,
        w = byte_width_of_string_accessor_width width ->
        bytes_like_bytes H a blv bs ->
        j < 0 \/ Z.of_nat (length bs) < j + w ->
        denot_mem_b
          (Op_ternary (TP_bytes_or_bigstring_set blv width))
          [V_ptr a; V_naked_nativeint j; v] H PR_undef
  | P_Ternary_BytesOrBigstringSet_Misaligned :
      forall blv width a bs j v H,
        saw_aligned width = true ->
        bytes_like_bytes H a blv bs ->
        denot_mem_b
          (Op_ternary (TP_bytes_or_bigstring_set blv width))
          [V_ptr a; V_naked_nativeint j; v] H PR_undef
  | P_Ternary_BytesOrBigstringSet_Immutable :
      forall blv width a mu bs j v H,
        blv = BLV_bytes ->
        heap_get_addr H a = Some (HO_Bytes mu bs) ->
        mu <> Mutable ->
        denot_mem_b
          (Op_ternary (TP_bytes_or_bigstring_set blv width))
          [V_ptr a; V_naked_nativeint j; v] H PR_undef

  (** RULE P.Ternary.BigarraySet (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive
      CODE middle_end/flambda2/terms/flambda_primitive.ml#writing_to_a_bigarray
      CODE middle_end/flambda2/simplify/simplify_ternary_primitive.ml#simplify_bigarray_set *)
  | P_Ternary_BigarraySet :
      forall ndims bk layout a dims elems j v sv elems' H H',
        heap_get_addr H a
          = Some (HO_Bigarray bk layout dims elems) ->
        length dims = ndims ->
        0 <= j -> j < zprod dims ->
        bigarray_encode bk v = Some sv ->
        elems' = list_splice (Z.to_nat j) [sv] elems ->
        H' = heap_upd H (HK_addr a)
               (HO_Bigarray bk layout dims elems') ->
        denot_mem_b (Op_ternary (TP_bigarray_set ndims bk layout))
          [V_ptr a; V_tagged_imm j; v] H
          (PR_ok (V_tagged_imm 0) H')
  (* Complex clause: "v is a pointer to a boxed complex and both
     parts are stored" — the parts land at stored width in the
     adjacent slots 2j, 2j+1 (reading the FloatBlock's fields is of
     immutable data, no observable coeffect). *)
  | P_Ternary_BigarraySet_Complex :
      forall ndims bk layout a dims elems j a' mu re im sre sim
             elems' H H',
        heap_get_addr H a
          = Some (HO_Bigarray bk layout dims elems) ->
        length dims = ndims ->
        0 <= j -> j < zprod dims ->
        bk = BGK_complex32 \/ bk = BGK_complex64 ->
        heap_get_addr H a' = Some (HO_FloatBlock mu [re; im]) ->
        complex_half_stored bk re = Some sre ->
        complex_half_stored bk im = Some sim ->
        elems'
          = list_splice (2 * Z.to_nat j)%nat [sre; sim] elems ->
        H' = heap_upd H (HK_addr a)
               (HO_Bigarray bk layout dims elems') ->
        denot_mem_b (Op_ternary (TP_bigarray_set ndims bk layout))
          [V_ptr a; V_tagged_imm j; V_ptr a'] H
          (PR_ok (V_tagged_imm 0) H')
  (* Companion: "undef on out-of-range j". *)
  | P_Ternary_BigarraySet_OutOfRange :
      forall ndims bk layout a dims elems j v H,
        heap_get_addr H a
          = Some (HO_Bigarray bk layout dims elems) ->
        length dims = ndims ->
        j < 0 \/ zprod dims <= j ->
        denot_mem_b (Op_ternary (TP_bigarray_set ndims bk layout))
          [V_ptr a; V_tagged_imm j; v] H PR_undef

  (** RULE P.Ternary.WriteOffset (CLAIM normative)
      -- 06-primitives-memory.md
      CODE middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive *)
  (* ENCODING NOTE: raw addressing is unmodelled, so "H with the
     kappa-hat value at (b + delta) set to v" is an UNCONSTRAINED
     successor heap (nondeterministic H') — the write dual of
     ReadOffset's arbitrary result — and the access may also be
     undef.  Both mirror the rule's conjectured status. *)
  | P_Ternary_WriteOffset :
      forall wk kw mode b d v H H',
        wok_base_ok wk b ->
        value_kind v = ws_kind kw ->
        denot_mem_b (Op_ternary (TP_write_offset wk kw mode))
          [b; d; v] H (PR_ok (V_tagged_imm 0) H')
  | P_Ternary_WriteOffset_Undef :
      forall wk kw mode b d v H,
        denot_mem_b (Op_ternary (TP_write_offset wk kw mode))
          [b; d; v] H PR_undef

  (** Data clauses of P.Nullary.ControlBarriers (see the rule header
      for [effects_of_nullary] below): "These have no data
      denotation (Poll returns unit; Invalid has none — reaching it
      is undef)".  Cpu_relax is Poll's spin-hint sibling, also unit.
      Probe_is_enabled / Enter_inlined_apply / Optimised_out have no
      data denotation at all (phantom/marker primitives — reaching
      one is stuck). *)
  | P_Nullary_Poll :
      forall H,
        denot_mem_b (Op_nullary NP_poll) [] H
          (PR_ok (V_tagged_imm 0) H)
  | P_Nullary_CpuRelax :
      forall H,
        denot_mem_b (Op_nullary NP_cpu_relax) [] H
          (PR_ok (V_tagged_imm 0) H)
  | P_Nullary_Invalid :
      forall k H,
        denot_mem_b (Op_nullary (NP_invalid k)) [] H PR_undef.

(** RULE P.Nullary.ControlBarriers (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/flambda_primitive.mli#nullary_primitive
    CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_nullary_primitive *)
(* The rule's effects table, row for row; made total on
   nullary_primitive by including the Dls_get/Tls_get/Domain_index
   rows, which transcribe P.Nullary.StateAccessors' classification
   (that rule's anchor is in §3 below).  Data clauses: the last
   three constructors of denot_mem_b above. *)
Definition effects_of_nullary (np : nullary_primitive) : ece :=
  match np with
  | NP_poll | NP_cpu_relax
  | NP_probe_is_enabled _ _ | NP_enter_inlined_apply
  | NP_invalid _ =>
      {| ece_effects := Arbitrary_effects;
         ece_coeffects := Has_coeffects;
         ece_placement := Strict;
         ece_validity := Cant_move_before_any_branch |}
  | NP_optimised_out _ => EC_pure
  | NP_dls_get | NP_tls_get | NP_domain_index => EC_read
  end.

(* Anti-drift check (agreed with Curry, PrimMemoryA owner): his total
   [effects_of : eff_flags -> prim_op -> ece] cannot import this file,
   so its Op_nullary clause inlines the same values as
   [effects_of_nullary].  This lemma keeps the two copies from
   drifting; a failure here is a genuine disagreement between the two
   files' transcriptions of the ch. 06 effects tables. *)
Theorem effects_of_nullary_agrees :
  forall (fl : eff_flags) (np : nullary_primitive),
    effects_of fl (Op_nullary np) = effects_of_nullary np.
Proof. destruct np; reflexivity. Qed.

(* ================================================================== *)
(* 3. Documented anchors (descriptive rules; one ruled exception)     *)
(* ================================================================== *)

(** RULE P.Bigarray.Indexing (CLAIM descriptive)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#bigarray_indexing
    CODE middle_end/flambda2/from_lambda/lambda_to_lambda_transforms.ml#transform_primitive

    Pbigarrayref/Pbigarrayset with known kind bk and layout, indices
    i1 .. in, is lowered by from_lambda to Bigarray_load/Bigarray_set
    (n, bk, layout) applied to the flat offset
      C:       offset = (..((i1*d2 + i2)*d3 + i3)..)*dn + in
      Fortran: offset = (..(((in-1)*d{n-1} + (i{n-1}-1))*d{n-2}+..)..)
                        *d1 + (i1-1)
    with, unless the access is unsafe, one bounds check per dimension
    (0 <= ik < dk for C; 1 <= ik <= dk for Fortran) guarding the
    access.  The offset arithmetic is ordinary tagged-immediate
    Mul/Add; each dk is a Bigarray_length {dimension = k} read (issued
    once in the bounds check and again in the offset computation).
    The loaded/stored value is boxed/tagged (resp. unboxed/untagged)
    around the primitive per element_kind(bk).  Unknown-kind/layout
    accesses never become Flambda primitives (they are rewritten to
    caml_ba_get_N/caml_ba_set_N C calls --
    caml_ba_float32_get_N/_set_N for float32_t -- supported for
    1 <= N <= 3 only).  Descriptive: documents the from_lambda
    lowering, not a Flambda term rewrite. *)
Definition P_Bigarray_Indexing_documented : Prop := True.

(** RULE P.Binary.AtomicLoadField (CLAIM descriptive)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive

    p = Atomic_load_field fk, H(l) = Block(t, mu, vbar),
    i = tagged_imm j  ==>  [[p]](ptr l, i; H) = (v_j, H) with acquire
    semantics.  fk in {Any_value, Immediate}.  Classified
    (Arbitrary_effects, Has_coeffects, ...) — the strongest class — so
    it is never reordered or removed.  Descriptive: this model has no
    concurrency, so the atomic/ordering content is prose only. *)
Definition P_Binary_AtomicLoadField_documented : Prop := True.

(** RULE P.Binary.Poke (CLAIM descriptive)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/flambda_primitive.mli#binary_primitive

    p = Poke sif, addr a naked pointer, v a scalar of kind sif,
    H' = H with the sif-scalar at address a set to v  ==>
    [[p]](a, v; H) = (tagged_imm 0, H').  Unsafe raw store; returns
    unit.  (Arbitrary_effects, No_coeffects, Strict,
    Can't_move_before_any_branch) — pinned.  undef unless a is a valid
    writable address of the right size and alignment; this model does
    not track raw addresses, so treated descriptively. *)
Definition P_Binary_Poke_documented : Prop := True.

(** RULE P.Unary.Peek (CLAIM descriptive)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/flambda_primitive.mli#unary_primitive

    p = Peek sif, addr a naked pointer  ==>
    [[p]](a; H) = (sif-scalar read from address a, H).  Unsafe raw
    load, read dual of Poke.  (Arbitrary_effects, Has_coeffects, ...)
    — pinned "for the moment".  undef unless a is a valid readable
    address; descriptive as raw addressing is unmodelled. *)
Definition P_Unary_Peek_documented : Prop := True.

(** RULE P.Ternary.AtomicSetField (CLAIM descriptive)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/flambda_primitive.mli#ternary_primitive

    p = Atomic_set_field fk, H(l) = Block(t, mu, vbar),
    i = tagged_imm j, H' = H[l.j |-> v] (release semantics)  ==>
    [[p]](ptr l, i, v; H) = (tagged_imm 0, H').
    (Arbitrary_effects, Has_coeffects, ...).  Variants documented with
    it: Atomic_exchange_field returns the PREVIOUS value (v_j,
    H[l.j |-> v]); Atomic_field_int_arith op (Fetch_add/Add/Sub/And/
    Or/Xor) reads field j, writes (v_j op v), returns the old value
    for Fetch_add and unit otherwise.  Descriptive: no concurrency in
    this model. *)
Definition P_Ternary_AtomicSetField_documented : Prop := True.

(** RULE P.Quaternary.AtomicCompareAndSetField (CLAIM descriptive)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/flambda_primitive.mli#quaternary_primitive

    p = Atomic_compare_and_set_field fk, H(l) = Block(t, mu, vbar),
    i = tagged_imm j; if v_j = v_old then H' = H[l.j |-> v_new], b = 1
    else H' = H, b = 0  ==>
    [[p]](ptr l, i, v_old, v_new; H) = (tagged_imm b, H').
    Arguments are (block, field-index, expected, desired); sets field
    j to v_new iff its current value is physically equal to v_old,
    returning a success flag.  (Arbitrary_effects, Has_coeffects,
    ...).  Atomic_compare_exchange_field is the variant returning the
    previous value; it carries both the atomic's field kind and the
    field kind in use on this occasion.  Descriptive: no concurrency
    in this model. *)
Definition P_Quaternary_AtomicCompareAndSetField_documented
  : Prop := True.

(** RULE P.Nullary.StateAccessors (CLAIM descriptive)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/terms/flambda_primitive.mli#nullary_primitive
    CODE middle_end/flambda2/terms/flambda_primitive.ml#effects_and_coeffects_of_nullary_primitive

    [[Dls_get]](.; H) = (ptr (domain-local state block), H)
    [[Tls_get]](.; H) = (ptr (thread-local state block), H)
    [[Domain_index]](.; H) = (naked_imm (current domain index), H)
    All three are (No_effects, Has_coeffects, Strict,
    Can't_move_before_any_branch): they read domain/thread context, so
    they may not move across code that could change that context, but
    do not themselves change the world.  Descriptive: the runtime
    context is not part of this model's state. *)
Definition P_Nullary_StateAccessors_documented : Prop := True.

(** RULE P.Unchecked.FrontendInsertsChecks (CLAIM normative)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_array_access
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#check_bound
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#checked_access
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives_helpers.ml#bind_recs

    ENCODING NOTE (coordinator ruling): normative rule about an
    OUT-OF-SCOPE component.  Its defining clause specifies the
    from_lambda lowering of safe accesses, and the Lambda side is not
    part of this mechanization (no FromLambda.v), so the rule is
    carried as a documented anchor rather than a defining clause.

    In-scope residue (already encoded): every access primitive in
    [denot_mem_b] (and PrimMemoryA's) is the UNCHECKED access — its
    out-of-contract cases are the PR_undef clauses, and the compiler
    may assume indices are in range, because of this rule.

    Wrapper shape, for a future FromLambda mechanization:
      safe access a.(i)    lowers to
        if 0 <=_u i <_u length(a) then <unchecked access>
        else raise Invalid_argument
      unsafe access a.!(i) lowers to <unchecked access> directly.
    The bounds test is a separate Int_comp (Yielding_bool
    (Lt Unsigned)) against Array_length / String_length / a bigarray
    dimension, compiled to a Switch by bind_recs (true -> primitive,
    false -> raise, failure branch cold); check_bound emits the
    comparison and checked_access builds Checked { primitive;
    validity_conditions; failure = Index_out_of_bounds; dbg }.  Safety
    is selected per Lambda primitive (safe `...s` variants and
    non-unsafe string/bytes accesses get the wrapper; `...u` variants
    and unsafe-flagged accesses do not).  Division/modulo get the
    analogous check_zero_division wrapper (failure =
    Division_by_zero). *)
Definition P_Unchecked_FrontendInsertsChecks_documented : Prop := True.

(** RULE P.Unchecked.WideAccess (CLAIM descriptive)
    -- 06-primitives-memory.md
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#actual_max_length_for_string_like_access_as_nativeint
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#array_vector_access_validity_condition
    CODE middle_end/flambda2/from_lambda/lambda_to_flambda_primitives.ml#bigstring_alignment_validity_condition

    A safe access touching m consecutive units (m = w for a w-byte
    string-like access, m = ceil(w_v / w_e) elements for a vector
    array access) is guarded by  i <_u max(length - (m - 1), 0),
    computed branchlessly (max_with_zero: mask arithmetic).  A safe
    ALIGNED bigstring access additionally checks
    Bigarray_get_alignment(b, w_v) before running the unchecked
    primitive.  Wide safe accesses lower to the same Checked wrapper
    as P.Unchecked.FrontendInsertsChecks, with the shrunken bound
    (and, for aligned bigstring accesses, the extra alignment validity
    condition).  For string-like accesses the index is in bytes and
    length is String_length / the bigstring's Bigarray_length
    {dimension = 1}; for arrays it is in elements of the underlying
    array kind.  There is NO alignment check for aligned accesses to
    heap strings/bytes (only word-aligned by the GC); such an access
    is undef if misaligned (P.Binary.StringOrBigstringLoad).
    Descriptive: documents the from_lambda lowering. *)
Definition P_Unchecked_WideAccess_documented : Prop := True.
