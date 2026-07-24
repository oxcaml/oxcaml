(* Representation.v -- ch. 17 (17-representation.md): the
   representation relation linking the abstract runtime values and
   heap objects of chs. 04/06 (Values.v) to the concrete Cmm machine
   words and byte memory of ch. 15 (Cmm.v / CmmMemory.v).

   Section map (doc section -> here):
     s1  shape of the relation    -> loc_map, word_at
     s2  values                   -> rep_val (the doc's ~v)
     s3  header encoding          -> hdr_word, header_is_local_spec
     s4  heap objects             -> rep_obj (~o)
     s5  whole heap, observation  -> rep_heap, rep_observe,
                                     gc_reloc_preserves_rep_heap
     s6  the target seam          -> prose only (no rule id): this
                                     file, like the chapter, fixes
                                     64-bit little-endian throughout

   Also discharged here: CmmMemory.v's sanctioned header_is_local
   Parameter (reviewer finding KF-009) is characterized against
   R.Header's color bits, and increment 2 states the preservation
   conjecture for the fused collection step of ch. 19 against the
   whole-heap relation.

   Conventions: rocq/CORRESPONDENCE.md.  Wave 4; owner: Milner. *)

From Stdlib Require Import ZArith Bool List String.
Import ListNotations.
Open Scope Z_scope.

From Flambda2 Require Import Base Syntax Values Cmm CmmMemory.
(* Increment 2: Opsem for the event type of R.Observe; PrimMemoryA
   for the mixed-block seam (offset_in_words / size_in_words, the
   P.MixedShape.Offset artifact). *)
From Flambda2 Require Import Opsem PrimMemoryA.

(* ================================================================== *)
(* 1. Shape of the relation (17 s1)                                   *)
(* ================================================================== *)

(* L : (Loc + Symbol) -partial-> Addr, the location map; the domain
   sum is Values' address type.  L a is the address of field 0 of the
   object at a; its header sits at L a - 8.  L is an existential
   witness wherever the whole-heap relation is asserted (R.Heap): the
   GC may relocate objects, changing L while preserving the relation
   (ch. 19, the fused collection step), which is why observations are
   compared up to the relation rather than by byte equality. *)
Definition loc_map : Type := fmap address Z.

(* The doc's "M[a] = w": the word at byte address a, little-endian,
   compared on the canonical unsigned byte image (cf. cm_read at
   Word_int/Word_val, which wraps the same eight bytes to the signed
   reading). *)
Definition word_at (M : cmm_mem) (a w : Z) : Prop :=
  mem_read_bytes (mem_bytes M) a 8
  = Some (le_bytes 8 (to_unsigned64 w)).

(* ================================================================== *)
(* 2. Values: v ~v w under L (17 s2)                                  *)
(* ================================================================== *)

(* Function-slot offsets are assigned by Slot_offsets /
   Exported_offsets once across the unit and are "treated as given
   here (as opaquely as 07 s6 treats them)" (R.Val.Clos NOTES).
   ENCODING NOTE (oracle Parameter; sanctioned as CORRESPONDENCE
   entry 47, same category as gc_reloc): the offset assignment enters
   as a Parameter keyed by the slot name.  R.Obj.Closures (increment 2)
   and ch. 18's projection images use the same Parameter, which gives
   offset-consistency across rules for free (the data_ptr precedent). *)
Parameter fun_slot_offset : function_slot -> Z.

Inductive rep_val (L : loc_map) : value -> cmm_value -> Prop :=

  (** RULE R.Val.Imm (CLAIM normative) -- 17-representation.md
      CODE backend/cmm_helpers.ml#tag_int
      CODE backend/cmm_helpers.ml#untag_int
      CODE middle_end/flambda2/kinds/flambda_kind.ml#t

      Two clauses, in doc order: tagged (2n+1, the tag bit set) and
      naked (the same integer untagged: switch scrutinee, tag).  n
      ranges over the target int range -- 63-bit for tagged_imm on
      this 64-bit target (the conclusion's range clause is the
      premise of the tagged form; 2n+1 then needs no wrap). *)
  | R_Val_Imm_tagged : forall n,
      - (2 ^ 62) <= n < 2 ^ 62 ->
      rep_val L (V_tagged_imm n) (CV_word (2 * n + 1))
  | R_Val_Imm_naked : forall n,
      rep_val L (V_naked_imm n) (CV_word n)

  (** RULE R.Val.NakedNumber (CLAIM normative) -- 17-representation.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unbox_number
      CODE backend/cmm_helpers.ml#sign_extend
      CODE backend/cmm.mli#machtype_component

      Ten clauses, in doc order.  TARGET-DEPENDENT (doc NOTES):
      int32/int8/int16 live in 64-bit registers held sign-extended
      (sign_extend is Cmm.v's, the same function the signed loads of
      CM.Mem.LoadStore use); int64 and nativeint coincide at this
      word size; floats/vectors sit in their own register classes
      (their stored chunk forms are the s4 object rules' business). *)
  | R_Val_NakedNumber_int64 : forall n,
      rep_val L (V_naked_int64 n) (CV_word n)
  | R_Val_NakedNumber_nativeint : forall n,
      rep_val L (V_naked_nativeint n) (CV_word n)
  | R_Val_NakedNumber_int32 : forall n,
      rep_val L (V_naked_int32 n) (CV_word (sign_extend 32 n))
  | R_Val_NakedNumber_int8 : forall n,
      rep_val L (V_naked_int8 n) (CV_word (sign_extend 8 n))
  | R_Val_NakedNumber_int16 : forall n,
      rep_val L (V_naked_int16 n) (CV_word (sign_extend 16 n))
  | R_Val_NakedNumber_float : forall f,
      rep_val L (V_naked_float f) (CV_flt f)
  | R_Val_NakedNumber_float32 : forall f,
      rep_val L (V_naked_float32 f) (CV_flt32 f)
  | R_Val_NakedNumber_vec128 : forall b,
      rep_val L (V_naked_vec128 b) (CV_vec128 b)
  | R_Val_NakedNumber_vec256 : forall b,
      rep_val L (V_naked_vec256 b) (CV_vec256 b)
  | R_Val_NakedNumber_vec512 : forall b,
      rep_val L (V_naked_vec512 b) (CV_vec512 b)

  (** RULE R.Val.Pointer (CLAIM normative) -- 17-representation.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#block_load
      CODE backend/cmm_helpers.ml#field_address

      ptr a is the base address L a (header at L a - 8; field i of a
      uniform block at L a + 8i is s4's business); null is word 0.
      For a symbol, R.Heap constrains L at Addr_sym to the link-time
      address.  Addr-typed derived pointers (Cadda results) are
      related to NO abstract value (doc NOTES) -- no clause. *)
  | R_Val_Pointer_ptr : forall a addr,
      L a = Some addr ->
      rep_val L (V_ptr a) (CV_word addr)
  | R_Val_Pointer_null :
      rep_val L V_null (CV_word 0)

  (** RULE R.Val.Clos (CLAIM normative) -- 17-representation.md
      CODE middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot
      CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#Layout
      CODE backend/cmm_helpers.ml#infix_field_address

      A closure value points at its function slot within the shared
      set-of-closures block: base + 8 * off(f).  The layout facts the
      rule states about off (off(f0) = 0 for the first slot; a
      sibling slot's preceding infix header) are R.Obj.Closures'
      layout obligations on the block, not re-stated on the value
      form. *)
  | R_Val_Clos : forall l f addr,
      L (Addr_loc l) = Some addr ->
      rep_val L (V_clos l f) (CV_word (addr + 8 * fun_slot_offset f)).

(* ================================================================== *)
(* 3. Header encoding (17 s3)                                         *)
(* ================================================================== *)

(* GC color field values (header bits 8-9): caml_black = caml_local =
   3 ("same bit pattern, different meaning by context", R.Header
   NOTES). *)
Definition col_white : Z := 0.
Definition col_black : Z := 3.
Definition col_local : Z := 3.

(** RULE R.Header (CLAIM normative) -- 17-representation.md
    CODE backend/cmm_helpers.ml#block_header
    CODE backend/cmm_helpers.ml#Mixed_block_support
    CODE backend/cmm_helpers.ml#caml_black

    hdr(t, sz, col, p) = (prefix_field << 56) | (sz << 10) |
    (col << 8) | t, where prefix_field = 0 for a non-mixed block and
    p+1 for a mixed block with scannable prefix p.  64-BIT-SPECIFIC
    (doc NOTES: reserved_header_bits = 8, addr_size_bits = 64).
    ENCODING NOTE (cataloged with main): the doc's fourth parameter
    conflates the scannable prefix p with its encoding -- a non-mixed
    block and a mixed one are not distinguishable from p alone (the
    object rules pass 0 for both R.Obj.Block, non-mixed top byte 0,
    and the boxed-vector case of R.Obj.Boxed, mixed top byte 1; while
    R.Obj.Array's "prefix(ak)" is already the encoded field).  So
    hdr_word takes the ENCODED prefix_field pf directly: callers pass
    0 for non-mixed and p+1 for mixed, and R.Obj.Array's prefix(ak)
    passes through unchanged.  Candidate doc clarification, raised
    with main. *)
Definition hdr_word (t sz col pf : Z) : Z :=
  Z.lor (Z.shiftl pf 56)
    (Z.lor (Z.shiftl sz 10) (Z.lor (Z.shiftl col 8) t)).

(* Discharge of CmmMemory.v's header_is_local Parameter (sanctioned;
   reviewer finding KF-009): a header word "carries caml_local"
   exactly when its color bits (8-9) read 3, the caml_local pattern
   of the header rule above.  caml_black is the same bit pattern; the
   local meaning is fixed by the CM.Alloc.Local context that premises
   header_is_local.  Stated over the canonical unsigned image,
   matching how the allocation rules store headers (to_unsigned64). *)
Theorem header_is_local_spec :
  forall w,
    header_is_local w <->
    Z.land (Z.shiftr (to_unsigned64 w) 8) 3 = 3.
Admitted.

(* ================================================================== *)
(* 4. Heap objects: o @ a ~o M under L (17 s4)                        *)
(* ================================================================== *)

(* Obj.* tag constants (R.Header NOTES: "Tag names (closure_tag,
   infix_tag, double_tag, double_array_tag, custom_tag, string_tag)
   are Obj.* constants"; runtime/caml/mlvalues.h). *)
Definition closure_tag : Z := 247.
Definition infix_tag : Z := 249.
Definition string_tag : Z := 252.
Definition double_tag : Z := 253.
Definition double_array_tag : Z := 254.
Definition custom_tag : Z := 255.

(* The never-packed vector array tags, "the CONSTANTS
   unboxed_vec128/256/512_array_tag = 6/7/8" (R.Obj.Array). *)
Definition unboxed_vec128_array_tag : Z := 6.
Definition unboxed_vec256_array_tag : Z := 7.
Definition unboxed_vec512_array_tag : Z := 8.

Definition tag_z (t : tag) : Z :=
  match t with Mk_tag n => Z.of_nat n end.

(* tag(t) of R.Obj.Lazy: Lazy_block_tag.to_tag, i.e. Tag.lazy_tag =
   246 / Tag.forward_tag = 250 (runtime/caml/mlvalues.h). *)
Definition lazy_tag_z (t : lazy_block_tag) : Z :=
  match t with Lazy_tag => 246 | Forward_tag => 250 end.

(* ENCODING NOTE: the object rules write col(mu), but the data that
   picks the color -- static data black, local blocks caml_local,
   otherwise white -- is allocation placement, which H does not
   record (Values.v's own note: region placement of an allocation is
   not recorded in the heap).  mu is not consulted; the color is
   existential over the two bit patterns of the header rule
   (caml_black = caml_local = 3).  Raised with main. *)
Definition col_ok (col : Z) : Prop :=
  col = col_white \/ col = col_black.

(* infix_header(off): "infix tag + slot offset" (R.Obj.Closures;
   cmm_helpers block_header Obj.infix_tag off, colorless). *)
Definition infix_hdr (off : Z) : Z := hdr_word infix_tag off 0 0.

(* pack_closure_info(ar, se, last) = (ar << 56) | (last << 55) |
   (se << 1) | 1 (R.Obj.Closures).  The arity byte is SIGNED (doc
   NOTES: pos_arity_in_closinfo = 56, tupled functions negative):
   Z.land ar 255 is the byte's two's-complement image, keeping the
   Z.lor composition well-defined for negative arities. *)
Definition pack_closure_info (ar se : Z) (last : bool) : Z :=
  Z.lor (Z.shiftl (Z.land ar 255) 56)
    (Z.lor (Z.shiftl (if last then 1 else 0) 55)
       (Z.lor (Z.shiftl se 1) 1)).

(* The closinfo arity: the unarized parameter count, negated for
   tupled functions ("the closinfo arity is negated for Tupled",
   20-to-cmm-soundness.md, quoted by the R.Obj.Closures NOTES). *)
Definition closinfo_arity (ai : arity_info) : Z :=
  let n := Z.of_nat (cardinal_unarized (ai_params_arity ai)) in
  if ai_is_tupled ai then - n else n.

(* 2- vs 3-word function slots, from the rule's parentheticals:
   2-word = Full_application_only (Curried arity <= 1); 3-word =
   Full_and_partial_application (Curried arity >= 2 or Tupled). *)
Definition slot_is_two_word (ai : arity_info) : bool :=
  negb (ai_is_tupled ai) &&
  (cardinal_unarized (ai_params_arity ai) <=? 1)%nat.

(* ENCODING NOTE (oracle Parameters, sanctioned by main as a batch;
   the fun_slot_offset rationale): value-slot offsets and the
   scanned flag come from Slot_offsets' Layout, which the doc treats
   as given ("Slot_offsets.Layout carries an is_scanned flag"; "Slot
   offsets from Slot_offsets / Exported_offsets, assigned once
   across the unit", R.Obj.Closures NOTES). *)
Parameter val_slot_offset : value_slot -> Z.
Parameter val_slot_is_scanned : value_slot -> bool.

(* ENCODING NOTE (oracle Parameter, sanctioned): word 0 of a 3-word
   slot, "the generic partial-application entry (caml_curryN via
   C.curry_function_sym, or C.fail_if_called_indirectly_sym when
   only_full_applications)" -- a link-time code address the doc
   names but does not define. *)
Parameter generic_entry_word : arity_info -> Z.

(* ENCODING NOTE (oracle Parameters, sanctioned): custom-operations
   table addresses ("M[a] = &caml_int32_ops" and kin, R.Obj.Boxed;
   "the bigarray custom-operations pointer", R.Obj.Bigarray) and the
   bigarray flags word ("flags (encoding bk and layout)") -- runtime
   link-time constants the doc names but does not define. *)
Parameter custom_ops_word : string -> Z.
Parameter bigarray_ops_word : Z.
Parameter ba_flags_word : bigarray_kind -> bigarray_layout -> Z.

(* ENCODING NOTE (oracle Parameter, sanctioned): the per-kind BASE
   tags of Cmm_helpers.Unboxed_or_untagged_array_tags ("a base tag
   plus the last-word padding", R.Obj.Array); the vector tags are
   the documented constants above and do not go through this. *)
Parameter unboxed_array_base_tag : array_kind -> Z.

(* Per-kind element widths w(ak) in bytes (R.Obj.Array's table);
   None for unboxed products (see the product clause below).
   ENCODING NOTE: the doc's per-kind list omits Naked_ints (untagged
   word-size ints, Unboxed_or_untagged_array_tags); they follow the
   Naked_int64/nativeint line (w = 8, prefix = 1).  Raised with
   main. *)
Definition array_elem_width (ak : array_kind) : option Z :=
  match ak with
  | AK_immediates | AK_gc_ignorable_values | AK_values => Some 8
  | AK_naked_floats => Some 8
  | AK_naked_ints | AK_naked_int64s | AK_naked_nativeints => Some 8
  | AK_naked_int32s | AK_naked_float32s => Some 4
  | AK_naked_int16s => Some 2
  | AK_naked_int8s => Some 1
  | AK_naked_vec128s => Some 16
  | AK_naked_vec256s => Some 32
  | AK_naked_vec512s => Some 64
  | AK_unboxed_product _ => None
  end.

(* The chunk an element of each kind is read back at (the rule's
   per-kind Word_val / Word_int / Double / Single / signed-integer
   widths; unaligned vector chunks, cf. the boxed-vector NOTES). *)
Definition array_elem_chunk (ak : array_kind) : option memory_chunk :=
  match ak with
  | AK_immediates | AK_gc_ignorable_values => Some Word_int
  | AK_values => Some Word_val
  | AK_naked_floats => Some Double
  | AK_naked_float32s => Some (Single Float32)
  | AK_naked_ints | AK_naked_int64s | AK_naked_nativeints =>
      Some Word_int
  | AK_naked_int32s => Some Thirtytwo_signed
  | AK_naked_int16s => Some Sixteen_signed
  | AK_naked_int8s => Some Byte_signed
  | AK_naked_vec128s => Some Onetwentyeight_unaligned
  | AK_naked_vec256s => Some Twofiftysix_unaligned
  | AK_naked_vec512s => Some Fivetwelve_unaligned
  | AK_unboxed_product _ => None
  end.

(* Elements per word of the PACKED kinds ("int32#/float32#: k = 2;
   int16#: k = 4; int8#: k = 8"); 1 elsewhere, making the padding
   below vanish for the word-size and vector kinds. *)
Definition array_packing (ak : array_kind) : Z :=
  match ak with
  | AK_naked_int32s | AK_naked_float32s => 2
  | AK_naked_int16s => 4
  | AK_naked_int8s => 8
  | _ => 1
  end.

(* "the last-word padding (k - n mod k) mod k (the number of unused
   element slots in the final word; 0 when k divides n)". *)
Definition array_pad (k n : Z) : Z := (k - n mod k) mod k.

(* tag(ak, n): 0 for the scanned/ignorable kinds, double_array_tag
   for naked floats, the constants 6/7/8 for the never-packed vector
   kinds, and base + padding for the packed / untagged kinds (the
   LENGTH-DEPENDENT tags: "the element-count PARITY lives in the
   TAG").  None for products. *)
Definition array_tag (ak : array_kind) (n : Z) : option Z :=
  match ak with
  | AK_immediates | AK_gc_ignorable_values | AK_values => Some 0
  | AK_naked_floats => Some double_array_tag
  | AK_naked_vec128s => Some unboxed_vec128_array_tag
  | AK_naked_vec256s => Some unboxed_vec256_array_tag
  | AK_naked_vec512s => Some unboxed_vec512_array_tag
  | AK_unboxed_product _ => None
  | _ => Some (unboxed_array_base_tag ak
               + array_pad (array_packing ak) n)
  end.

(* prefix(ak), already the ENCODED prefix field (doc finding #10):
   0 for the GC-scanned value/float kinds ("a plain (non-mixed)
   header"), 1 for every unboxed/untagged kind ("a MIXED header with
   scannable prefix 0"). *)
Definition array_prefix_field (ak : array_kind) : Z :=
  match ak with
  | AK_immediates | AK_gc_ignorable_values | AK_values
  | AK_naked_floats => 0
  | _ => 1
  end.

(* size_words(ak, n) = ceil(n * w / 8): n words for the word-size
   kinds, n * 2/4/8 words for vectors, packed sub-word kinds rounded
   up to the containing word -- one formula agreeing with each size
   the rule states explicitly. *)
Definition array_size_words (w n : Z) : Z := (n * w + 7) / 8.

(* chunk(sigma, j) (R.Obj.MixedBlock): Word_val for the value prefix
   (Word_int decodes identically in cm_read, covering the doc's
   "Word_val/Word_int"), then the flat-suffix element's chunk
   (memory_chunk_of_flat_suffix_element, ch. 18). *)
Definition fse_chunk (e : flat_suffix_element) : memory_chunk :=
  match e with
  | FS_naked_float => Double
  | FS_naked_float32 => Single Float32
  | FS_naked_int8 => Byte_signed
  | FS_naked_int16 => Sixteen_signed
  | FS_naked_int32 => Thirtytwo_signed
  | FS_naked_int64 | FS_naked_nativeint | FS_naked_immediate =>
      Word_int
  | FS_naked_vec128 => Onetwentyeight_unaligned
  | FS_naked_vec256 => Twofiftysix_unaligned
  | FS_naked_vec512 => Fivetwelve_unaligned
  end.

Definition mixed_field_chunk (sigma : mixed_block_shape) (j : nat)
  : option memory_chunk :=
  if (j <? value_prefix_size sigma)%nat then Some Word_val
  else option_map fse_chunk
         (nth_error (flat_suffix sigma)
            (j - value_prefix_size sigma)%nat).

(* elt_size(bk) in bytes (the taxonomy table of ch. 06, which
   R.Obj.Bigarray's elt_size names). *)
Definition ba_elt_size (bk : bigarray_kind) : Z :=
  match bk with
  | BGK_float16 => 2
  | BGK_float32 | BGK_float32_t => 4
  | BGK_float64 => 8
  | BGK_sint8 | BGK_uint8 => 1
  | BGK_sint16 | BGK_uint16 => 2
  | BGK_int32 => 4
  | BGK_int64 => 8
  | BGK_int_width_int | BGK_targetint_width_int => 8
  | BGK_complex32 => 8
  | BGK_complex64 => 16
  end.

(* Bytes per STORED element-list entry: elt_size, except the complex
   kinds, whose stored entries are the unarized HALVES (catalog 9;
   the rule: "complex elements store re then im, each elt_size/2
   bytes" -- with halves as consecutive entries, a uniform stride at
   half width IS that layout). *)
Definition ba_stored_width (bk : bigarray_kind) : Z :=
  match bk with
  | BGK_complex32 => 4
  | BGK_complex64 => 8
  | _ => ba_elt_size bk
  end.

(* Little-endian byte image of one stored entry, per the stored-value
   conventions of PrimMemoryB (bigarray_encode / complex_half_stored;
   sub-word ints stored extended, so the image is the low bytes).
   ENCODING NOTE: BGK_float16 has no bit-level carrier (PrimMemoryB
   stores the widened, pre-rounded naked_float; its f16_round is
   opaque), so the two stored bytes are only length-constrained. *)
Definition ba_entry_bytes (bk : bigarray_kind) (sv : value)
    (bs : list Z) : Prop :=
  match bk, sv with
  | BGK_float16, V_naked_float _ => List.length bs = 2%nat
  | (BGK_float32 | BGK_float32_t), V_naked_float32 f =>
      bs = le_bytes 4 (f32_to_bits f)
  | BGK_float64, V_naked_float f => bs = le_bytes 8 (f64_to_bits f)
  | (BGK_sint8 | BGK_uint8), V_naked_imm n =>
      bs = le_bytes 1 (n mod 2 ^ 8)
  | (BGK_sint16 | BGK_uint16), V_naked_imm n =>
      bs = le_bytes 2 (n mod 2 ^ 16)
  | BGK_int32, V_naked_int32 n => bs = le_bytes 4 (n mod 2 ^ 32)
  | BGK_int64, V_naked_int64 n => bs = le_bytes 8 (to_unsigned64 n)
  | BGK_int_width_int, V_naked_imm n =>
      bs = le_bytes 8 (to_unsigned64 n)
  | BGK_targetint_width_int, V_naked_nativeint n =>
      bs = le_bytes 8 (to_unsigned64 n)
  | BGK_complex32, V_naked_float32 f =>
      bs = le_bytes 4 (f32_to_bits f)
  | BGK_complex64, V_naked_float f => bs = le_bytes 8 (f64_to_bits f)
  | _, _ => False
  end.

(* The object relation H |- o @ a ~o M, one constructor group per s4
   rule.  ENCODING NOTE: the doc's H context is not consulted by any
   clause -- pointer fields relate through rep_val's L, and R.Heap
   threads the pointer consistency -- so it is omitted; SA (the
   link-time symbol addresses, a cmm_program's cp_symaddr) is the
   context R.Obj.Closures' code pointers and R.Heap's symbol clause
   need instead.  HO_Bigstring has no clause (off-heap, "the bytes
   are in malloc'd memory not under L", R.Obj.Bytes NOTES); HO_Code
   has no clause (ch. 17 prose: code becomes a Cfunction phrase, not
   heap data).  (HO_Lazy was a doc gap; R.Obj.Lazy now covers it.) *)
Inductive rep_obj (SA : fmap cmm_symbol Z) (L : loc_map) (M : cmm_mem)
  : heap_object -> Z -> Prop :=

  (** RULE R.Obj.Block (CLAIM normative) -- 17-representation.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block
      CODE backend/cmm_helpers.ml#make_alloc_generic

      Header hdr(t, n, col(mu), 0) at a - 8, then n one-word fields,
      field i a Word_val/Word_int at a + 8i representing v_i. *)
  | R_Obj_Block : forall t mu fields col a,
      col_ok col ->
      word_at M (a - 8)
        (hdr_word (tag_z t) (Z.of_nat (List.length fields)) col 0) ->
      (forall i v, nth_error fields i = Some v ->
         exists cw,
           cm_read M (a + 8 * Z.of_nat i) Word_val = Some cw /\
           rep_val L v cw) ->
      rep_obj SA L M (HO_Block t mu fields) a

  (** RULE R.Obj.Lazy (CLAIM normative) -- 17-representation.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive
      CODE middle_end/flambda2/terms/flambda_primitive.ml#Lazy_block_tag

      hdr(tag(t), 1, col, 0); one Word_val field representing v.
      Heap-only (mode Heap hard-coded in the Make_lazy lowering), so
      col is white, or black once marked -- never caml_local; col_ok
      says exactly that. *)
  | R_Obj_Lazy : forall t v col a,
      col_ok col ->
      word_at M (a - 8) (hdr_word (lazy_tag_z t) 1 col 0) ->
      (exists cw,
         cm_read M a Word_val = Some cw /\ rep_val L v cw) ->
      rep_obj SA L M (HO_Lazy t v) a

  (** RULE R.Obj.FloatBlock (CLAIM normative) -- 17-representation.md
      CODE backend/cmm_helpers.ml#float_header
      CODE backend/cmm_helpers.ml#make_float_alloc
      CODE backend/cmm_helpers.ml#floatarray_header

      hdr(double_array_tag, n, col(mu), 0); the 8 bytes at a + 8i
      decode (Double) to f_i.  (size_float = size_addr = 8, so the
      doc's n * (size_float/size_addr) is n.) *)
  | R_Obj_FloatBlock : forall mu fields col a,
      col_ok col ->
      word_at M (a - 8)
        (hdr_word double_array_tag
           (Z.of_nat (List.length fields)) col 0) ->
      (forall i f, nth_error fields i = Some f ->
         cm_read M (a + 8 * Z.of_nat i) Double = Some (CV_flt f)) ->
      rep_obj SA L M (HO_FloatBlock mu fields) a

  (** RULE R.Obj.MixedBlock (CLAIM normative) -- 17-representation.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block
      CODE backend/cmm_helpers.ml#make_mixed_alloc
      CODE middle_end/flambda2/kinds/flambda_kind.ml#Mixed_block_shape.offset_in_words

      hdr(t, size_in_words(sigma), col(mu), value_prefix_size(sigma)
      + 1) -- the + 1 per doc finding #10 (a mixed header's encoded
      prefix field is p + 1); logical field j at byte offset
      8 * offset_in_words(sigma, j) (PrimMemoryA's P.MixedShape.Offset
      artifact, "the seam ... now given concrete meaning"), read at
      chunk(sigma, j).  LITTLE-ENDIAN-ONLY (doc NOTES). *)
  | R_Obj_MixedBlock : forall t mu sigma fields col a,
      col_ok col ->
      word_at M (a - 8)
        (hdr_word (tag_z t) (size_in_words sigma) col
           (Z.of_nat (value_prefix_size sigma) + 1)) ->
      (forall j v, nth_error fields j = Some v ->
         exists ch cw,
           mixed_field_chunk sigma j = Some ch /\
           cm_read M (a + 8 * offset_in_words sigma j) ch = Some cw /\
           rep_val L v cw) ->
      rep_obj SA L M (HO_MixedBlock t mu sigma fields) a

  (** RULE R.Obj.Array (CLAIM normative) -- 17-representation.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#make_block
      CODE backend/cmm_helpers.ml#array_indexing
      CODE backend/cmm_helpers.ml#Unboxed_or_untagged_array_tags
      CODE backend/cmm_helpers.ml#unboxed_or_untagged_packed_array_length

      Two clauses.  Main clause (every kind with a defined per-kind
      row): hdr(tag(ak, n), size_words(ak, n), col(mu), prefix(ak));
      element i occupies w(ak) bytes at a + w(ak) * i, representing
      v_i at the kind's chunk.  Product clause: the doc's whole
      specification is "unboxed products -> the n*m component scalars
      packed" -- the component offsets are not given (there is no
      per-array analogue of the mixed-shape offset seam), so the
      clause constrains the header shape only (mixed, encoded prefix
      field 1, existential tag/size).  ENCODING NOTE: exact product
      packing deferred to ch. 18's array-access images; raised with
      main. *)
  | R_Obj_Array : forall ak mu elems col a w ch t,
      col_ok col ->
      array_elem_width ak = Some w ->
      array_elem_chunk ak = Some ch ->
      array_tag ak (Z.of_nat (List.length elems)) = Some t ->
      word_at M (a - 8)
        (hdr_word t
           (array_size_words w (Z.of_nat (List.length elems)))
           col (array_prefix_field ak)) ->
      (forall i v, nth_error elems i = Some v ->
         exists cw,
           cm_read M (a + w * Z.of_nat i) ch = Some cw /\
           rep_val L v cw) ->
      rep_obj SA L M (HO_Array ak mu elems) a
  | R_Obj_Array_unboxed_product : forall kinds mu elems col a t sz,
      col_ok col ->
      word_at M (a - 8) (hdr_word t sz col 1) ->
      rep_obj SA L M (HO_Array (AK_unboxed_product kinds) mu elems) a

  (** RULE R.Obj.Bytes (CLAIM normative) -- 17-representation.md
      CODE backend/cmm_helpers.ml#string_header

      hdr(string_tag, (len + 8)/8, col(mu), 0); the len bytes at
      a ... a + len - 1; the final byte of the last word holds the
      padding count.  (The doc's 8 * ceil((len+1)/8) - len - 1 is
      8 * words - len - 1 with words = (len + 8)/8, the header's own
      size field.)  LITTLE-ENDIAN baked in (doc NOTES). *)
  | R_Obj_Bytes : forall mu bytes col a len words,
      len = Z.of_nat (List.length bytes) ->
      words = (len + 8) / 8 ->
      col_ok col ->
      word_at M (a - 8) (hdr_word string_tag words col 0) ->
      mem_read_bytes (mem_bytes M) a (List.length bytes)
        = Some bytes ->
      mem_read_bytes (mem_bytes M) (a + 8 * words - 1) 1
        = Some [8 * words - len - 1] ->
      rep_obj SA L M (HO_Bytes mu bytes) a

  (** RULE R.Obj.Bigarray (CLAIM normative) -- 17-representation.md
      CODE backend/cmm_helpers.ml#bigarray_load
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#unary_primitive
      VERIFIED 14-validation/bigarray_access.md

      A custom block mirroring struct caml_ba_array after the ops
      word: data, num_dims, flags, proxy, dim[] (dimension d at field
      4 + d, 1-based, i.e. field 5 + i 0-based); elements off the
      off-heap data pointer at stride elt_size(bk) (stored entries at
      ba_stored_width, halving for complex kinds -- see
      ba_stored_width above).  The header size is left unnamed by the
      rule (existential); data is off-heap, not under L. *)
  | R_Obj_Bigarray : forall bk layout dims elems col a size data
                            proxy,
      col_ok col ->
      word_at M (a - 8) (hdr_word custom_tag size col 0) ->
      word_at M a bigarray_ops_word ->
      word_at M (a + 8) data ->
      word_at M (a + 16) (Z.of_nat (List.length dims)) ->
      word_at M (a + 24) (ba_flags_word bk layout) ->
      word_at M (a + 32) proxy ->
      (forall i d, nth_error dims i = Some d ->
         word_at M (a + 8 * (5 + Z.of_nat i)) d) ->
      (forall j sv, nth_error elems j = Some sv ->
         exists bs,
           mem_read_bytes (mem_bytes M)
             (data + ba_stored_width bk * Z.of_nat j)
             (Z.to_nat (ba_stored_width bk)) = Some bs /\
           ba_entry_bytes bk sv bs) ->
      rep_obj SA L M (HO_Bigarray bk layout dims elems) a

  (** RULE R.Obj.Boxed (CLAIM normative) -- 17-representation.md
      CODE middle_end/flambda2/to_cmm/to_cmm_primitive.ml#box_number
      CODE backend/cmm_helpers.ml#float_header
      CODE backend/cmm_helpers.ml#boxedint64_header
      CODE backend/cmm_helpers.ml#boxedvec128_header

      Eight clauses, by kappa in doc order: float (bare double_tag
      block), float32 / int32 / int64 / nativeint (custom blocks, ops
      word then payload at a + 8; int32 stored sign-extended --
      little-endian target, the doc's big-endian variant is not
      exercised), and the three vector kinds (tag-0 all-flat mixed
      blocks, encoded prefix field 1 per doc finding #10, payload at
      offset 0, no ops word, unaligned chunks). *)
  | R_Obj_Boxed_float : forall f col a,
      col_ok col ->
      word_at M (a - 8) (hdr_word double_tag 1 col 0) ->
      cm_read M a Double = Some (CV_flt f) ->
      rep_obj SA L M
        (HO_Boxed (K_naked_number NN_naked_float) (V_naked_float f)) a
  | R_Obj_Boxed_float32 : forall f col a,
      col_ok col ->
      word_at M (a - 8) (hdr_word custom_tag 2 col 0) ->
      word_at M a (custom_ops_word "caml_float32_ops"%string) ->
      cm_read M (a + 8) (Single Float32) = Some (CV_flt32 f) ->
      rep_obj SA L M
        (HO_Boxed (K_naked_number NN_naked_float32)
           (V_naked_float32 f)) a
  | R_Obj_Boxed_int32 : forall n col a,
      col_ok col ->
      word_at M (a - 8) (hdr_word custom_tag 2 col 0) ->
      word_at M a (custom_ops_word "caml_int32_ops"%string) ->
      word_at M (a + 8) (sign_extend 32 n) ->
      rep_obj SA L M
        (HO_Boxed (K_naked_number NN_naked_int32) (V_naked_int32 n)) a
  | R_Obj_Boxed_int64 : forall n col a,
      col_ok col ->
      word_at M (a - 8) (hdr_word custom_tag 2 col 0) ->
      word_at M a (custom_ops_word "caml_int64_ops"%string) ->
      word_at M (a + 8) n ->
      rep_obj SA L M
        (HO_Boxed (K_naked_number NN_naked_int64) (V_naked_int64 n)) a
  | R_Obj_Boxed_nativeint : forall n col a,
      col_ok col ->
      word_at M (a - 8) (hdr_word custom_tag 2 col 0) ->
      word_at M a (custom_ops_word "caml_nativeint_ops"%string) ->
      word_at M (a + 8) n ->
      rep_obj SA L M
        (HO_Boxed (K_naked_number NN_naked_nativeint)
           (V_naked_nativeint n)) a
  | R_Obj_Boxed_vec128 : forall b col a,
      col_ok col ->
      word_at M (a - 8) (hdr_word 0 2 col 1) ->
      cm_read M a Onetwentyeight_unaligned = Some (CV_vec128 b) ->
      rep_obj SA L M
        (HO_Boxed (K_naked_number NN_naked_vec128)
           (V_naked_vec128 b)) a
  | R_Obj_Boxed_vec256 : forall b col a,
      col_ok col ->
      word_at M (a - 8) (hdr_word 0 4 col 1) ->
      cm_read M a Twofiftysix_unaligned = Some (CV_vec256 b) ->
      rep_obj SA L M
        (HO_Boxed (K_naked_number NN_naked_vec256)
           (V_naked_vec256 b)) a
  | R_Obj_Boxed_vec512 : forall b col a,
      col_ok col ->
      word_at M (a - 8) (hdr_word 0 8 col 1) ->
      cm_read M a Fivetwelve_unaligned = Some (CV_vec512 b) ->
      rep_obj SA L M
        (HO_Boxed (K_naked_number NN_naked_vec512)
           (V_naked_vec512 b)) a

  (** RULE R.Obj.Closures (CLAIM normative) -- 17-representation.md
      CODE middle_end/flambda2/to_cmm/to_cmm_set_of_closures.ml#fill_slot
      CODE backend/cmm_helpers.ml#pack_closure_info
      CODE backend/cmm_helpers.ml#infix_header
      CODE middle_end/flambda2/simplify_shared/slot_offsets.ml#Layout

      hdr(closure_tag, total_size, col, 0); per function slot f at
      off(f) (fun_slot_offset): an infix header at word off(f) - 1
      when off(f) > 0; then the 2-word layout (code pointer, closinfo)
      or the 3-word layout (generic entry, closinfo, code pointer),
      closinfo = pack_closure_info(arity, startenv - off(f), is_last)
      at + 8 in both.  Per value slot: scanned slots at offsets >=
      startenv read back as Word_val; unscanned slots (known
      immediates / naked numbers) sit BELOW startenv.  ENCODING
      NOTES: total_size, startenv, and each slot's is_last flag are
      layout outputs of Slot_offsets the rule does not determine --
      existential (startenv shared across all slot constraints, which
      is the rule's content); the unscanned word's "per the slot's
      kind" reading is an existential chunk (venv does not record
      slot kinds); code pointers go through SA (CS_code cid), the
      ch. 17 prose ("a clos l f's code pointer ... is the address of
      that symbol"). *)
  | R_Obj_Closures : forall funs venv col a startenv total_size,
      col_ok col ->
      word_at M (a - 8) (hdr_word closure_tag total_size col 0) ->
      (forall f cid ai, funs f = Some (cid, ai) ->
         exists code_addr (is_last : bool),
           SA (CS_code cid) = Some code_addr /\
           (fun_slot_offset f > 0 ->
              word_at M (a + 8 * fun_slot_offset f - 8)
                (infix_hdr (fun_slot_offset f))) /\
           (if slot_is_two_word ai
            then
              word_at M (a + 8 * fun_slot_offset f) code_addr /\
              word_at M (a + 8 * fun_slot_offset f + 8)
                (pack_closure_info (closinfo_arity ai)
                   (startenv - fun_slot_offset f) is_last)
            else
              word_at M (a + 8 * fun_slot_offset f)
                (generic_entry_word ai) /\
              word_at M (a + 8 * fun_slot_offset f + 8)
                (pack_closure_info (closinfo_arity ai)
                   (startenv - fun_slot_offset f) is_last) /\
              word_at M (a + 8 * fun_slot_offset f + 16)
                code_addr)) ->
      (forall vs v, venv vs = Some v ->
         (if val_slot_is_scanned vs
          then val_slot_offset vs >= startenv
          else val_slot_offset vs < startenv) /\
         (if val_slot_is_scanned vs
          then exists cw,
            cm_read M (a + 8 * val_slot_offset vs) Word_val
              = Some cw /\
            rep_val L v cw
          else exists ch cw,
            cm_read M (a + 8 * val_slot_offset vs) ch = Some cw /\
            rep_val L v cw)) ->
      rep_obj SA L M (HO_Closures funs venv) a.

(* ================================================================== *)
(* 5. Whole heap and observation (17 s5)                              *)
(* ================================================================== *)

(** RULE R.Heap (CLAIM interpretive) -- 17-representation.md
    CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit

    H ~_L M: L injective on distinct live objects; every heap object
    laid out at L of its key per its s4 rule; symbols at their
    link-time addresses (SA).  The pointer-consistency clause ("if a
    field of one object is ptr l', its stored word is L(l')") is
    built into the s4 rules, whose fields go through rep_val's
    pointer clause -- the SAME L.  Its interpretive claim is about
    maintenance (the translation and the GC must preserve this;
    the conjecture below, and ch. 20), not about this definition;
    L existential and mutable at every use site.  HK_code keys are
    not heap data (ch. 17 prose). *)
Definition rep_heap (SA : fmap cmm_symbol Z) (H : heap) (L : loc_map)
    (M : cmm_mem) : Prop :=
  (forall a1 a2 addr o1 o2,
     H (HK_addr a1) = Some o1 -> H (HK_addr a2) = Some o2 ->
     L a1 = Some addr -> L a2 = Some addr -> a1 = a2) /\
  (forall a o, H (HK_addr a) = Some o ->
     exists addr, L a = Some addr /\ rep_obj SA L M o addr) /\
  (forall s o, H (HK_addr (Addr_sym s)) = Some o ->
     L (Addr_sym s) = SA (CS_sym s)).

(* The Flambda C-call events (Opsem.v's event) and the Cmm Cextern
   events (Cmm.v's cm_event) carry different value and memory types,
   so the rule's "the two effect traces are equal" cannot be typed
   as list equality.  ENCODING NOTE: it is a Forall2 over a
   Section-parameterized per-event correspondence; "equal" is
   grounded in both sides consulting the same axiomatized external
   oracle (Cmm.v's cextern_c note, "OS.Apply.CCall =~ CM.Extcall"),
   and ch. 20 instantiates ev_rel with its cumulative per-event
   correspondence (reviewer finding KF-015). *)
Section Observation.
  Variable ev_rel : event -> cm_event -> Prop.

  (** RULE R.Observe (CLAIM normative) -- 17-representation.md
      CODE middle_end/flambda2/terms/flambda_unit.mli#module_symbol
      CODE middle_end/flambda2/to_cmm/to_cmm.ml#unit

      The Flambda observation (module block value plus C-call trace,
      OS.Unit.Final) relates to the Cmm observation (byte image
      reachable from the module symbol plus Cextern trace,
      CM.Unit.Final) iff the module block is object-related at
      L(sym_mod) and the traces correspond.  The parenthetical
      "(transitively through pointer fields)" is reached by pairing
      this with rep_heap, which relates everything the module block
      points to; ch. 20's statement does exactly that. *)
  Definition rep_observe (SA : fmap cmm_symbol Z) (sym_mod : symbol)
      (H : heap) (L : loc_map) (M : cmm_mem)
      (tr : list event) (tr_c : list cm_event) : Prop :=
    (exists o addr,
       H (HK_addr (Addr_sym sym_mod)) = Some o /\
       L (Addr_sym sym_mod) = Some addr /\
       rep_obj SA L M o addr) /\
    Forall2 ev_rel tr tr_c.

End Observation.

(* phi o L, the relocated location map: moved blocks follow phi;
   unmoved blocks (phi undefined on them -- local blocks in
   particular, cf. the fused step's premise in CmmMemory.v) keep
   their address. *)
Definition reloc (phi : fmap Z Z) (L : loc_map) : loc_map :=
  fun a =>
    match L a with
    | Some addr =>
        Some (match phi addr with
              | Some addr' => addr'
              | None => addr
              end)
    | None => None
    end.

(* The ~-preservation conclusion of ch. 19's fused collection step.
   Its traceability header -- CM.Alloc.GC -- lives on CmmMemory.v's
   cmem_gc constructor; this is the conjecture that rule's STATUS
   marks, deferred here because ~ did not exist at wave 2: a
   gc_reloc transition preserves the whole-heap relation with the
   location map rewritten by phi (the rule's condition (i), "the
   abstract heap is unchanged; only L moves"), hence no
   Flambda-observable changes.  This constrains the gc_reloc oracle
   exactly as header_is_local_spec constrains header_is_local. *)
Theorem gc_reloc_preserves_rep_heap :
  forall SA phi e ce chi M TT RR e' ce' chi' M' TT' RR' H L,
    gc_reloc phi (CmCfg e ce chi M TT RR)
                 (CmCfg e' ce' chi' M' TT' RR') ->
    rep_heap SA H L M ->
    rep_heap SA H (reloc phi L) M'.
Admitted.
