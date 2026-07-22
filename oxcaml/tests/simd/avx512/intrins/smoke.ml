(* Hand-written smoke test validating the generated AVX512 intrinsic selection
   (arg reordering per mask shape, plus several newly-added CSV instruction
   rows) bit-for-bit against the real C intrinsics. The generated per-intrinsic
   harness (see gen/) covers everything; this file exercises one representative
   of each shape so failures are easy to localize. *)

open Stdlib

external m512_of_w :
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  float32x16 = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external i512_of_w :
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int32x16 = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external q512_of_w :
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64 ->
  int64x8 = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external w0 : float32x16 -> int64 = "" "vec512_w0" [@@noalloc] [@@unboxed]

external w1 : float32x16 -> int64 = "" "vec512_w1" [@@noalloc] [@@unboxed]

external w2 : float32x16 -> int64 = "" "vec512_w2" [@@noalloc] [@@unboxed]

external w3 : float32x16 -> int64 = "" "vec512_w3" [@@noalloc] [@@unboxed]

external w4 : float32x16 -> int64 = "" "vec512_w4" [@@noalloc] [@@unboxed]

external w5 : float32x16 -> int64 = "" "vec512_w5" [@@noalloc] [@@unboxed]

external w6 : float32x16 -> int64 = "" "vec512_w6" [@@noalloc] [@@unboxed]

external w7 : float32x16 -> int64 = "" "vec512_w7" [@@noalloc] [@@unboxed]

external mask_of_int64 : int64 -> mask
  = "caml_vec512_unreachable" "caml_mask_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external int64_of_mask : mask -> int64
  = "caml_vec512_unreachable" "caml_int64_of_mask"
[@@noalloc] [@@unboxed] [@@builtin]

(* Reinterpret any 512-bit vector as float32x16 for extraction. *)
external as_m512 : 'a -> float32x16 = "%identity"

let failures = ref 0

let check512 name a b =
  let a = as_m512 a and b = as_m512 b in
  let ok =
    Int64.equal (w0 a) (w0 b)
    && Int64.equal (w1 a) (w1 b)
    && Int64.equal (w2 a) (w2 b)
    && Int64.equal (w3 a) (w3 b)
    && Int64.equal (w4 a) (w4 b)
    && Int64.equal (w5 a) (w5 b)
    && Int64.equal (w6 a) (w6 b)
    && Int64.equal (w7 a) (w7 b)
  in
  if not ok
  then (
    incr failures;
    Printf.printf "MISMATCH %s\n" name)

let check_mask name a b =
  if not (Int64.equal (int64_of_mask a) (int64_of_mask b))
  then (
    incr failures;
    Printf.printf "MISMATCH %s: %Lx <> %Lx\n" name (int64_of_mask a)
      (int64_of_mask b))

(* Deterministic 512-bit inputs (raw bit patterns; some are valid floats). *)
let fa =
  m512_of_w 0x3f8000003f800000L 0x4000000040400000L 0x40800000c0a00000L
    0x41000000c1200000L 0x3f8000003f800000L 0x4000000040400000L
    0x40800000c0a00000L 0x41000000c1200000L

let fb =
  m512_of_w 0x3fc000003fc00000L 0x40a0000040c00000L 0x4110000041300000L
    0x41500000c1700000L 0x3fc000003fc00000L 0x40a0000040c00000L
    0x4110000041300000L 0x41500000c1700000L

let fc =
  m512_of_w 0x4048000040490000L 0x40c9000040d90000L 0x4149000041590000L
    0x41c9000041d90000L 0x4048000040490000L 0x40c9000040d90000L
    0x4149000041590000L 0x41c9000041d90000L

let ia =
  i512_of_w 0x0000000100000002L 0xfffffffe00000004L 0x0000000500000006L
    0x000000070000ffffL 0x1000000120000002L 0x3000000340000004L
    0x5000000560000006L 0x7000000780000008L

let ib =
  i512_of_w 0x000000030000ffffL 0x00000002ffffffffL 0x0000000a0000000bL
    0x0000000c0000000dL 0x0100000102000002L 0x0300000304000004L
    0x0500000506000006L 0x0700000708000008L

let qa =
  q512_of_w 1L (-2L) 3L (-4L) 5L (-6L) 0x7fffffffffffffffL 0x8000000000000000L

let qb = q512_of_w 10L 20L (-30L) 40L (-50L) 60L 1L (-1L)

let ic =
  i512_of_w 0x0000000000000001L 0x0000000200000003L 0x0000000400000005L
    0x0000000600000007L 0x0000000800000009L 0x0000000a0000000bL
    0x0000000c0000000dL 0x0000000e0000000fL

let mk = mask_of_int64 0xA5A5L

(* -- externals: OCaml builtins (native) + C oracles (plain extcall) -- *)
external caml_mm512_add_ps :
  (float32x16[@unboxed]) -> (float32x16[@unboxed]) -> (float32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_add_ps"
[@@noalloc] [@@builtin]

external c_add_ps :
  (float32x16[@unboxed]) -> (float32x16[@unboxed]) -> (float32x16[@unboxed])
  = "" "ctest_mm512_add_ps"
[@@noalloc]

external caml_mm512_add_epi32 :
  (int32x16[@unboxed]) -> (int32x16[@unboxed]) -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_add_epi32"
[@@noalloc] [@@builtin]

external c_add_epi32 :
  (int32x16[@unboxed]) -> (int32x16[@unboxed]) -> (int32x16[@unboxed])
  = "" "ctest_mm512_add_epi32"
[@@noalloc]

external caml_mm512_sub_epi32 :
  (int32x16[@unboxed]) -> (int32x16[@unboxed]) -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_sub_epi32"
[@@noalloc] [@@builtin]

external c_sub_epi32 :
  (int32x16[@unboxed]) -> (int32x16[@unboxed]) -> (int32x16[@unboxed])
  = "" "ctest_mm512_sub_epi32"
[@@noalloc]

external caml_mm512_abs_epi32 : (int32x16[@unboxed]) -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_abs_epi32"
[@@noalloc] [@@builtin]

external c_abs_epi32 : (int32x16[@unboxed]) -> (int32x16[@unboxed])
  = "" "ctest_mm512_abs_epi32"
[@@noalloc]

external caml_mm512_abs_epi64 : (int64x8[@unboxed]) -> (int64x8[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_abs_epi64"
[@@noalloc] [@@builtin]

external c_abs_epi64 : (int64x8[@unboxed]) -> (int64x8[@unboxed])
  = "" "ctest_mm512_abs_epi64"
[@@noalloc]

external caml_mm512_max_epi64 :
  (int64x8[@unboxed]) -> (int64x8[@unboxed]) -> (int64x8[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_max_epi64"
[@@noalloc] [@@builtin]

external c_max_epi64 :
  (int64x8[@unboxed]) -> (int64x8[@unboxed]) -> (int64x8[@unboxed])
  = "" "ctest_mm512_max_epi64"
[@@noalloc]

external caml_mm512_mask_add_ps :
  (float32x16[@unboxed]) ->
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_mask_add_ps"
[@@noalloc] [@@builtin]

external c_mask_add_ps :
  (float32x16[@unboxed]) ->
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "" "ctest_mm512_mask_add_ps"
[@@noalloc]

external caml_mm512_maskz_add_ps :
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_maskz_add_ps"
[@@noalloc] [@@builtin]

external c_maskz_add_ps :
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "" "ctest_mm512_maskz_add_ps"
[@@noalloc]

external caml_mm512_cmp_ps_mask :
  (int[@untagged]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (mask[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_cmp_ps_mask"
[@@noalloc] [@@builtin]

external c_cmp_ps_mask :
  (int[@untagged]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (mask[@unboxed]) = "" "ctest_mm512_cmp_ps_mask"
[@@noalloc]

external caml_mm512_mask_cmp_ps_mask :
  (int[@untagged]) ->
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (mask[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_mask_cmp_ps_mask"
[@@noalloc] [@@builtin]

external c_mask_cmp_ps_mask :
  (int[@untagged]) ->
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (mask[@unboxed]) = "" "ctest_mm512_mask_cmp_ps_mask"
[@@noalloc]

external caml_mm512_fmadd_ps :
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_fmadd_ps"
[@@noalloc] [@@builtin]

external c_fmadd_ps :
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "" "ctest_mm512_fmadd_ps"
[@@noalloc]

external caml_mm512_mask_fmadd_ps :
  (float32x16[@unboxed]) ->
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_mask_fmadd_ps"
[@@noalloc] [@@builtin]

external c_mask_fmadd_ps :
  (float32x16[@unboxed]) ->
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "" "ctest_mm512_mask_fmadd_ps"
[@@noalloc]

external caml_mm512_mask3_fmadd_ps :
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_mask3_fmadd_ps"
[@@noalloc] [@@builtin]

external c_mask3_fmadd_ps :
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) = "" "ctest_mm512_mask3_fmadd_ps"
[@@noalloc]

external caml_mm512_maskz_fmadd_ps :
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_maskz_fmadd_ps"
[@@noalloc] [@@builtin]

external c_maskz_fmadd_ps :
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "" "ctest_mm512_maskz_fmadd_ps"
[@@noalloc]

external caml_mm512_mask_blend_ps :
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_mask_blend_ps"
[@@noalloc] [@@builtin]

external c_mask_blend_ps :
  (mask[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "" "ctest_mm512_mask_blend_ps"
[@@noalloc]

external caml_mm512_permutex2var_epi32 :
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_permutex2var_epi32"
[@@noalloc] [@@builtin]

external c_permutex2var_epi32 :
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) = "" "ctest_mm512_permutex2var_epi32"
[@@noalloc]

external caml_mm512_mask2_permutex2var_epi32 :
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (mask[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_mask2_permutex2var_epi32"
[@@noalloc] [@@builtin]

external c_mask2_permutex2var_epi32 :
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (mask[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) = "" "ctest_mm512_mask2_permutex2var_epi32"
[@@noalloc]

external caml_mm512_slli_epi32 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_slli_epi32"
[@@noalloc] [@@builtin]

external c_slli_epi32 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> (int32x16[@unboxed])
  = "" "ctest_mm512_slli_epi32"
[@@noalloc]

external caml_mm512_shuffle_epi32 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_shuffle_epi32"
[@@noalloc] [@@builtin]

external c_shuffle_epi32 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> (int32x16[@unboxed])
  = "" "ctest_mm512_shuffle_epi32"
[@@noalloc]

external caml_mm512_ternarylogic_epi32 :
  (int[@untagged]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_ternarylogic_epi32"
[@@noalloc] [@@builtin]

external c_ternarylogic_epi32 :
  (int[@untagged]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) = "" "ctest_mm512_ternarylogic_epi32"
[@@noalloc]

external caml_mm512_cmplt_epi32_mask :
  (int32x16[@unboxed]) -> (int32x16[@unboxed]) -> (mask[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_cmplt_epi32_mask"
[@@noalloc] [@@builtin]

external c_cmplt_epi32_mask :
  (int32x16[@unboxed]) -> (int32x16[@unboxed]) -> (mask[@unboxed])
  = "" "ctest_mm512_cmplt_epi32_mask"
[@@noalloc]

external caml_mm512_add_round_ps :
  (int[@untagged]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_add_round_ps"
[@@noalloc] [@@builtin]

external c_add_round_ps :
  (int[@untagged]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "" "ctest_mm512_add_round_ps"
[@@noalloc]

external caml_kortestz_mask16_u8 :
  (mask[@unboxed]) -> (mask[@unboxed]) -> (int[@untagged])
  = "caml_vec512_unreachable" "caml_kortestz_mask16_u8"
[@@noalloc] [@@builtin]

external c_kortestz_mask16_u8 :
  (mask[@unboxed]) -> (mask[@unboxed]) -> (int[@untagged])
  = "" "ctest_kortestz_mask16_u8"
[@@noalloc]

external caml_kortestc_mask16_u8 :
  (mask[@unboxed]) -> (mask[@unboxed]) -> (int[@untagged])
  = "caml_vec512_unreachable" "caml_kortestc_mask16_u8"
[@@noalloc] [@@builtin]

external c_kortestc_mask16_u8 :
  (mask[@unboxed]) -> (mask[@unboxed]) -> (int[@untagged])
  = "" "ctest_kortestc_mask16_u8"
[@@noalloc]

external caml_ktestz_mask16_u8 :
  (mask[@unboxed]) -> (mask[@unboxed]) -> (int[@untagged])
  = "caml_vec512_unreachable" "caml_ktestz_mask16_u8"
[@@noalloc] [@@builtin]

external c_ktestz_mask16_u8 :
  (mask[@unboxed]) -> (mask[@unboxed]) -> (int[@untagged])
  = "" "ctest_ktestz_mask16_u8"
[@@noalloc]

external caml_mm512_movm_epi8 : (mask[@unboxed]) -> (int8x64[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_movm_epi8"
[@@noalloc] [@@builtin]

external c_movm_epi8 : (mask[@unboxed]) -> (int8x64[@unboxed])
  = "" "ctest_mm512_movm_epi8"
[@@noalloc]

external caml_mm512_getmant_ps :
  (int[@untagged]) ->
  (int[@untagged]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "caml_vec512_unreachable" "caml_mm512_getmant_ps"
[@@noalloc] [@@builtin]

external c_getmant_ps :
  (int[@untagged]) ->
  (int[@untagged]) ->
  (float32x16[@unboxed]) ->
  (float32x16[@unboxed]) = "" "ctest_mm512_getmant_ps"
[@@noalloc]

external caml_mm512_roundscale_ps :
  (int[@untagged]) -> (float32x16[@unboxed]) -> (float32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_roundscale_ps"
[@@noalloc] [@@builtin]

external c_roundscale_ps :
  (int[@untagged]) -> (float32x16[@unboxed]) -> (float32x16[@unboxed])
  = "" "ctest_mm512_roundscale_ps"
[@@noalloc]

let () =
  check512 "add_ps" (caml_mm512_add_ps fa fb) (c_add_ps fa fb);
  check512 "add_epi32" (caml_mm512_add_epi32 ia ib) (c_add_epi32 ia ib);
  check512 "sub_epi32" (caml_mm512_sub_epi32 ia ib) (c_sub_epi32 ia ib);
  check512 "abs_epi32" (caml_mm512_abs_epi32 ia) (c_abs_epi32 ia);
  check512 "abs_epi64" (caml_mm512_abs_epi64 qa) (c_abs_epi64 qa);
  check512 "max_epi64" (caml_mm512_max_epi64 qa qb) (c_max_epi64 qa qb);
  check512 "mask_add_ps"
    (caml_mm512_mask_add_ps fc mk fa fb)
    (c_mask_add_ps fc mk fa fb);
  check512 "maskz_add_ps"
    (caml_mm512_maskz_add_ps mk fa fb)
    (c_maskz_add_ps mk fa fb);
  check_mask "cmp_ps_mask"
    (caml_mm512_cmp_ps_mask 1 fa fb)
    (c_cmp_ps_mask 1 fa fb);
  check_mask "mask_cmp_ps_mask"
    (caml_mm512_mask_cmp_ps_mask 1 mk fa fb)
    (c_mask_cmp_ps_mask 1 mk fa fb);
  check512 "fmadd_ps" (caml_mm512_fmadd_ps fa fb fc) (c_fmadd_ps fa fb fc);
  check512 "mask_fmadd_ps"
    (caml_mm512_mask_fmadd_ps fa mk fb fc)
    (c_mask_fmadd_ps fa mk fb fc);
  check512 "mask3_fmadd_ps"
    (caml_mm512_mask3_fmadd_ps fa fb fc mk)
    (c_mask3_fmadd_ps fa fb fc mk);
  check512 "maskz_fmadd_ps"
    (caml_mm512_maskz_fmadd_ps mk fa fb fc)
    (c_maskz_fmadd_ps mk fa fb fc);
  check512 "mask_blend_ps"
    (caml_mm512_mask_blend_ps mk fa fb)
    (c_mask_blend_ps mk fa fb);
  check512 "permutex2var_epi32"
    (caml_mm512_permutex2var_epi32 ia ib ic)
    (c_permutex2var_epi32 ia ib ic);
  check512 "mask2_permutex2var_epi32"
    (caml_mm512_mask2_permutex2var_epi32 ia ib mk ic)
    (c_mask2_permutex2var_epi32 ia ib mk ic);
  check512 "slli_epi32" (caml_mm512_slli_epi32 3 ia) (c_slli_epi32 3 ia);
  check512 "shuffle_epi32"
    (caml_mm512_shuffle_epi32 0x1b ia)
    (c_shuffle_epi32 0x1b ia);
  check512 "ternarylogic_epi32"
    (caml_mm512_ternarylogic_epi32 0xca ia ib ic)
    (c_ternarylogic_epi32 0xca ia ib ic);
  check_mask "cmplt_epi32_mask"
    (caml_mm512_cmplt_epi32_mask ia ib)
    (c_cmplt_epi32_mask ia ib);
  check512 "add_round_ps"
    (caml_mm512_add_round_ps 8 fa fb)
    (c_add_round_ps 8 fa fb);
  let check_int name a b =
    if a <> b
    then begin
      incr failures;
      Printf.printf "MISMATCH %s: %d<>%d\n" name a b
    end
  in
  let m1 = mask_of_int64 0x0L and m2 = mask_of_int64 0xFFFFL in
  let m3 = mask_of_int64 0x1234L in
  check_int "kortestz_ff_00"
    (caml_kortestz_mask16_u8 m2 m1)
    (c_kortestz_mask16_u8 m2 m1);
  check_int "kortestz_00_00"
    (caml_kortestz_mask16_u8 m1 m1)
    (c_kortestz_mask16_u8 m1 m1);
  check_int "kortestc_ff_ff"
    (caml_kortestc_mask16_u8 m2 m2)
    (c_kortestc_mask16_u8 m2 m2);
  check_int "kortestc_12_34"
    (caml_kortestc_mask16_u8 m3 m3)
    (c_kortestc_mask16_u8 m3 m3);
  check_int "ktestz" (caml_ktestz_mask16_u8 m3 m2) (c_ktestz_mask16_u8 m3 m2);
  check512 "movm_epi8" (caml_mm512_movm_epi8 mk) (c_movm_epi8 mk);
  check512 "getmant_ps" (caml_mm512_getmant_ps 0 0 fa) (c_getmant_ps 0 0 fa);
  check512 "roundscale_ps"
    (caml_mm512_roundscale_ps 0x13 fa)
    (c_roundscale_ps 0x13 fa);
  if !failures = 0 then () else exit 1
