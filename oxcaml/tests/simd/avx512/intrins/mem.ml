(* Validates AVX512 memory-form intrinsics (loads/stores, plain + masked +
   expand/compress) against the real C intrinsics, over an aligned buffer. *)

open Stdlib

type void : void
type addr = nativeint#

external aligned_alloc
  :  align:nativeint#
  -> size:nativeint#
  -> addr
  = "" "vec_aligned_alloc"

external of_w
  :  int64
  -> int64
  -> int64
  -> int64
  -> int64
  -> int64
  -> int64
  -> int64
  -> int32x16
  = "" "vec512_of_int64s"
[@@noalloc] [@@unboxed]

external w
  :  (int32x16[@unboxed])
  -> (int[@untagged])
  -> (int64[@unboxed])
  = "" "vec512_wi"
[@@noalloc]

external mask_of_int64 : int64 -> mask = "caml_vec512_unreachable" "caml_mask_of_int64"
[@@noalloc] [@@unboxed] [@@builtin]

external memeq : addr -> addr -> (int[@untagged]) = "" "buf_eq64" [@@noalloc]

let failures = ref 0

let checkv name (a : int32x16) (b : int32x16) =
  let ok = ref true in
  for i = 0 to 7 do
    if not (Int64.equal (w a i) (w b i)) then ok := false
  done;
  if not !ok
  then (
    incr failures;
    Printf.printf "MISMATCH %s\n" name)
;;

let checkbuf name p q =
  if memeq p q <> 1
  then (
    incr failures;
    Printf.printf "MISMATCH %s\n" name)
;;

let va =
  of_w
    0x1111111122222222L
    0x3333333344444444L
    0x5555555566666666L
    0x7777777788888888L
    0x99999999aaaaaaaaL
    0xbbbbbbbbccccccccL
    0xddddddddeeeeeeeeL
    0x0f0f0f0ff0f0f0f0L
;;

let mk = mask_of_int64 0xA5A5L

external caml_mm512_loadu_epi32
  :  addr
  -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_loadu_epi32"
[@@noalloc] [@@builtin]

external c_loadu_epi32 : addr -> (int32x16[@unboxed]) = "" "ctest_loadu_epi32" [@@noalloc]

external caml_mm512_load_epi32
  :  addr
  -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_load_epi32"
[@@noalloc] [@@builtin]

external c_load_epi32 : addr -> (int32x16[@unboxed]) = "" "ctest_load_epi32" [@@noalloc]

external caml_mm512_mask_loadu_epi32
  :  (int32x16[@unboxed])
  -> (mask[@unboxed])
  -> addr
  -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_mask_loadu_epi32"
[@@noalloc] [@@builtin]

external c_mask_loadu_epi32
  :  (int32x16[@unboxed])
  -> (mask[@unboxed])
  -> addr
  -> (int32x16[@unboxed])
  = "" "ctest_mask_loadu_epi32"
[@@noalloc]

external caml_mm512_maskz_loadu_epi32
  :  (mask[@unboxed])
  -> addr
  -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_maskz_loadu_epi32"
[@@noalloc] [@@builtin]

external c_maskz_loadu_epi32
  :  (mask[@unboxed])
  -> addr
  -> (int32x16[@unboxed])
  = "" "ctest_maskz_loadu_epi32"
[@@noalloc]

external caml_mm512_storeu_epi32
  :  addr
  -> (int32x16[@unboxed])
  -> void
  = "caml_vec512_unreachable" "caml_mm512_storeu_epi32"
[@@noalloc] [@@builtin]

external c_storeu_epi32 : addr -> (int32x16[@unboxed]) -> void = "" "ctest_storeu_epi32"
[@@noalloc]

external caml_mm512_mask_storeu_epi32
  :  addr
  -> (mask[@unboxed])
  -> (int32x16[@unboxed])
  -> void
  = "caml_vec512_unreachable" "caml_mm512_mask_storeu_epi32"
[@@noalloc] [@@builtin]

external c_mask_storeu_epi32
  :  addr
  -> (mask[@unboxed])
  -> (int32x16[@unboxed])
  -> void
  = "" "ctest_mask_storeu_epi32"
[@@noalloc]

external caml_mm512_mask_compressstoreu_epi32
  :  addr
  -> (mask[@unboxed])
  -> (int32x16[@unboxed])
  -> void
  = "caml_vec512_unreachable" "caml_mm512_mask_compressstoreu_epi32"
[@@noalloc] [@@builtin]

external c_compressstoreu_epi32
  :  addr
  -> (mask[@unboxed])
  -> (int32x16[@unboxed])
  -> void
  = "" "ctest_compressstoreu_epi32"
[@@noalloc]

external caml_mm512_mask_expandloadu_epi32
  :  (int32x16[@unboxed])
  -> (mask[@unboxed])
  -> addr
  -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_mask_expandloadu_epi32"
[@@noalloc] [@@builtin]

external c_expandloadu_epi32
  :  (int32x16[@unboxed])
  -> (mask[@unboxed])
  -> addr
  -> (int32x16[@unboxed])
  = "" "ctest_expandloadu_epi32"
[@@noalloc]


external caml_mm512_i32gather_epi32 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> addr -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_i32gather_epi32" [@@noalloc] [@@builtin]
external c_i32gather_epi32 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> addr -> (int32x16[@unboxed])
  = "" "ctest_i32gather_epi32" [@@noalloc]

external caml_mm512_mask_i32gather_epi32 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> (mask[@unboxed]) ->
  (int32x16[@unboxed]) -> addr -> (int32x16[@unboxed])
  = "caml_vec512_unreachable" "caml_mm512_mask_i32gather_epi32"
  [@@noalloc] [@@builtin]
external c_mask_i32gather_epi32 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> (mask[@unboxed]) ->
  (int32x16[@unboxed]) -> addr -> (int32x16[@unboxed])
  = "" "ctest_mask_i32gather_epi32" [@@noalloc]

external caml_mm512_i32scatter_epi32 :
  (int[@untagged]) -> addr -> (int32x16[@unboxed]) -> (int32x16[@unboxed]) ->
  void
  = "caml_vec512_unreachable" "caml_mm512_i32scatter_epi32" [@@noalloc] [@@builtin]
external c_i32scatter_epi32 :
  (int[@untagged]) -> addr -> (int32x16[@unboxed]) -> (int32x16[@unboxed]) ->
  void
  = "" "ctest_i32scatter_epi32" [@@noalloc]

external caml_mm512_mask_i32scatter_epi32 :
  (int[@untagged]) -> addr -> (mask[@unboxed]) -> (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) -> void
  = "caml_vec512_unreachable" "caml_mm512_mask_i32scatter_epi32"
  [@@noalloc] [@@builtin]
external c_mask_i32scatter_epi32 :
  (int[@untagged]) -> addr -> (mask[@unboxed]) -> (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) -> void
  = "" "ctest_mask_i32scatter_epi32" [@@noalloc]

let () =
  let src = aligned_alloc ~align:#64n ~size:#64n in
  let d1 = aligned_alloc ~align:#64n ~size:#64n in
  let d2 = aligned_alloc ~align:#64n ~size:#64n in
  (* Seed [src] via the (assumed-correct) plain store, checked against C. *)
  let _ = caml_mm512_storeu_epi32 src va in
  let _ = c_storeu_epi32 d1 va in
  checkbuf "storeu_epi32" src d1;
  checkv "loadu_epi32" (caml_mm512_loadu_epi32 src) (c_loadu_epi32 src);
  checkv "load_epi32" (caml_mm512_load_epi32 src) (c_load_epi32 src);
  checkv
    "mask_loadu_epi32"
    (caml_mm512_mask_loadu_epi32 va mk src)
    (c_mask_loadu_epi32 va mk src);
  checkv
    "maskz_loadu_epi32"
    (caml_mm512_maskz_loadu_epi32 mk src)
    (c_maskz_loadu_epi32 mk src);
  (* Seed both destinations with the same baseline so masked-off lanes match. *)
  let vb = of_w (-1L) (-1L) (-1L) (-1L) (-1L) (-1L) (-1L) (-1L) in
  let _ = caml_mm512_storeu_epi32 d1 vb in
  let _ = caml_mm512_storeu_epi32 d2 vb in
  let _ = caml_mm512_mask_storeu_epi32 d1 mk va in
  let _ = c_mask_storeu_epi32 d2 mk va in
  checkbuf "mask_storeu_epi32" d1 d2;
  let _ = caml_mm512_storeu_epi32 d1 vb in
  let _ = caml_mm512_storeu_epi32 d2 vb in
  let _ = caml_mm512_mask_compressstoreu_epi32 d1 mk va in
  let _ = c_compressstoreu_epi32 d2 mk va in
  checkbuf "mask_compressstoreu_epi32" d1 d2;
  checkv
    "mask_expandloadu_epi32"
    (caml_mm512_mask_expandloadu_epi32 va mk src)
    (c_expandloadu_epi32 va mk src);
  (* gathers/scatters: indices 0..15 reversed over a 16-int table *)
  let table = aligned_alloc ~align:#64n ~size:#64n in
  let _ = caml_mm512_storeu_epi32 table va in
  let idxv = of_w 0x0000000e0000000fL 0x0000000c0000000dL 0x0000000a0000000bL
      0x0000000800000009L 0x0000000600000007L 0x0000000400000005L
      0x0000000200000003L 0x0000000000000001L in
  checkv "i32gather_epi32" (caml_mm512_i32gather_epi32 4 idxv table)
    (c_i32gather_epi32 4 idxv table);
  checkv "mask_i32gather_epi32"
    (caml_mm512_mask_i32gather_epi32 4 va mk idxv table)
    (c_mask_i32gather_epi32 4 va mk idxv table);
  let s1 = aligned_alloc ~align:#64n ~size:#64n in
  let s2 = aligned_alloc ~align:#64n ~size:#64n in
  let _ = caml_mm512_storeu_epi32 s1 vb in
  let _ = caml_mm512_storeu_epi32 s2 vb in
  let _ = caml_mm512_i32scatter_epi32 4 s1 idxv va in
  let _ = c_i32scatter_epi32 4 s2 idxv va in
  checkbuf "i32scatter_epi32" s1 s2;
  let _ = caml_mm512_storeu_epi32 s1 vb in
  let _ = caml_mm512_storeu_epi32 s2 vb in
  let _ = caml_mm512_mask_i32scatter_epi32 4 s1 mk idxv va in
  let _ = c_mask_i32scatter_epi32 4 s2 mk idxv va in
  checkbuf "mask_i32scatter_epi32" s1 s2;
  if !failures <> 0 then exit 1
;;
