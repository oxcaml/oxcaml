(* Validates AVX512 memory-form intrinsics (loads/stores, plain + masked +
   expand/compress) against the real C intrinsics, over an aligned buffer. *)

open Stdlib

type void : void
type addr = nativeint_u

external aligned_alloc
  :  align:nativeint_u
  -> size:nativeint_u
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

external reint : 'a -> 'b = "%identity"

external of_w256 : int64 -> int64 -> int64 -> int64 -> int32x8
  = "" "vec256_of_int64s" [@@noalloc] [@@unboxed]

external w256 : (int32x8[@unboxed]) -> (int[@untagged]) -> (int64[@unboxed])
  = "" "vec256_wi" [@@noalloc]

external of_w128 : int64 -> int64 -> int64x2
  = "" "vec128_of_int64s" [@@noalloc] [@@unboxed]

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

let checkv256 name a b =
  for i = 0 to 3 do
    if not (Int64.equal (w256 a i) (w256 b i)) then (
      incr failures;
      Printf.printf "MISMATCH %s lane %d\n" name i)
  done
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

let vb = of_w (-1L) (-1L) (-1L) (-1L) (-1L) (-1L) (-1L) (-1L)

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
external c_i32gather_epi32_s1 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> addr -> (int32x16[@unboxed])
  = "" "ctest_i32gather_epi32_s1" [@@noalloc]
external c_i32gather_epi32_s2 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> addr -> (int32x16[@unboxed])
  = "" "ctest_i32gather_epi32_s2" [@@noalloc]
external c_i32gather_epi32_s4 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> addr -> (int32x16[@unboxed])
  = "" "ctest_i32gather_epi32_s4" [@@noalloc]
external c_i32gather_epi32_s8 :
  (int[@untagged]) -> (int32x16[@unboxed]) -> addr -> (int32x16[@unboxed])
  = "" "ctest_i32gather_epi32_s8" [@@noalloc]

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
external c_i32scatter_epi32_s1 :
  (int[@untagged]) -> addr -> (int32x16[@unboxed]) -> (int32x16[@unboxed]) ->
  void
  = "" "ctest_i32scatter_epi32_s1" [@@noalloc]
external c_i32scatter_epi32_s2 :
  (int[@untagged]) -> addr -> (int32x16[@unboxed]) -> (int32x16[@unboxed]) ->
  void
  = "" "ctest_i32scatter_epi32_s2" [@@noalloc]
external c_i32scatter_epi32_s4 :
  (int[@untagged]) -> addr -> (int32x16[@unboxed]) -> (int32x16[@unboxed]) ->
  void
  = "" "ctest_i32scatter_epi32_s4" [@@noalloc]
external c_i32scatter_epi32_s8 :
  (int[@untagged]) -> addr -> (int32x16[@unboxed]) -> (int32x16[@unboxed]) ->
  void
  = "" "ctest_i32scatter_epi32_s8" [@@noalloc]

external caml_mm512_mask_i32scatter_epi32 :
  (int[@untagged]) -> addr -> (mask[@unboxed]) -> (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) -> void
  = "caml_vec512_unreachable" "caml_mm512_mask_i32scatter_epi32"
  [@@noalloc] [@@builtin]
external c_mask_i32scatter_epi32 :
  (int[@untagged]) -> addr -> (mask[@unboxed]) -> (int32x16[@unboxed]) ->
  (int32x16[@unboxed]) -> void
  = "" "ctest_mask_i32scatter_epi32" [@@noalloc]

external gather64 :
  (int[@untagged]) -> (int32x8[@unboxed]) -> (mask[@unboxed]) ->
  (int64x8[@unboxed]) -> addr -> (int32x8[@unboxed])
  = "" "caml_mm512_mask_i64gather_epi32" [@@noalloc] [@@builtin]

external c_gather64 :
  (int32x8[@unboxed]) -> (mask[@unboxed]) -> (int64x8[@unboxed]) -> addr ->
  (int32x8[@unboxed]) = "" "ctest_mask_i64gather_epi32" [@@noalloc]

external scatter64 :
  (int[@untagged]) -> addr -> (mask[@unboxed]) -> (int64x8[@unboxed]) ->
  (int32x8[@unboxed]) -> void
  = "" "caml_mm512_mask_i64scatter_epi32" [@@noalloc] [@@builtin]

external c_scatter64 :
  addr -> (mask[@unboxed]) -> (int64x8[@unboxed]) -> (int32x8[@unboxed]) -> void
  = "" "ctest_mask_i64scatter_epi32" [@@noalloc]

external narrow_store :
  addr -> (mask[@unboxed]) -> (int64x2[@unboxed]) -> void
  = "" "caml_mm_mask_cvtepi64_storeu_epi32" [@@noalloc] [@@builtin]

external c_narrow_store :
  addr -> (mask[@unboxed]) -> (int64x2[@unboxed]) -> void
  = "" "ctest_mask_cvtepi64_storeu_epi32" [@@noalloc]

let test mk =
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
    (caml_mm512_mask_loadu_epi32 vb mk src)
    (c_mask_loadu_epi32 vb mk src);
  checkv
    "maskz_loadu_epi32"
    (caml_mm512_maskz_loadu_epi32 mk src)
    (c_maskz_loadu_epi32 mk src);
  (* Seed both destinations with the same baseline so masked-off lanes match. *)
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
  (* gathers/scatters over a 16-int table; each scale is checked against its
     own C oracle (the C intrinsic requires a literal scale). Indices keep
     every access within the 64-byte table; the scale-8 indices repeat, which
     scatter resolves in lane order on both sides. *)
  let idx_vec f =
    let pack i =
      Int64.logor
        (Int64.of_int (f (2 * i)))
        (Int64.shift_left (Int64.of_int (f ((2 * i) + 1))) 32)
    in
    of_w (pack 0) (pack 1) (pack 2) (pack 3) (pack 4) (pack 5) (pack 6)
      (pack 7)
  in
  let idx1 = idx_vec (fun i -> (15 - i) * 4) in
  let idx2 = idx_vec (fun i -> (15 - i) * 2) in
  let idx4 = idx_vec (fun i -> 15 - i) in
  let idx8 = idx_vec (fun i -> 7 - (i mod 8)) in
  let table = aligned_alloc ~align:#64n ~size:#64n in
  let _ = caml_mm512_storeu_epi32 table va in
  checkv "i32gather_epi32_s1" (caml_mm512_i32gather_epi32 1 idx1 table)
    (c_i32gather_epi32_s1 1 idx1 table);
  checkv "i32gather_epi32_s2" (caml_mm512_i32gather_epi32 2 idx2 table)
    (c_i32gather_epi32_s2 2 idx2 table);
  checkv "i32gather_epi32_s4" (caml_mm512_i32gather_epi32 4 idx4 table)
    (c_i32gather_epi32_s4 4 idx4 table);
  checkv "i32gather_epi32_s8" (caml_mm512_i32gather_epi32 8 idx8 table)
    (c_i32gather_epi32_s8 8 idx8 table);
  checkv "mask_i32gather_epi32"
    (caml_mm512_mask_i32gather_epi32 4 va mk idx4 table)
    (c_mask_i32gather_epi32 4 va mk idx4 table);
  let s1 = aligned_alloc ~align:#64n ~size:#64n in
  let s2 = aligned_alloc ~align:#64n ~size:#64n in
  (* The builtins must stay fully applied (an indirect call would hit the C
     assert stub), so the scatter cases are written out. *)
  let reset () =
    let _ = caml_mm512_storeu_epi32 s1 vb in
    let _ = caml_mm512_storeu_epi32 s2 vb in
    ()
  in
  reset ();
  let _ = caml_mm512_i32scatter_epi32 1 s1 idx1 va in
  let _ = c_i32scatter_epi32_s1 1 s2 idx1 va in
  checkbuf "i32scatter_epi32_s1" s1 s2;
  reset ();
  let _ = caml_mm512_i32scatter_epi32 2 s1 idx2 va in
  let _ = c_i32scatter_epi32_s2 2 s2 idx2 va in
  checkbuf "i32scatter_epi32_s2" s1 s2;
  reset ();
  let _ = caml_mm512_i32scatter_epi32 4 s1 idx4 va in
  let _ = c_i32scatter_epi32_s4 4 s2 idx4 va in
  checkbuf "i32scatter_epi32_s4" s1 s2;
  reset ();
  let _ = caml_mm512_i32scatter_epi32 8 s1 idx8 va in
  let _ = c_i32scatter_epi32_s8 8 s2 idx8 va in
  checkbuf "i32scatter_epi32_s8" s1 s2;
  let _ = caml_mm512_storeu_epi32 s1 vb in
  let _ = caml_mm512_storeu_epi32 s2 vb in
  let _ = caml_mm512_mask_i32scatter_epi32 4 s1 mk idx4 va in
  let _ = c_mask_i32scatter_epi32 4 s2 mk idx4 va in
  checkbuf "mask_i32scatter_epi32" s1 s2;
  (* A 512-bit index with a 256-bit result exercises mixed-width VSIB operands.
     Keep the mask live across two gathers: the instructions overwrite it. *)
  let idx64 = reint (of_w 15L 13L 11L 9L 7L 5L 3L 1L) in
  let merge = of_w256 (-1L) (-2L) (-3L) (-4L) in
  let g1 = gather64 4 merge mk idx64 table in
  let g2 = gather64 4 merge mk idx64 src in
  checkv256 "mask_i64gather_epi32/first" g1 (c_gather64 merge mk idx64 table);
  checkv256 "mask_i64gather_epi32/reused_mask" g2 (c_gather64 merge mk idx64 src);
  reset ();
  let _ = scatter64 4 s1 mk idx64 merge in
  let _ = c_scatter64 s2 mk idx64 merge in
  checkbuf "mask_i64scatter_epi32" s1 s2;
  (* Narrow stores must leave all bytes outside the destination untouched. *)
  reset ();
  let small = of_w128 0x1122334455667788L 0x8877665544332211L in
  let _ = narrow_store s1 mk small in
  let _ = c_narrow_store s2 mk small in
  checkbuf "mask_cvtepi64_storeu_epi32" s1 s2
;;

let () =
  List.iter (fun bits -> test (mask_of_int64 bits)) [0L; 0xffffL; 0xa5a5L; 1L; 0x8000L];
  if !failures <> 0 then exit 1
;;
