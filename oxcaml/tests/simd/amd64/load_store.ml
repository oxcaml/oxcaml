[@@@ocaml.warning "-unused-module"]

open! Utils
open! Utils256

type void : void
type addr = nativeint#

external box_intnat : addr -> nativeint @@ portable = "%box_nativeint"
external unbox_intnat : nativeint -> addr @@ portable = "%unbox_nativeint"
let next addr = unbox_intnat (Nativeint.add (box_intnat addr) 1n)

external aligned_alloc : align:nativeint# -> size:nativeint# -> addr = "" "vec_aligned_alloc"

module Sse = struct
  external load_aligned : addr -> (int64x2[@unboxed]) = "" "caml_sse_load_aligned"
  [@@noalloc] [@@builtin]
  external load_unaligned : addr -> (int64x2[@unboxed]) = "" "caml_sse_load_unaligned"
  [@@noalloc] [@@builtin]
  external store_aligned : addr -> (int64x2[@unboxed]) -> void = "" "caml_sse_store_aligned"
  [@@noalloc] [@@builtin]
  external store_unaligned : addr -> (int64x2[@unboxed]) -> void = "" "caml_sse_store_unaligned"
  [@@noalloc] [@@builtin]

  let () = 
    let mem = aligned_alloc ~align:#16n ~size:#32n in
    let x = int64x2_of_int64s 1L 2L in 
    let _ = store_unaligned mem x in
    let x' = load_unaligned mem in
    eq (int64x2_low_int64 x) (int64x2_high_int64 x)
       (int64x2_low_int64 x') (int64x2_high_int64 x');
    let _ = store_unaligned (next mem) x in
    let x' = load_unaligned (next mem) in
    eq (int64x2_low_int64 x) (int64x2_high_int64 x)
       (int64x2_low_int64 x') (int64x2_high_int64 x');
    let x = int64x2_of_int64s 3L 4L in 
    let _ = store_aligned mem x in
    let x' = load_aligned mem in
    eq (int64x2_low_int64 x) (int64x2_high_int64 x)
       (int64x2_low_int64 x') (int64x2_high_int64 x')
end

module Avx = struct
  external load_aligned : addr -> (int64x4[@unboxed]) = "" "caml_avx_load_aligned"
  [@@noalloc] [@@builtin]
  external load_unaligned : addr -> (int64x4[@unboxed]) = "" "caml_avx_load_unaligned"
  [@@noalloc] [@@builtin]
  external store_aligned : addr -> (int64x4[@unboxed]) -> void = "" "caml_avx_store_aligned"
  [@@noalloc] [@@builtin]
  external store_unaligned : addr -> (int64x4[@unboxed]) -> void = "" "caml_avx_store_unaligned"
  [@@noalloc] [@@builtin]

  let () = 
    let mem = aligned_alloc ~align:#32n ~size:#64n in
    let x = int64x4_of_int64s 1L 2L 3L 4L in 
    let _ = store_unaligned mem x in
    let x' = load_unaligned mem in
    eq4 (int64x4_first_int64 x) (int64x4_second_int64 x)
        (int64x4_third_int64 x) (int64x4_fourth_int64 x)
        (int64x4_first_int64 x') (int64x4_second_int64 x')
        (int64x4_third_int64 x') (int64x4_fourth_int64 x');
    let _ = store_unaligned (next mem) x in
    let x' = load_unaligned (next mem) in
    eq4 (int64x4_first_int64 x) (int64x4_second_int64 x)
        (int64x4_third_int64 x) (int64x4_fourth_int64 x)
        (int64x4_first_int64 x') (int64x4_second_int64 x')
        (int64x4_third_int64 x') (int64x4_fourth_int64 x');
    let x = int64x4_of_int64s 5L 6L 7L 8L in 
    let _ = store_aligned mem x in
    let x' = load_aligned mem in
    eq4 (int64x4_first_int64 x) (int64x4_second_int64 x)
        (int64x4_third_int64 x) (int64x4_fourth_int64 x)
        (int64x4_first_int64 x') (int64x4_second_int64 x')
        (int64x4_third_int64 x') (int64x4_fourth_int64 x')
end
