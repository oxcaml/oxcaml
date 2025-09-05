[@@@ocaml.warning "-unused-module"]

open Utils
open Utils256

type addr = nativeint#

external aligned_alloc : align:nativeint# -> size:nativeint# -> addr = "" "aligned_alloc"

module Sse = struct
  external load_aligned : addr -> (int64x2[@unboxed]) = "" "caml_sse_load_aligned"
  [@@noalloc] [@@builtin]
  external load_unaligned : addr -> (int64x2[@unboxed]) = "" "caml_sse_load_unaligned"
  [@@noalloc] [@@builtin]
  external store_aligned : addr -> (int64x2[@unboxed]) -> unit = "" "caml_sse_store_aligned"
  [@@noalloc] [@@builtin]
  external store_unaligned : addr -> (int64x2[@unboxed]) -> unit = "" "caml_sse_store_unaligned"
  [@@noalloc] [@@builtin]

  let () = 
    let mem = aligned_alloc ~align:#16n ~size:#16n in
    let x = int64x2_of_int64s 1L 2L in 
    store_unaligned mem x;
    let x' = load_unaligned mem in
    eq (int64x2_low_int64 x) (int64x2_low_int64 x')
       (int64x2_high_int64 x) (int64x2_high_int64 x');
    let x = int64x2_of_int64s 3L 4L in 
    store_aligned mem x;
    let x' = load_aligned mem in
    eq (int64x2_low_int64 x) (int64x2_low_int64 x')
       (int64x2_high_int64 x) (int64x2_high_int64 x')
end

module Avx = struct
  external load_aligned : addr -> (int64x4[@unboxed]) = "" "caml_avx_load_aligned"
  [@@noalloc] [@@builtin]
  external load_unaligned : addr -> (int64x4[@unboxed]) = "" "caml_avx_load_unaligned"
  [@@noalloc] [@@builtin]
  external store_aligned : addr -> (int64x4[@unboxed]) -> unit = "" "caml_avx_store_aligned"
  [@@noalloc] [@@builtin]
  external store_unaligned : addr -> (int64x4[@unboxed]) -> unit = "" "caml_avx_store_unaligned"
  [@@noalloc] [@@builtin]

  let () = 
    let mem = aligned_alloc ~align:#32n ~size:#32n in
    let x = int64x4_of_int64s 1L 2L 3L 4L in 
    store_unaligned mem x;
    let x' = load_unaligned mem in
    eq4 (int64x4_first_int64 x) (int64x4_first_int64 x')
        (int64x4_second_int64 x) (int64x4_second_int64 x')
        (int64x4_third_int64 x) (int64x4_third_int64 x')
        (int64x4_fourth_int64 x) (int64x4_fourth_int64 x');
    let x = int64x4_of_int64s 5L 6L 7L 8L in 
    store_aligned mem x;
    let x' = load_aligned mem in
    eq4 (int64x4_first_int64 x) (int64x4_first_int64 x')
        (int64x4_second_int64 x) (int64x4_second_int64 x')
        (int64x4_third_int64 x) (int64x4_third_int64 x')
        (int64x4_fourth_int64 x) (int64x4_fourth_int64 x');
end
