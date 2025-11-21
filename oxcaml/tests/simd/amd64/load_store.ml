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
  external load_aligned : addr -> (int64x2[@unboxed]) = "" "caml_sse_vec128_load_aligned"
  [@@noalloc] [@@builtin]
  external load_unaligned : addr -> (int64x2[@unboxed]) = "" "caml_sse_vec128_load_unaligned"
  [@@noalloc] [@@builtin]
  external store_aligned : addr -> (int64x2[@unboxed]) -> void = "" "caml_sse_vec128_store_aligned"
  [@@noalloc] [@@builtin]
  external store_unaligned : addr -> (int64x2[@unboxed]) -> void = "" "caml_sse_vec128_store_unaligned"
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

  external load_aligned_uncached : addr -> (int64x2[@unboxed]) = "" "caml_sse41_vec128_load_aligned_uncached"
  [@@noalloc] [@@builtin]
  external store_aligned_uncached : addr -> (int64x2[@unboxed]) -> void = "" "caml_sse_vec128_store_aligned_uncached"
  [@@noalloc] [@@builtin]

  let () =
    let mem = aligned_alloc ~align:#16n ~size:#32n in
    let x = int64x2_of_int64s 1L 2L in
    let _ = store_aligned_uncached mem x in
    let x' = load_aligned_uncached mem in
    eq (int64x2_low_int64 x) (int64x2_high_int64 x)
       (int64x2_low_int64 x') (int64x2_high_int64 x')

  external load_low64 : addr -> (int64x2[@unboxed]) = "" "caml_sse2_vec128_load_low64"
  [@@noalloc] [@@builtin]
  external load_low64_copy_high64 : (int64x2[@unboxed]) -> addr -> (int64x2[@unboxed]) = "" "caml_sse2_vec128_load_low64_copy_high64"
  [@@noalloc] [@@builtin]
  external load_high64_copy_low64 : (int64x2[@unboxed]) -> addr -> (int64x2[@unboxed]) = "" "caml_sse2_vec128_load_high64_copy_low64"
  [@@noalloc] [@@builtin]
  external load_zero_low64 : addr -> (int64x2[@unboxed]) = "" "caml_sse2_vec128_load_zero_low64"
  [@@noalloc] [@@builtin]
  external load_broadcast64 : addr -> (int64x2[@unboxed]) = "" "caml_sse3_vec128_load_broadcast64"
  [@@noalloc] [@@builtin]
  external store_low64 : addr -> (int64x2[@unboxed]) -> void = "" "caml_sse2_vec128_store_low64"
  [@@noalloc] [@@builtin]

  let () =
    let mem = aligned_alloc ~align:#16n ~size:#32n in
    let _ = store_aligned mem (int64x2_of_int64s 1L 2L) in
    let x = load_low64 mem in
    eq64 (int64x2_low_int64 x) 1L;
    let x = load_zero_low64 mem in
    let y = int64x2_of_int64s 1L 0L in
    eq_int64x2 ~result:x ~expect:y;
    let x = load_low64_copy_high64 (int64x2_of_int64s 3L 4L) mem in
    let y = int64x2_of_int64s 1L 4L in
    eq_int64x2 ~result:x ~expect:y;
    let x = load_high64_copy_low64 (int64x2_of_int64s 3L 4L) mem in
    let y = int64x2_of_int64s 3L 1L in
    eq_int64x2 ~result:x ~expect:y;
    let x = load_broadcast64 mem in
    let y = int64x2_of_int64s 1L 1L in
    eq_int64x2 ~result:x ~expect:y;
    (* current contents of mem are still [1, 2] *)
    let _ = store_low64 mem (int64x2_of_int64s 3L 4L) in
    let x = load_aligned mem in
    let y = int64x2_of_int64s 3L 2L in
    eq_int64x2 ~result:x ~expect:y

  external load_low32 : addr -> (int32x4[@unboxed]) = "" "caml_sse2_vec128_load_low32"
  [@@noalloc] [@@builtin]
  external load_zero_low32 : addr -> (int32x4[@unboxed]) = "" "caml_sse2_vec128_load_zero_low32"
  [@@noalloc] [@@builtin]
  external store_low32 : addr -> (int32x4[@unboxed]) -> void = "" "caml_sse2_vec128_store_low32"
  [@@noalloc] [@@builtin]

  external load_aligned32 : addr -> (int32x4[@unboxed]) = "" "caml_sse_vec128_load_aligned"
  [@@noalloc] [@@builtin]
  external store_aligned32 : addr -> (int32x4[@unboxed]) -> void = "" "caml_sse_vec128_store_aligned"
  [@@noalloc] [@@builtin]

  let () =
    let mem = aligned_alloc ~align:#16n ~size:#32n in
    let _ = store_aligned32 mem (Int32s.to_int32x4 1l 2l 3l 4l) in
    let x = load_low32 mem in
    eql (int32x4_low_int64 x |> Int64.to_int32) 0l 1l 0l;
    let x = load_zero_low32 mem in
    let y = Int32s.to_int32x4 1l 0l 0l 0l in
    eq_int32x4 ~result:x ~expect:y;
    let _ = store_low32 mem (Int32s.to_int32x4 5l 6l 7l 8l) in
    let x = load_aligned32 mem in
    let y = Int32s.to_int32x4 5l 2l 3l 4l in
    eq_int32x4 ~result:x ~expect:y

  external store_mask8 : (int8x16[@unboxed]) -> (int8x16[@unboxed]) -> addr -> void = "" "caml_sse2_vec128_store_mask8"
  [@@noalloc] [@@builtin]
  external load_aligned8 : addr -> (int8x16[@unboxed]) = "" "caml_sse_vec128_load_aligned"
  [@@noalloc] [@@builtin]

  let () =
    let mem = aligned_alloc ~align:#16n ~size:#32n in
    let _ = store_aligned mem (int64x2_of_int64s 0L 0L) in
    let x = Int8.to_int8x16 1 2 3 4 5 6 7 8 in
    let y = Int8.to_int8x16 0x80 0 0x80 0 0x80 0 0x80 0 in
    let _ = store_mask8 x y mem in
    let a = load_aligned8 mem in
    let b = Int8.to_int8x16 1 0 3 0 5 0 7 0 in
    eq_int8x16 ~result:a ~expect:b

  external store_int32_uncached : addr -> (int32[@unboxed]) -> void = "" "caml_sse2_int32_store_uncached"
  [@@noalloc] [@@builtin]
  external store_int64_uncached : addr -> (int64[@unboxed]) -> void = "" "caml_sse2_int64_store_uncached"
  [@@noalloc] [@@builtin]

  let () =
    let mem = aligned_alloc ~align:#16n ~size:#32n in
    let _ = store_int32_uncached mem 1l in
    let x = load_aligned mem in
    eql (int64x2_low_int64 x |> Int64.to_int32) 0l 1l 0l;
    let _ = store_int64_uncached mem 2L in
    let x = load_aligned mem in
    eq64 (int64x2_low_int64 x) 2L
end

module Avx = struct
  external load_aligned : addr -> (int64x4[@unboxed]) = "" "caml_avx_vec256_load_aligned"
  [@@noalloc] [@@builtin]
  external load_unaligned : addr -> (int64x4[@unboxed]) = "" "caml_avx_vec256_load_unaligned"
  [@@noalloc] [@@builtin]
  external store_aligned : addr -> (int64x4[@unboxed]) -> void = "" "caml_avx_vec256_store_aligned"
  [@@noalloc] [@@builtin]
  external store_unaligned : addr -> (int64x4[@unboxed]) -> void = "" "caml_avx_vec256_store_unaligned"
  [@@noalloc] [@@builtin]

  let () =
    let mem = aligned_alloc ~align:#32n ~size:#64n in
    let x = int64x4_of_int64s 1L 2L 3L 4L in
    let _ = store_unaligned mem x in
    let x' = load_unaligned mem in
    eq_int64x4 ~result:x' ~expect:x;
    let _ = store_unaligned (next mem) x in
    let x' = load_unaligned (next mem) in
    eq_int64x4 ~result:x' ~expect:x;
    let x = int64x4_of_int64s 5L 6L 7L 8L in
    let _ = store_aligned mem x in
    let x' = load_aligned mem in
    eq_int64x4 ~result:x' ~expect:x

  external load_aligned_uncached : addr -> (int64x4[@unboxed]) = "" "caml_avx_vec256_load_aligned_uncached"
  [@@noalloc] [@@builtin]
  external store_aligned_uncached : addr -> (int64x4[@unboxed]) -> void = "" "caml_avx_vec256_store_aligned_uncached"
  [@@noalloc] [@@builtin]

  let () =
    let mem = aligned_alloc ~align:#32n ~size:#64n in
    let x = int64x4_of_int64s 1L 2L 3L 4L in
    let _ = store_aligned_uncached mem x in
    let x' = load_aligned_uncached mem in
    eq_int64x4 ~result:x' ~expect:x

  external broadcast128 : addr -> (int64x4[@unboxed]) = "" "caml_avx_vec256_load_broadcast128"
  [@@noalloc] [@@builtin]
  external broadcast64 : addr -> (int64x4[@unboxed]) = "" "caml_avx_vec256_load_broadcast64"
  [@@noalloc] [@@builtin]
  external broadcast32x8 : addr -> (int32x8[@unboxed]) = "" "caml_avx_vec256_load_broadcast32"
  [@@noalloc] [@@builtin]
  external broadcast32x4 : addr -> (int32x4[@unboxed]) = "" "caml_avx_vec128_load_broadcast32"
  [@@noalloc] [@@builtin]

  external store_aligned32 : addr -> (int32x8[@unboxed]) -> void = "" "caml_avx_vec256_store_aligned"
  [@@noalloc] [@@builtin]

  let () =
    let mem = aligned_alloc ~align:#32n ~size:#64n in
    let _ = store_aligned mem (int64x4_of_int64s 1L 2L 3L 4L) in
    let x = broadcast128 mem in
    let y = int64x4_of_int64s 1L 2L 1L 2L in
    eq_int64x4 ~result:x ~expect:y;
    let x = broadcast64 mem in
    let y = int64x4_of_int64s 1L 1L 1L 1L in
    eq_int64x4 ~result:x ~expect:y;
    let _ = store_aligned32 mem (Int32s.to_int32x8 1l 2l 3l 4l 5l 6l 7l 8l) in
    let x = broadcast32x8 mem in
    let y = Int32s.to_int32x8 1l 1l 1l 1l 1l 1l 1l 1l in
    eq_int32x8 ~result:x ~expect:y;
    let x = broadcast32x4 mem in
    let y = Int32s.to_int32x4 1l 1l 1l 1l in
    eq_int32x4 ~result:x ~expect:y

  external load_mask64x2 : (int64x2[@unboxed]) -> addr -> (int64x2[@unboxed]) = "" "caml_avx_vec128_load_mask64"
  [@@noalloc] [@@builtin]
  external load_mask64x4 : (int64x4[@unboxed]) -> addr -> (int64x4[@unboxed]) = "" "caml_avx_vec256_load_mask64"
  [@@noalloc] [@@builtin]
  external load_mask32x4 : (int32x4[@unboxed]) -> addr -> (int32x4[@unboxed]) = "" "caml_avx_vec128_load_mask32"
  [@@noalloc] [@@builtin]
  external load_mask32x8 : (int32x8[@unboxed]) -> addr -> (int32x8[@unboxed]) = "" "caml_avx_vec256_load_mask32"
  [@@noalloc] [@@builtin]

  let () =
    let mem = aligned_alloc ~align:#32n ~size:#64n in
    let _ = store_aligned mem (int64x4_of_int64s 1L 2L 3L 4L) in
    let x = load_mask64x4 (int64x4_of_int64s Int64.min_int 0L Int64.min_int 0L) mem in
    let y = int64x4_of_int64s 1L 0L 3L 0L in
    eq_int64x4 ~result:x ~expect:y;
    let x = load_mask64x2 (int64x2_of_int64s Int64.min_int 0L) mem in
    let y = int64x2_of_int64s 1L 0L in
    eq (int64x2_low_int64 x) (int64x2_high_int64 x)
       (int64x2_low_int64 y) (int64x2_high_int64 y);
    let _ = store_aligned32 mem (Int32s.to_int32x8 1l 2l 3l 4l 5l 6l 7l 8l) in
    let x = load_mask32x8 (Int32s.to_int32x8 Int32.min_int 0l Int32.min_int 0l Int32.min_int 0l Int32.min_int 0l) mem in
    let y = Int32s.to_int32x8 1l 0l 3l 0l 5l 0l 7l 0l in
    eq_int32x8 ~result:x ~expect:y;
    let x = load_mask32x4 (Int32s.to_int32x4 Int32.min_int 0l Int32.min_int 0l) mem in
    let y = Int32s.to_int32x4 1l 0l 3l 0l in
    eq_int32x4 ~result:x ~expect:y


  external store_mask64x4 : addr -> (int64x4[@unboxed]) -> (int64x4[@unboxed]) -> void = "" "caml_avx_vec256_store_mask64"
  [@@noalloc] [@@builtin]
  external store_mask32x8 : addr -> (int32x8[@unboxed]) -> (int32x8[@unboxed]) -> void = "" "caml_avx_vec256_store_mask32"
  [@@noalloc] [@@builtin]

  external load_aligned32 : addr -> (int32x8[@unboxed]) = "" "caml_avx_vec256_load_aligned"
  [@@noalloc] [@@builtin]

  let () =
    let mem = aligned_alloc ~align:#32n ~size:#64n in
    let _ = store_aligned mem (int64x4_of_int64s 0L 0L 0L 0L) in
    let _ = store_mask64x4 mem
                  (int64x4_of_int64s Int64.min_int 0L Int64.min_int 0L)
                  (int64x4_of_int64s 1L 2L 3L 4L) in
    let x = load_aligned mem in
    eq4 (int64x4_first_int64 x) (int64x4_second_int64 x)
        (int64x4_third_int64 x) (int64x4_fourth_int64 x)
        1L 0L 3L 0L;
    let _ = store_aligned mem (int64x4_of_int64s 0L 0L 0L 0L) in
    let _ = store_mask32x8 mem
                (Int32s.to_int32x8 Int32.min_int 0l Int32.min_int 0l Int32.min_int 0l Int32.min_int 0l)
                (Int32s.to_int32x8 1l 2l 3l 4l 5l 6l 7l 8l) in
    let x = load_aligned32 mem in
    let y = Int32s.to_int32x8 1l 0l 3l 0l 5l 0l 7l 0l in
    eq_int32x8 ~result:x ~expect:y

  external store_mask64x2 : addr -> (int64x2[@unboxed]) -> (int64x2[@unboxed]) -> void = "" "caml_avx_vec128_store_mask64"
  [@@noalloc] [@@builtin]
  external store_mask32x4 : addr -> (int32x4[@unboxed]) -> (int32x4[@unboxed]) -> void = "" "caml_avx_vec128_store_mask32"
  [@@noalloc] [@@builtin]

  let () =
    let mem = aligned_alloc ~align:#16n ~size:#32n in
    let _ = Sse.store_aligned mem (int64x2_of_int64s 0L 0L) in
    let _ = store_mask64x2 mem
                  (int64x2_of_int64s Int64.min_int 0L)
                  (int64x2_of_int64s 1L 2L) in
    let x = Sse.load_aligned mem in
    eq (int64x2_low_int64 x) (int64x2_high_int64 x)
       1L 0L;
    let _ = Sse.store_aligned mem (int64x2_of_int64s 0L 0L) in
    let _ = store_mask32x4 mem
                (Int32s.to_int32x4 Int32.min_int 0l Int32.min_int 0l)
                (Int32s.to_int32x4 1l 2l 3l 4l) in
    let x = Sse.load_aligned32 mem in
    let y = Int32s.to_int32x4 1l 0l 3l 0l in
    eq_int32x4 ~result:x ~expect:y
end
