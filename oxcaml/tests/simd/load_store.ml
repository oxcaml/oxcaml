[@@@ocaml.warning "-unused-module"]

open! Utils
open! Utils256

type addr = nativeint#

external box_intnat : addr -> nativeint @@ portable = "%box_nativeint"
external unbox_intnat : nativeint -> addr @@ portable = "%unbox_nativeint"
let next addr = unbox_intnat (Nativeint.add (box_intnat addr) 1n)

external aligned_alloc : align:nativeint# -> size:nativeint# -> addr = "" "vec_aligned_alloc"

module Vec128 = struct
  include Builtins.Vec128_load_store
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


  let () =
    let mem = aligned_alloc ~align:#16n ~size:#32n in
    let x = int64x2_of_int64s 1L 2L in
    let _ = store_aligned_uncached mem x in
    let x' = load_aligned_uncached mem in
    eq (int64x2_low_int64 x) (int64x2_high_int64 x)
       (int64x2_low_int64 x') (int64x2_high_int64 x')

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

  let () =
    let mem = aligned_alloc ~align:#16n ~size:#32n in
    let _ = store_int32_uncached mem 1l in
    let x = load_aligned mem in
    eql (int64x2_low_int64 x |> Int64.to_int32) 0l 1l 0l;
    let _ = store_int64_uncached mem 2L in
    let x = load_aligned mem in
    eq64 (int64x2_low_int64 x) 2L
end
