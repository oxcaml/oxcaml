(* CR gyorsh: Add arm64 support for intrinsics below. This file contains amd64 intrinsics
   that don't have an equivalent arm64 neon intrinsic. They can be implemented using a
   very short sequence of arm64 instructons in a separate library [ocaml_simd_neon], but
   we may need to add compiler intrinsics. *)
module Float32x4 = struct
  external addsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float32x4_addsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float32x4_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_float32x4_dp"
    [@@noalloc] [@@builtin]
end

module Float64x2 = struct
  external addsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float64x2_addsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_sse3_float64x2_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dp :
    (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_sse41_float64x2_dp"
    [@@noalloc] [@@builtin]
end

module Int32x4 = struct
  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int32x4_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]
end

module Int64 = struct
  type t = int64

  external bit_deposit : t -> t -> t
    = "caml_vec128_unreachable" "caml_bmi2_int64_deposit_bits"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bit_extract : t -> t -> t
    = "caml_vec128_unreachable" "caml_bmi2_int64_extract_bits"
    [@@noalloc] [@@unboxed] [@@builtin]
end

module Int16x8 = struct
  external hsub : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hsub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hsub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_ssse3_int16x8_hsub_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]
end

module SSSE3_Util = struct
  external shuffle_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_ssse3_vec128_shuffle_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external align_right_bytes :
    (int[@untagged]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed]) ->
    (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_ssse3_vec128_align_right_bytes"
    [@@noalloc] [@@builtin]
end

module SSE41_Util = struct
  external blend_16 :
    (int[@untagged]) ->
    (int16x8[@unboxed]) ->
    (int16x8[@unboxed]) ->
    (int16x8[@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_16"
    [@@noalloc] [@@builtin]

  external blend_32 :
    (int[@untagged]) ->
    (int32x4[@unboxed]) ->
    (int32x4[@unboxed]) ->
    (int32x4[@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_32"
    [@@noalloc] [@@builtin]

  external blend_64 :
    (int[@untagged]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed]) ->
    (int64x2[@unboxed]) = "caml_vec128_unreachable" "caml_sse41_vec128_blend_64"
    [@@noalloc] [@@builtin]

  external blendv_8 : int8x16 -> int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external blendv_32 : int32x4 -> int32x4 -> int32x4 -> int32x4
    = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external blendv_64 : int64x2 -> int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_sse41_vec128_blendv_64"
    [@@noalloc] [@@unboxed] [@@builtin]
end
