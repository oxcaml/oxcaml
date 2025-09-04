(* CR gyorsh: all instructions in this file can be renamed from "caml_neon" to
   "caml_simd" because they have the corresponding implementation on amd64. If
   we do it, [builtins.ml] in target specific folders will be identical, and we
   can move them up into the parent folder. If we keep support for both
   "caml_neon" and "caml_simd" in the compiler, we should add some tests for
   both versions. *)
module Float64 = struct
  type t = float

  external max : t -> t -> t = "caml_vec128_unreachable" "caml_neon_float64_max"
    [@@noalloc] [@@builtin] [@@unboxed]

  external min : t -> t -> t = "caml_vec128_unreachable" "caml_neon_float64_min"
    [@@noalloc] [@@builtin] [@@unboxed]

  external max_match_sse : t -> t -> t
    = "caml_vec128_unreachable" "caml_simd_float64_max"
    [@@noalloc] [@@builtin] [@@unboxed]

  external min_match_sse : t -> t -> t
    = "caml_vec128_unreachable" "caml_simd_float64_min"
    [@@noalloc] [@@builtin] [@@unboxed]

  external sqrt : t -> t = "caml_vec128_unreachable" "caml_neon_float64_sqrt"
    [@@noalloc] [@@builtin] [@@unboxed]

  external round_near : (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64_round_near"
    [@@noalloc] [@@builtin]
end

module Float_cond_x86 = struct
  type t =
    | EQf
    | LTf
    | LEf
    | UNORDf
    | NEQf
    | NLTf
    | NLEf
    | ORDf

  let float_condition_of_int = function
    | 0 -> EQf
    | 1 -> LTf
    | 2 -> LEf
    | 3 -> UNORDf
    | 4 -> NEQf
    | 5 -> NLTf
    | 6 -> NLEf
    | 7 -> ORDf
    | n -> failwith (Printf.sprintf "Invalid float rounding immediate: %d" n)
end

module Int64x2 = struct
  type t = int64x2

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int64x2_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int64x2_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeqz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpeqz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpgez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgtz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpgtz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmplez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmplez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpltz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpltz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int64x2_slli"
    [@@noalloc] [@@builtin]

  external srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int64x2_srli"
    [@@noalloc] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int64[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int64x2_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int64[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int64x2_insert"
    [@@noalloc] [@@builtin]

  external bitwise_not : t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_bitwise_not"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_or : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_bitwise_or"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_and : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_bitwise_and"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_xor : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_bitwise_xor"
    [@@noalloc] [@@unboxed] [@@builtin]

  external neg : t -> t = "caml_vec128_unreachable" "caml_neon_int64x2_neg"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_float64x2 : t -> float64x2
    = "caml_vec128_unreachable" "caml_neon_cvt_int64x2_to_float64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_int32x4 : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvt_int64x2_to_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_int32x4_saturating : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvt_int64x2_to_int32x4_low_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* preserves low bits *)
  external to_int16x8_high_saturating : int32x4 -> t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvt_int64x2_to_int32x4_high_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external ushl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_ushl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sshl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int64x2_sshl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup : t -> t = "caml_vec128_unreachable" "caml_neon_int64x2_dup"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup_lane : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int64x2_dup_lane"
    [@@noalloc] [@@builtin]

  (* Shift each element of the vector by the count in the corresponding element
     of the second vector. *)
  let srl_by_vector_of_shifts : t -> t -> t =
   fun arg count -> ushl arg (neg count)

  let sra_by_vector_of_shifts : t -> t -> t =
   fun arg count -> sshl arg (neg count)

  (* Shifts with [count] at the bottom of the register. See comment in
     [Int32x4]. *)

  let sll : t -> t -> t = fun arg count -> ushl arg (dup count)

  let srl : t -> t -> t = fun arg count -> ushl arg (dup (neg count))

  let sra : t -> t -> t = fun arg count -> sshl arg (dup (neg count))
end

module Int32x4 = struct
  type t = int32x4

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeqz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpeqz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpgez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgtz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpgtz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmplez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmplez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpltz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpltz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_slli"
    [@@noalloc] [@@builtin]

  external srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_srli"
    [@@noalloc] [@@builtin]

  external srai : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_srai"
    [@@noalloc] [@@builtin]

  external cvt_f32 : t -> float32x4
    = "caml_vec128_unreachable" "caml_neon_cvt_int32x4_to_float32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external abs : t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_abs"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_max_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_min_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_cvtsx_int32x4_to_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  let cvt_f64 : t -> float64x2 =
   fun t -> t |> cvtsx_i64 |> Int64x2.cvt_float64x2

  external cvtzx_i64 : t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_cvtzx_int32x4_to_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* zeros upper bits *)
  external to_int16x8_low : t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvt_int32x4_to_int16x8_low"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* preserves low bits *)
  external to_int16x8_high : int16x8 -> t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvt_int32x4_to_int16x8_high"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* zeros upper bits *)
  external to_int16x8_low_saturating : t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvt_int32x4_to_int16x8_low_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* preserves low bits *)
  external to_int16x8_high_saturating : int16x8 -> t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvt_int32x4_to_int16x8_high_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  let cvt_si16 : t -> t -> int16x8 =
   fun low high ->
    let low = to_int16x8_low_saturating low in
    to_int16x8_high_saturating low high

  (* zeros upper bits *)
  external to_int16x8_low_saturating_unsigned : t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvt_int32x4_to_int16x8_low_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* preserves low bits *)
  external to_int16x8_high_saturating_unsigned : int16x8 -> t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvt_int32x4_to_int16x8_high_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  let cvt_su16 : t -> t -> int16x8 =
   fun low high ->
    let low = to_int16x8_low_saturating_unsigned low in
    to_int16x8_high_saturating_unsigned low high

  (* The meaning of PACKUSDW on amd64 is not exactly the same as UQXTN on arm64:
     amd64 treats the input as signed int32, whereas arm64 treats the input as
     unsigned int32, therefore negative input results in 0H on amd64 and FFFFH
     on arm64. *)

  external mul_low : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_mul_low"
    [@@noalloc] [@@unboxed] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int32[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_insert"
    [@@noalloc] [@@builtin]

  external bitwise_not : t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_not"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_or : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_or"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_and : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_and"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_xor : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_bitwise_xor"
    [@@noalloc] [@@unboxed] [@@builtin]

  external neg : t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_neg"
    [@@noalloc] [@@unboxed] [@@builtin]

  external ushl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_ushl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sshl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int32x4_sshl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup : t -> t = "caml_vec128_unreachable" "caml_neon_int32x4_dup"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup_lane : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int32x4_dup_lane"
    [@@noalloc] [@@builtin]

  (* Shifts with [count] in a register.

     The function below match the semantics of amd64 shift builtins for
     instructions that operate on a register. The second argument [count] is an
     unsigned 128-bit integer (reinterpreting the declared type [t]). All lanes
     of the first argument [arg] are shifted by the same [count].

     The corresponding arm64 instructions expects a vector of signed [count]
     values, one per lane. If [count] is large then the bit-width of a lane, the
     shift is 0.

     There seems to be no arm64 instruction for right shift (logic or
     arithmetic) that take [count] in a register (not an immediate). The
     operation can be expressed using a negative count for the corresponding
     shift left instructions USHL and SSHL. *)

  let sll : t -> t -> t = fun arg count -> ushl arg (dup count)

  let srl : t -> t -> t = fun arg count -> ushl arg (neg (dup count))

  let sra : t -> t -> t = fun arg count -> sshl arg (neg (dup count))
end

module Float32x4 = struct
  type t = float32x4

  external cmeq : (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_cmeq"
    [@@noalloc] [@@builtin]

  external cmge : (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_cmge"
    [@@noalloc] [@@builtin]

  external cmgt : (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_cmgt"
    [@@noalloc] [@@builtin]

  external cmle : (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_cmle"
    [@@noalloc] [@@builtin]

  external cmlt : (t[@unboxed]) -> (t[@unboxed]) -> (int32x4[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_cmlt"
    [@@noalloc] [@@builtin]

  let is_nan t = Int32x4.bitwise_not (cmeq t t)

  let cmp n t1 t2 =
    match Float_cond_x86.float_condition_of_int n with
    | EQf -> cmeq t1 t2
    | LTf -> cmlt t1 t2
    | LEf -> cmle t1 t2
    | NEQf -> Int32x4.bitwise_not (cmeq t1 t2)
    | NLTf -> Int32x4.bitwise_not (cmlt t1 t2)
    | NLEf -> Int32x4.bitwise_not (cmle t1 t2)
    (* CR gyorsh: this is not efficient but gives us more testing coverage. *)
    | UNORDf -> Int32x4.bitwise_or (is_nan t1) (is_nan t2)
    | ORDf -> Int32x4.bitwise_not (Int32x4.bitwise_or (is_nan t1) (is_nan t2))

  external add : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_mul"
    [@@noalloc] [@@unboxed] [@@builtin]

  external div : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_div"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external rcp : t -> t = "caml_vec128_unreachable" "caml_neon_float32x4_rcp"
    [@@noalloc] [@@unboxed] [@@builtin]

  external rsqrt : t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_rsqrt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sqrt : t -> t = "caml_vec128_unreachable" "caml_neon_float32x4_sqrt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_int32x4 : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvt_float32x4_to_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtt_int32x4 : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvtt_float32x4_to_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_float64x2 : t -> float64x2
    = "caml_vec128_unreachable" "caml_neon_cvt_float32x2_to_float64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float32x4_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external round_near : (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float32x4_round_near"
    [@@noalloc] [@@builtin]
end

module Float64x2 = struct
  type t = float64x2

  (* Math *)

  external cmeq : (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_cmeq"
    [@@noalloc] [@@builtin]

  external cmge : (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_cmge"
    [@@noalloc] [@@builtin]

  external cmgt : (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_cmgt"
    [@@noalloc] [@@builtin]

  external cmle : (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_cmle"
    [@@noalloc] [@@builtin]

  external cmlt : (t[@unboxed]) -> (t[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_cmlt"
    [@@noalloc] [@@builtin]

  let is_nan t = Int64x2.bitwise_not (cmeq t t)

  let cmp n t1 t2 =
    match Float_cond_x86.float_condition_of_int n with
    | EQf -> cmeq t1 t2
    | LTf -> cmlt t1 t2
    | LEf -> cmle t1 t2
    | NEQf -> Int64x2.bitwise_not (cmeq t1 t2)
    | NLTf -> Int64x2.bitwise_not (cmlt t1 t2)
    | NLEf -> Int64x2.bitwise_not (cmle t1 t2)
    (* CR gyorsh: this is not efficient but gives us more testing coverage. *)
    | UNORDf -> Int64x2.bitwise_or (is_nan t1) (is_nan t2)
    | ORDf -> Int64x2.bitwise_not (Int64x2.bitwise_or (is_nan t1) (is_nan t2))

  external add : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external mul : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_mul"
    [@@noalloc] [@@unboxed] [@@builtin]

  external div : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_div"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_float64x2_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sqrt : t -> t = "caml_vec128_unreachable" "caml_neon_float64x2_sqrt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_int64x2 : t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_cvt_float64x2_to_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtt_int64x2 : t -> int64x2
    = "caml_vec128_unreachable" "caml_neon_cvtt_float64x2_to_int64x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvt_float32x4 : t -> float32x4
    = "caml_vec128_unreachable" "caml_neon_cvt_float64x2_to_float32x2"
    [@@noalloc] [@@unboxed] [@@builtin]

  external round_near : (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_round_near"
    [@@noalloc] [@@builtin]

  external round_current : (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_float64x2_round_current"
    [@@noalloc] [@@builtin]

  let cvt_int32x4 : t -> int32x4 =
   (* Use saturating narrowing conversion here to match SSE intrinsics and C
      stubs behavior. *)
   fun t -> t |> round_current |> cvt_int64x2 |> Int64x2.cvt_int32x4_saturating

  let cvtt_int32x4 : t -> int32x4 =
   (* Use saturating narrowing conversion here to match SSE intrinsics and C
      stubs behavior. *)
   fun t -> t |> cvtt_int64x2 |> Int64x2.cvt_int32x4_saturating
end

module Int16x8 = struct
  type t = int16x8

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_add_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_add_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_sub_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_sub_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external maxu : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_max_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external minu : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_min_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeqz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_cmpeqz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_cmpgez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgtz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_cmpgtz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmplez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_cmplez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpltz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_cmpltz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* zeros upper bits *)
  external to_int8x16_low : t -> int8x16
    = "caml_vec128_unreachable" "caml_neon_cvt_int16x8_to_int8x16_low"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* preserves low bits *)
  external to_int8x16_high : int8x16 -> t -> int8x16
    = "caml_vec128_unreachable" "caml_neon_cvt_int16x8_to_int8x16_high"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* zeros upper bits *)
  external to_int8x16_low_saturating : t -> int8x16
    = "caml_vec128_unreachable" "caml_neon_cvt_int16x8_to_int8x16_low_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* preserves low bits *)
  external to_int8x16_high_saturating : int8x16 -> t -> int8x16
    = "caml_vec128_unreachable" "caml_neon_cvt_int16x8_to_int8x16_high_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  let cvt_si8 : t -> t -> int8x16 =
   fun low high ->
    let low = to_int8x16_low_saturating low in
    to_int8x16_high_saturating low high

  (* zeros upper bits *)
  external to_int8x16_low_saturating_unsigned : t -> int8x16
    = "caml_vec128_unreachable" "caml_neon_cvt_int16x8_to_int8x16_low_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* preserves low bits *)
  external to_int8x16_high_saturating_unsigned : int8x16 -> t -> int8x16
    = "caml_vec128_unreachable" "caml_neon_cvt_int16x8_to_int8x16_high_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  let cvt_su8 : t -> t -> int8x16 =
   fun low high ->
    let low = to_int8x16_low_saturating_unsigned low in
    to_int8x16_high_saturating_unsigned low high

  external cvtsx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvtsx_int16x8_to_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  let cvtsx_i64 : t -> int64x2 = fun t -> t |> cvtsx_i32 |> Int32x4.cvtsx_i64

  external cvtzx_i32 : t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_cvtzx_int16x8_to_int32x4"
    [@@noalloc] [@@unboxed] [@@builtin]

  let cvtzx_i64 : t -> int64x2 = fun t -> t |> cvtzx_i32 |> Int32x4.cvtzx_i64

  external abs : t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_abs"
    [@@noalloc] [@@unboxed] [@@builtin]

  external hadd : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_hadd"
    [@@noalloc] [@@unboxed] [@@builtin]

  external minposu : t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_minpos_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* multiply the low halfs and write the wide result *)
  external mul_low_long : t -> t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_int16x8_mul_low_long"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* multiply the high halfs and write the wide result *)
  external mul_high_long : t -> t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_int16x8_mul_high_long"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* unsigned multiply the low halfs and write the wide result *)
  external mul_low_long_unsigned : t -> t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_int16x8_mul_low_long_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* multiply the high halfs and write the wide result *)
  external mul_high_long_unsigned : t -> t -> int32x4
    = "caml_vec128_unreachable" "caml_neon_int16x8_mul_high_long_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  let mul_high : t -> t -> t =
   fun a b ->
    let low = mul_low_long a b |> Int32x4.srai 16 in
    let high = mul_high_long a b |> Int32x4.srai 16 in
    let low = Int32x4.to_int16x8_low low in
    Int32x4.to_int16x8_high low high

  let mul_high_unsigned : t -> t -> t =
   fun a b ->
    let low = mul_low_long_unsigned a b |> Int32x4.srli 16 in
    let high = mul_high_long_unsigned a b |> Int32x4.srli 16 in
    let low = Int32x4.to_int16x8_low low in
    Int32x4.to_int16x8_high low high

  external mul_low : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_mul_low"
    [@@noalloc] [@@unboxed] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_neon_int16x8_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int16x8_insert"
    [@@noalloc] [@@builtin]

  external slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int16x8_slli"
    [@@noalloc] [@@builtin]

  external srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int16x8_srli"
    [@@noalloc] [@@builtin]

  external srai : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int16x8_srai"
    [@@noalloc] [@@builtin]

  external neg : t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_neg"
    [@@noalloc] [@@unboxed] [@@builtin]

  external ushl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_ushl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sshl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int16x8_sshl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup : t -> t = "caml_vec128_unreachable" "caml_neon_int16x8_dup"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup_lane : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int16x8_dup_lane"
    [@@noalloc] [@@builtin]

  (* Shifts with [count] in a register. See comment in [Int32x4]. *)

  let sll : t -> t -> t = fun arg count -> ushl arg (dup count)

  let srl : t -> t -> t = fun arg count -> ushl arg (dup (neg count))

  let sra : t -> t -> t = fun arg count -> sshl arg (dup (neg count))
end

module Int8x16 = struct
  type t = int8x16

  external ext :
    (int[@untagged]) ->
    low:(int8x16[@unboxed]) ->
    high:(int8x16[@unboxed]) ->
    (int8x16[@unboxed]) = "caml_vec128_unreachable" "caml_neon_int8x16_ext"
    [@@noalloc] [@@builtin]

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_add_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_add_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_sub_saturating"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_saturating_unsigned : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_sub_saturating_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external max : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_max"
    [@@noalloc] [@@unboxed] [@@builtin]

  external min : t -> t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_min"
    [@@noalloc] [@@unboxed] [@@builtin]

  external maxu : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_max_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external minu : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_min_unsigned"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeqz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_cmpeqz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_cmpgez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgtz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_cmpgtz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmplez : t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_cmplez"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpltz : t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_cmpltz"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpeq : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_cmpeq"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cmpgt : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_cmpgt"
    [@@noalloc] [@@unboxed] [@@builtin]

  external cvtsx_i16 : t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvtsx_int8x16_to_int16x8"
    [@@noalloc] [@@unboxed] [@@builtin]

  let cvtsx_i32 : t -> int32x4 = fun t -> t |> cvtsx_i16 |> Int16x8.cvtsx_i32

  let cvtsx_i64 : t -> int64x2 = fun t -> t |> cvtsx_i16 |> Int16x8.cvtsx_i64

  external cvtzx_i16 : t -> int16x8
    = "caml_vec128_unreachable" "caml_neon_cvtzx_int8x16_to_int16x8"
    [@@noalloc] [@@unboxed] [@@builtin]

  let cvtzx_i32 : t -> int32x4 = fun t -> t |> cvtzx_i16 |> Int16x8.cvtzx_i32

  let cvtzx_i64 : t -> int64x2 = fun t -> t |> cvtzx_i16 |> Int16x8.cvtzx_i64

  external abs : t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_abs"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_or : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_bitwise_or"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_and : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_bitwise_and"
    [@@noalloc] [@@unboxed] [@@builtin]

  external bitwise_xor : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_bitwise_xor"
    [@@noalloc] [@@unboxed] [@@builtin]

  external extract : (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_neon_int8x16_extract"
    [@@noalloc] [@@builtin]

  external insert :
    (int[@untagged]) -> (t[@unboxed]) -> (int[@untagged]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int8x16_insert"
    [@@noalloc] [@@builtin]

  external slli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int8x16_slli"
    [@@noalloc] [@@builtin]

  external srli : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int8x16_srli"
    [@@noalloc] [@@builtin]

  external srai : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int8x16_srai"
    [@@noalloc] [@@builtin]

  external neg : t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_neg"
    [@@noalloc] [@@unboxed] [@@builtin]

  external ushl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_ushl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sshl : t -> t -> t
    = "caml_vec128_unreachable" "caml_neon_int8x16_sshl"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup : t -> t = "caml_vec128_unreachable" "caml_neon_int8x16_dup"
    [@@noalloc] [@@unboxed] [@@builtin]

  external dup_lane : (int[@untagged]) -> (t[@unboxed]) -> (t[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_int8x16_dup_lane"
    [@@noalloc] [@@builtin]

  (* Shifts with [count] in a register. See comment in [Int32x4]. *)

  let sll : t -> t -> t = fun arg count -> ushl arg (dup count)

  let srl : t -> t -> t = fun arg count -> ushl arg (dup (neg count))

  let sra : t -> t -> t = fun arg count -> sshl arg (dup (neg count))
end

module SSE_Util = struct
  type t = int32x4

  let bitwise_and : int64x2 -> int64x2 -> int64x2 = Int64x2.bitwise_and

  let bitwise_or : int64x2 -> int64x2 -> int64x2 = Int64x2.bitwise_or

  let andnot : int64x2 -> int64x2 -> int64x2 =
   fun a b -> Int64x2.bitwise_and (Int64x2.bitwise_not a) b

  let bitwise_xor : int64x2 -> int64x2 -> int64x2 = Int64x2.bitwise_xor

  external high_64_to_low_64 : t -> t -> t
    = "caml_vec128_unreachable" "caml_simd_vec128_high_64_to_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_64_to_high_64 : t -> t -> t
    = "caml_vec128_unreachable" "caml_simd_vec128_low_64_to_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_32 : t -> t -> t
    = "caml_vec128_unreachable" "caml_simd_vec128_interleave_high_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_32 : t -> t -> t
    = "caml_vec128_unreachable" "caml_simd_vec128_interleave_low_32"
    [@@noalloc] [@@unboxed] [@@builtin]

  (* CR gyorsh: looks like there is no single instruction on arm64 that
     corresponds to Intel's [shuffle_32], TBL and TBX are the closest. *)
  let shuffle_32 : int -> t -> t -> t =
   fun ctrl a b ->
    let open Int32x4 in
    let dup_lane lane t =
      match lane with
      | 0 -> dup_lane 0 t
      | 1 -> dup_lane 1 t
      | 2 -> dup_lane 2 t
      | 3 -> dup_lane 3 t
      | _ -> assert false
    in
    let extract lane t =
      match lane with
      | 0 -> extract 0 t
      | 1 -> extract 1 t
      | 2 -> extract 2 t
      | 3 -> extract 3 t
      | _ -> assert false
    in
    let[@inline always] ctrl i = (ctrl lsr (i * 2)) land 3 in
    let res = dup_lane (ctrl 0) a in
    let dst1 = extract (ctrl 1) a in
    let dst2 = extract (ctrl 2) b in
    let dst3 = extract (ctrl 3) b in
    let res = insert 1 res dst1 in
    let res = insert 2 res dst2 in
    let res = insert 3 res dst3 in
    res

  (* CR gyorsh: [movemask_32] is not supported on arm64. This implementation
     uses [t < zero]. The result is in a completely different format:
     [movemask_32] creates a 4-bit mask with one bit of the mask set for each
     negative element of the input, whereas [cmpltz] sets all bits in the
     corresponding vector element of the result.

     The naive sequence below extracts the mask from from the result of
     [cmpltz]. *)
  (* CR-someday gyorsh: optimize the sequence for example see these blog posts:

     https://community.arm.com/arm-community-blogs/b/servers-and-cloud-computing-blog/posts/porting-x86-vector-bitmask-optimizations-to-arm-neon

     : https://zeux.io/2022/09/02/vpexpandb-neon-z3/ *)
  let movemask_32 t =
    let mask = Int32x4.cmpltz t in
    let res = 0l in
    let i = 0 in
    let lane_mask = Int32x4.extract i mask |> Int32.logand Int32.one in
    let res = Int32.logor res (Int32.shift_left lane_mask i) in
    let i = 1 in
    let lane_mask = Int32x4.extract i mask |> Int32.logand Int32.one in
    let res = Int32.logor res (Int32.shift_left lane_mask i) in
    let i = 2 in
    let lane_mask = Int32x4.extract i mask |> Int32.logand Int32.one in
    let res = Int32.logor res (Int32.shift_left lane_mask i) in
    let i = 3 in
    let lane_mask = Int32x4.extract i mask |> Int32.logand Int32.one in
    let res = Int32.logor res (Int32.shift_left lane_mask i) in
    Int32.to_int res
end

module SSE2_Util = struct
  (* See [movemask_32]. *)
  let movemask_8 (t : int8x16) : int =
    let mask = Int8x16.cmpltz t in
    let res = 0 in
    let i = 0 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 1 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 2 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 3 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 4 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 5 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 6 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 7 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 8 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 9 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 10 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 11 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 12 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 13 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 14 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    let i = 15 in
    let lane_mask = Int8x16.extract i mask |> Int.logand Int.one in
    let res = Int.logor res (Int.shift_left lane_mask i) in
    res

  let movemask_64 (t : int64x2) : int =
    let mask = Int64x2.cmpltz t in
    let res = 0L in
    let i = 0 in
    let lane_mask = Int64x2.extract i mask |> Int64.logand Int64.one in
    let res = Int64.logor res (Int64.shift_left lane_mask i) in
    let i = 1 in
    let lane_mask = Int64x2.extract i mask |> Int64.logand Int64.one in
    let res = Int64.logor res (Int64.shift_left lane_mask i) in
    Int64.to_int res

  external shift_left_bytes :
    (int[@untagged]) -> (int8x16[@unboxed]) -> (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_vec128_shift_left_bytes"
    [@@noalloc] [@@builtin]

  external shift_right_bytes :
    (int[@untagged]) -> (int8x16[@unboxed]) -> (int8x16[@unboxed])
    = "caml_vec128_unreachable" "caml_neon_vec128_shift_right_bytes"
    [@@noalloc] [@@builtin]

  let shuffle_64 : int -> int64x2 -> int64x2 -> int64x2 =
   fun ctrl a b ->
    let open Int64x2 in
    let dup_lane lane t =
      match lane with
      | 0 -> dup_lane 0 t
      | 1 -> dup_lane 1 t
      | _ -> assert false
    in
    let extract lane t =
      match lane with 0 -> extract 0 t | 1 -> extract 1 t | _ -> assert false
    in
    let[@inline always] ctrl i = (ctrl lsr i) land 1 in
    let res = dup_lane (ctrl 0) a in
    let dst1 = extract (ctrl 1) b in
    let res = insert 1 res dst1 in
    res

  let shuffle_low_16 : int -> int16x8 -> int16x8 =
   fun ctrl a ->
    let open Int16x8 in
    let extract lane t =
      match lane with
      | 0 -> extract 0 t
      | 1 -> extract 1 t
      | 2 -> extract 2 t
      | 3 -> extract 3 t
      | _ -> assert false
    in
    let[@inline always] ctrl i = (ctrl lsr (i * 2)) land 3 in
    let res = a in
    let i = 0 in
    let lane = extract (ctrl i) a in
    let res = insert i res lane in
    let i = 1 in
    let lane = extract (ctrl i) a in
    let res = insert i res lane in
    let i = 2 in
    let lane = extract (ctrl i) a in
    let res = insert i res lane in
    let i = 3 in
    let lane = extract (ctrl i) a in
    let res = insert i res lane in
    res

  let shuffle_high_16 : int -> int16x8 -> int16x8 =
   fun ctrl a ->
    let open Int16x8 in
    let extract lane t =
      match lane with
      | 4 -> extract 4 t
      | 5 -> extract 5 t
      | 6 -> extract 6 t
      | 7 -> extract 7 t
      | _ -> assert false
    in
    let[@inline always] ctrl i = ((ctrl lsr (i * 2)) land 3) + 4 in
    let res = a in
    let i = 0 in
    let lane = extract (ctrl i) a in
    let res = insert (i + 4) res lane in
    let i = 1 in
    let lane = extract (ctrl i) a in
    let res = insert (i + 4) res lane in
    let i = 2 in
    let lane = extract (ctrl i) a in
    let res = insert (i + 4) res lane in
    let i = 3 in
    let lane = extract (ctrl i) a in
    let res = insert (i + 4) res lane in
    res

  external interleave_high_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_simd_vec128_interleave_high_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_8 : int8x16 -> int8x16 -> int8x16
    = "caml_vec128_unreachable" "caml_simd_vec128_interleave_low_8"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_16 : int16x8 -> int16x8 -> int16x8
    = "caml_vec128_unreachable" "caml_simd_vec128_interleave_high_16"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_16 : int16x8 -> int16x8 -> int16x8
    = "caml_vec128_unreachable" "caml_simd_vec128_interleave_low_16"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_64 : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_simd_vec128_interleave_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_low_64 : int64x2 -> int64x2 -> int64x2
    = "caml_vec128_unreachable" "caml_simd_vec128_interleave_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]
end

module SSE3_Util = struct
  let dup_low_64 a = Int64x2.dup a

  let dup_odd_32 : int32x4 -> int32x4 =
   fun a ->
    let open Int32x4 in
    let extract lane t =
      match lane with
      | 0 -> extract 0 t
      | 1 -> extract 1 t
      | 2 -> extract 2 t
      | 3 -> extract 3 t
      | _ -> assert false
    in
    let res = a in
    let i = 1 in
    let lane = extract i a in
    let res = insert (i - 1) res lane in
    let i = 3 in
    let lane = extract i a in
    let res = insert (i - 1) res lane in
    res

  let dup_even_32 : int32x4 -> int32x4 =
   fun a ->
    let open Int32x4 in
    let extract lane t =
      match lane with
      | 0 -> extract 0 t
      | 1 -> extract 1 t
      | 2 -> extract 2 t
      | 3 -> extract 3 t
      | _ -> assert false
    in
    let res = a in
    let i = 0 in
    let lane = extract i a in
    let res = insert (i + 1) res lane in
    let i = 2 in
    let lane = extract i a in
    let res = insert (i + 1) res lane in
    res
end

(* CR-someday gyorsh: neon doesn't seem to have dedicated instructions for
   comparing strings that correspond to [cmpestr*] and [cmpistr*] instructions
   from amd64. If needed, these can be implemented as sequences of intrinsics to
   match [SSE42_String]. *)
