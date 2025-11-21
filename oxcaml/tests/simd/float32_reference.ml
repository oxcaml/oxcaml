(* For testing *)
type t = int32

let of_float f = Int32.bits_of_float f

let to_float i = Int32.float_of_bits i

external zero : unit -> (t[@unboxed]) = "caml_vec128_unreachable" "float32_zero"
  [@@noalloc]

external neg_zero : unit -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_neg_zero"
  [@@noalloc]

external one : unit -> (t[@unboxed]) = "caml_vec128_unreachable" "float32_one"
  [@@noalloc]

external neg_one : unit -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_neg_one"
  [@@noalloc]

external nan : unit -> (t[@unboxed]) = "caml_vec128_unreachable" "float32_nan"
  [@@noalloc]

external neg_infinity : unit -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_neg_infinity"
  [@@noalloc]

external infinity : unit -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_infinity"
  [@@noalloc]

external maxv : unit -> (t[@unboxed]) = "caml_vec128_unreachable" "float32_maxv"
  [@@noalloc]

external minv : unit -> (t[@unboxed]) = "caml_vec128_unreachable" "float32_minv"
  [@@noalloc]

let zero = zero ()

let neg_zero = neg_zero ()

let one = one ()

let nan = nan ()

let neg_infinity = neg_infinity ()

let infinity = infinity ()

let neg_one = neg_one ()

let maxv = maxv ()

let minv = minv ()

external cvt_i32 : (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_cvt_i32"
  [@@noalloc]

external cvtt_i32 : (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_cvtt_i32"
  [@@noalloc]

external round : (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_round"
  [@@noalloc]

external eq : (t[@unboxed]) -> (t[@unboxed]) -> bool
  = "caml_vec128_unreachable" "float32_eq"
  [@@noalloc]

external lt : (t[@unboxed]) -> (t[@unboxed]) -> bool
  = "caml_vec128_unreachable" "float32_lt"
  [@@noalloc]

external le : (t[@unboxed]) -> (t[@unboxed]) -> bool
  = "caml_vec128_unreachable" "float32_le"
  [@@noalloc]

external neq : (t[@unboxed]) -> (t[@unboxed]) -> bool
  = "caml_vec128_unreachable" "float32_ne"
  [@@noalloc]

external nle : (t[@unboxed]) -> (t[@unboxed]) -> bool
  = "caml_vec128_unreachable" "float32_nle"
  [@@noalloc]

external nlt : (t[@unboxed]) -> (t[@unboxed]) -> bool
  = "caml_vec128_unreachable" "float32_nlt"
  [@@noalloc]

external ord : (t[@unboxed]) -> (t[@unboxed]) -> bool
  = "caml_vec128_unreachable" "float32_ord"
  [@@noalloc]

external uord : (t[@unboxed]) -> (t[@unboxed]) -> bool
  = "caml_vec128_unreachable" "float32_uord"
  [@@noalloc]

external add : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_add"
  [@@noalloc]

external sub : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_sub"
  [@@noalloc]

external mul : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_mul"
  [@@noalloc]

external div : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_div"
  [@@noalloc]

external c_min : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_min"
  [@@noalloc]

external c_max : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_max"
  [@@noalloc]

external min_match_sse : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_min_match_sse"
  [@@noalloc]

external max_match_sse : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_max_match_sse"
  [@@noalloc]

external rcp : (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_rcp"
  [@@noalloc]

external sqrt : (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_sqrt"
  [@@noalloc]

external rsqrt : (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float32_rsqrt"
  [@@noalloc]

let check_floats f =
  Random.set_state (Random.State.make [| 1234567890 |]);
  f zero zero;
  f zero one;
  f one one;
  f zero neg_one;
  f neg_one neg_one;
  f one neg_one;
  f zero neg_zero;
  f nan zero;
  f infinity zero;
  f neg_infinity zero;
  f nan nan;
  f infinity infinity;
  f neg_infinity neg_infinity;
  f neg_infinity infinity;
  f infinity nan;
  f neg_infinity nan;
  f maxv infinity;
  f maxv neg_infinity;
  f minv infinity;
  f minv neg_infinity;
  f maxv maxv;
  f minv minv;
  f maxv minv;
  for _ = 0 to 100_000 do
    let f0 = Random.int32 Int32.max_int in
    let f1 = Random.int32 Int32.max_int in
    f
      (if Random.bool () then f0 else Int32.neg f0)
      (if Random.bool () then f1 else Int32.neg f1)
  done
