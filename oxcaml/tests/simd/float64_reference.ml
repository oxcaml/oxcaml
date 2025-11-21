type t = float

external c_round : (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float64_round"
  [@@noalloc]

external c_min : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float64_min"
  [@@noalloc]

external c_max : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float64_max"
  [@@noalloc]

external c_min_match_sse : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float64_min_match_sse"
  [@@noalloc]

external c_max_match_sse : (t[@unboxed]) -> (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float64_max_match_sse"
  [@@noalloc]

external c_sqrt : (t[@unboxed]) -> (t[@unboxed])
  = "caml_vec128_unreachable" "float64_sqrt"
  [@@noalloc]

external cvtt_i32 : (t[@unboxed]) -> (int32[@unboxed])
  = "caml_vec128_unreachable" "float64_cvtt_i32"
  [@@noalloc]

external float32x4_of_int64s : int64 -> int64 -> float32x4
  = "caml_vec128_unreachable" "vec128_of_int64s"
  [@@noalloc] [@@unboxed]

let check_floats f =
  let open Float in
  Random.set_state (Random.State.make [| 1234567890 |]);
  f zero zero;
  f zero one;
  f one one;
  f zero minus_one;
  f minus_one minus_one;
  f one minus_one;
  f zero (-0.0);
  f (-0.0) zero;
  f nan zero;
  f zero nan;
  f infinity zero;
  f neg_infinity zero;
  f nan nan;
  f infinity infinity;
  f neg_infinity neg_infinity;
  f neg_infinity infinity;
  f infinity nan;
  f neg_infinity nan;
  f max_float infinity;
  f max_float neg_infinity;
  f min_float infinity;
  f min_float neg_infinity;
  f max_float max_float;
  f min_float min_float;
  f max_float min_float;
  for _ = 0 to 100_000 do
    let f0 = Random.int64 Int64.max_int in
    let f1 = Random.int64 Int64.max_int in
    f
      (if Random.bool ()
      then Int64.float_of_bits f0
      else Int64.(neg f0 |> float_of_bits))
      (if Random.bool ()
      then Int64.float_of_bits f1
      else Int64.(neg f1 |> float_of_bits))
  done
