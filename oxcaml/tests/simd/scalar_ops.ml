[@@@ocaml.warning "-unused-value-declaration"]

[@@@ocaml.warning "-unused-module"]

external int64x2_of_int64s : int64 -> int64 -> int64x2
  = "caml_vec128_unreachable" "vec128_of_int64s"
  [@@noalloc] [@@unboxed]

external int64x2_low_int64 : int64x2 -> int64
  = "caml_vec128_unreachable" "vec128_low_int64"
  [@@noalloc] [@@unboxed]

external int64x2_high_int64 : int64x2 -> int64
  = "caml_vec128_unreachable" "vec128_high_int64"
  [@@noalloc] [@@unboxed]

let eq lv hv l h =
  if l <> lv then Printf.printf "%016Lx <> %016Lx\n" lv l;
  if h <> hv then Printf.printf "%016Lx <> %016Lx\n" hv h

let eq' x y = if x <> y then Printf.printf "%016Lx <> %016Lx\n" x y

let eqi x y = if x <> y then Printf.printf "%d <> %d\n" x y

let eq64 x y = if x <> y then Printf.printf "%Ld <> %Ld\n" x y

let eqf x y =
  if Float.is_nan x && Float.is_nan y
  then ()
  else if x <> y
  then Printf.printf "%f <> %f\n" x y

external f32_to_f64 : float32 -> float = "%floatoffloat32"

let eqf32 x y =
  let x, y = f32_to_f64 x, f32_to_f64 y in
  if Float.is_nan x && Float.is_nan y
  then ()
  else if x <> y
  then Printf.printf "%f <> %f\n" x y

module Int = struct
  external count_leading_zeros : int -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int_clz_tagged_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_leading_zeros2 : int -> int
    = "caml_vec128_unreachable" "caml_int_clz_untagged_to_untagged"
    [@@untagged] [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_set_bits : int -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int_popcnt_tagged_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_set_bits2 : int -> int
    = "caml_vec128_unreachable" "caml_int_popcnt_untagged_to_untagged"
    [@@untagged] [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_trailing_zeros : int -> int
    = "caml_vec128_unreachable" "caml_int_ctz_untagged_to_untagged"
    [@@untagged] [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  let check f g =
    let open Int in
    Random.set_state (Random.State.make [| 1234567890 |]);
    eqi (f zero) (g zero);
    eqi (f one) (g one);
    eqi (f minus_one) (g minus_one);
    eqi (f max_int) (g max_int);
    eqi (f min_int) (g min_int);
    for _ = 0 to 100_000 do
      let i = Random.full_int max_int in
      let i = if Random.bool () then i else neg i in
      eqi (f i) (g i)
    done

  let rec clz i =
    if i = 0
    then 63
    else if i land 0x4000000000000000 = 0x4000000000000000
    then 0
    else 1 + clz (i lsl 1)

  let rec ctz i =
    if i = 0 then 63 else if i land 1 = 1 then 0 else 1 + ctz (i lsr 1)

  let rec popcnt i = if i = 0 then 0 else (i land 1) + popcnt (i lsr 1)

  let () =
    check count_leading_zeros clz;
    check count_leading_zeros2 clz;
    check count_trailing_zeros ctz;
    check count_set_bits popcnt;
    check count_set_bits2 popcnt
end

module Int64 = struct
  external count_leading_zeros : (int64[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int64_clz_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_leading_zeros_nonzero_arg :
    (int64[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int64_clz_nonzero_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_trailing_zeros : (int64[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int64_ctz_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (** Same as [count_trailing_zeros] except if the argument is zero,
      then the result is undefined. Emits more efficient code. *)
  external count_trailing_zeros_nonzero_arg :
    (int64[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int64_ctz_nonzero_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (** [count_set_bits n] returns the number of bits that are 1 in [n]. *)
  external count_set_bits : (int64[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int64_popcnt_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  let check' ~eq ?nonzero f g =
    let nz = Option.value ~default:false nonzero in
    let open Stdlib.Int64 in
    Random.set_state (Random.State.make [| 1234567890 |]);
    if not nz then eq (f zero) (g zero);
    eq (f one) (g one);
    eq (f minus_one) (g minus_one);
    eq (f max_int) (g max_int);
    eq (f min_int) (g min_int);
    for _ = 0 to 100_000 do
      let i = Random.int64 max_int in
      let i = if Random.bool () then i else neg i in
      if (not nz) || i <> 0L then eq (f i) (g i)
    done

  let check ?nonzero f g = check' ~eq:eqi ?nonzero f g

  let rec clz i =
    if i = 0L
    then 64
    else if Int64.(logand i 0x8000000000000000L = 0x8000000000000000L)
    then 0
    else 1 + clz (Int64.shift_left i 1)

  let rec ctz i =
    if i = 0L
    then 64
    else if Int64.(logand i 1L = 1L)
    then 0
    else 1 + ctz (Int64.shift_right_logical i 1)

  let rec popcnt i =
    if i = 0L
    then 0
    else Int64.(logand i 1L |> to_int) + popcnt Int64.(shift_right_logical i 1)

  let () =
    check count_leading_zeros clz;
    check ~nonzero:true count_leading_zeros_nonzero_arg clz;
    check count_trailing_zeros ctz;
    check ~nonzero:true count_trailing_zeros_nonzero_arg ctz;
    check count_set_bits popcnt
end

module Int32 = struct
  external count_leading_zeros : (int32[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int32_clz_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_leading_zeros_nonzero_arg :
    (int32[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int32_clz_nonzero_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external count_trailing_zeros : (int32[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int32_ctz_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (** Same as [count_trailing_zeros] except if the argument is zero,
      then the result is undefined. Emits more efficient code. *)
  external count_trailing_zeros_nonzero_arg :
    (int32[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int32_ctz_nonzero_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (** [count_set_bits n] returns the number of bits that are 1 in [n]. *)
  external count_set_bits : (int32[@unboxed]) -> (int[@untagged])
    = "caml_vec128_unreachable" "caml_int32_popcnt_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  let check ?nonzero f g =
    let nz = Option.value ~default:false nonzero in
    let open Stdlib.Int32 in
    Random.set_state (Random.State.make [| 1234567890 |]);
    if not nz then eqi (f zero) (g zero);
    eqi (f one) (g one);
    eqi (f minus_one) (g minus_one);
    eqi (f max_int) (g max_int);
    eqi (f min_int) (g min_int);
    for _ = 0 to 100_000 do
      let i = Random.int32 max_int in
      let i = if Random.bool () then i else neg i in
      if (not nz) || i <> 0l then eqi (f i) (g i)
    done

  let rec clz i =
    if i = 0l
    then 32
    else if Int32.(logand i 0x80000000l = 0x80000000l)
    then 0
    else 1 + clz (Int32.shift_left i 1)

  let rec ctz i =
    if i = 0l
    then 32
    else if Int32.(logand i 1l = 1l)
    then 0
    else 1 + ctz (Int32.shift_right_logical i 1)

  let rec popcnt i =
    if i = 0l
    then 0
    else Int32.(logand i 1l |> to_int) + popcnt Int32.(shift_right_logical i 1)

  let () =
    check count_leading_zeros clz;
    check ~nonzero:true count_leading_zeros_nonzero_arg clz;
    check count_trailing_zeros ctz;
    check ~nonzero:true count_trailing_zeros_nonzero_arg ctz;
    check count_set_bits popcnt
end

module Float = struct
  external to_int64 : float -> int64
    = "caml_int64_of_float" "caml_int64_of_float_unboxed"
    [@@unboxed] [@@noalloc] [@@builtin]

  external of_int64 : int64 -> float
    = "caml_int64_to_float" "caml_int64_to_float_unboxed"
    [@@unboxed] [@@noalloc] [@@builtin]

  external max_f64 : float -> float -> float
    = "caml_vec128_unreachable" "caml_simd_float64_max"
    [@@noalloc] [@@builtin] [@@unboxed]

  external max_f32 : float32 -> float32 -> float32
    = "caml_vec128_unreachable" "caml_simd_float32_max"
    [@@noalloc] [@@builtin] [@@unboxed]

  external min_f64 : float -> float -> float
    = "caml_vec128_unreachable" "caml_simd_float64_min"
    [@@noalloc] [@@builtin] [@@unboxed]

  external min_f32 : float32 -> float32 -> float32
    = "caml_vec128_unreachable" "caml_simd_float32_min"
    [@@noalloc] [@@builtin] [@@unboxed]

  external iround_f64 : float -> int64
    = "caml_vec128_unreachable" "caml_simd_cast_float64_int64"
    [@@noalloc] [@@builtin] [@@unboxed]

  external iround_f32 : float32 -> int64
    = "caml_vec128_unreachable" "caml_simd_cast_float32_int64"
    [@@noalloc] [@@builtin] [@@unboxed]

  external round_current_f64 : float -> float
    = "caml_vec128_unreachable" "caml_simd_float64_round_current"
    [@@noalloc] [@@builtin] [@@unboxed]

  external round_neg_inf_f64 : float -> float
    = "caml_vec128_unreachable" "caml_simd_float64_round_neg_inf"
    [@@noalloc] [@@builtin] [@@unboxed]

  external round_pos_inf_f64 : float -> float
    = "caml_vec128_unreachable" "caml_simd_float64_round_pos_inf"
    [@@noalloc] [@@builtin] [@@unboxed]

  external round_towards_zero_f64 : float -> float
    = "caml_vec128_unreachable" "caml_simd_float64_round_towards_zero"
    [@@noalloc] [@@builtin] [@@unboxed]

  external round_current_f32 : float32 -> float32
    = "caml_vec128_unreachable" "caml_simd_float32_round_current"
    [@@noalloc] [@@builtin] [@@unboxed]

  external round_neg_inf_f32 : float32 -> float32
    = "caml_vec128_unreachable" "caml_simd_float32_round_neg_inf"
    [@@noalloc] [@@builtin] [@@unboxed]

  external round_pos_inf_f32 : float32 -> float32
    = "caml_vec128_unreachable" "caml_simd_float32_round_pos_inf"
    [@@noalloc] [@@builtin] [@@unboxed]

  external round_towards_zero_f32 : float32 -> float32
    = "caml_vec128_unreachable" "caml_simd_float32_round_towards_zero"
    [@@noalloc] [@@builtin] [@@unboxed]

  external float32_of_bits : int32 -> float32
    = "caml_float32_of_bits_bytecode" "caml_float32_of_bits"
    [@@unboxed] [@@noalloc]

  let f32_nan = float32_of_bits 0x7fc00001l

  let () =
    Float64_reference.check_floats (fun x _ ->
        eq64 (Float64_reference.c_to_int64 x) (to_int64 x));
    Int64.check' ~eq:eqf Float64_reference.c_of_int64 of_int64

  let () =
    eqf (round_current_f64 0.5) 0.;
    eqf (round_neg_inf_f64 0.5) 0.;
    eqf (round_pos_inf_f64 0.5) 1.;
    eqf (round_towards_zero_f64 0.5) 0.;
    eqf (round_current_f64 (-0.5)) 0.;
    eqf (round_neg_inf_f64 (-0.5)) (-1.);
    eqf (round_pos_inf_f64 (-0.5)) 0.;
    eqf (round_towards_zero_f64 (-0.5)) 0.;
    eqf32 (round_current_f32 0.5s) 0.s;
    eqf32 (round_neg_inf_f32 0.5s) 0.s;
    eqf32 (round_pos_inf_f32 0.5s) 1.s;
    eqf32 (round_towards_zero_f32 0.5s) 0.s;
    eqf32 (round_current_f32 (-0.5s)) 0.s;
    eqf32 (round_neg_inf_f32 (-0.5s)) (-1.s);
    eqf32 (round_pos_inf_f32 (-0.5s)) 0.s;
    eqf32 (round_towards_zero_f32 (-0.5s)) 0.s

  (* If either argument is nan, returns the second argument. *)
  let () =
    eqf (max_f64 (-1.) 1.) 1.;
    eqf (max_f64 1. (-1.)) 1.;
    eqf (max_f64 Float.nan 1.) 1.;
    eqf (max_f64 1. Float.nan) Float.nan;
    eqf (max_f64 Float.nan Float.nan) Float.nan;
    eqf32 (max_f32 (-1.s) 1.s) 1.s;
    eqf32 (max_f32 1.s (-1.s)) 1.s;
    eqf32 (max_f32 f32_nan 1.s) 1.s;
    eqf32 (max_f32 1.s f32_nan) f32_nan;
    eqf32 (max_f32 f32_nan f32_nan) f32_nan;
    eqf (min_f64 (-1.) 1.) (-1.);
    eqf (min_f64 1. (-1.)) (-1.);
    eqf (min_f64 Float.nan 1.) 1.;
    eqf (min_f64 1. Float.nan) Float.nan;
    eqf (min_f64 Float.nan Float.nan) Float.nan;
    eqf32 (min_f32 (-1.s) 1.s) (-1.s);
    eqf32 (min_f32 1.s (-1.s)) (-1.s);
    eqf32 (min_f32 f32_nan 1.s) 1.s;
    eqf32 (min_f32 1.s f32_nan) f32_nan;
    eqf32 (min_f32 f32_nan f32_nan) f32_nan

  (* In native code, the current mode should be half-to-even. Result is
     unspecified for inf/nan/out-of-range values. *)
  let () =
    eq64 (iround_f64 0.) 0L;
    eq64 (iround_f64 (-0.)) 0L;
    eq64 (iround_f64 0.5) 0L;
    eq64 (iround_f64 (-0.5)) 0L;
    eq64 (iround_f64 1.5) 2L;
    eq64 (iround_f64 (-1.5)) (-2L);
    eq64 (iround_f32 0.s) 0L;
    eq64 (iround_f32 (-0.s)) 0L;
    eq64 (iround_f32 0.5s) 0L;
    eq64 (iround_f32 (-0.5s)) 0L;
    eq64 (iround_f32 1.5s) 2L;
    eq64 (iround_f32 (-1.5s)) (-2L)
end
