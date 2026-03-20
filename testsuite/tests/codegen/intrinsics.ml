module Int32 = struct
  external add :
    (int32[@local_opt]) -> (int32[@local_opt]) -> (int32[@local_opt])
    @@ portable = "%int32_add"

  external of_int : int -> (int32[@local_opt])
    @@ portable = "%int32_of_int"

  external to_int : (int32[@local_opt]) -> int
    @@ portable = "%int32_to_int"

  external compare :
    (int32[@local_opt]) -> (int32[@local_opt]) -> int
    @@ portable = "%compare"

  external bswap :
    (int32[@local_opt]) -> (int32[@local_opt])
    @@ portable = "%bswap_int32"
end

module Int32_u = struct
  type t = int32#

  external to_int32 : t -> (int32[@local_opt]) @@ portable =
    "%box_int32" [@@warning "-187"]

  external of_int32 : (int32[@local_opt]) -> t @@ portable =
    "%unbox_int32" [@@warning "-187"]

  let[@inline always] add x y =
    of_int32 (Int32.add (to_int32 x) (to_int32 y))

  let[@inline always] of_int x = of_int32 (Int32.of_int x)
  let[@inline always] to_int x = Int32.to_int (to_int32 x)

  let[@inline always] compare x y =
    Int32.compare (to_int32 x) (to_int32 y)

  let[@inline always] bswap x =
    of_int32 (Int32.bswap (to_int32 x))

  let[@inline always] min x y =
    let x' = to_int32 x in
    let y' = to_int32 y in
    if x' <= y' then x else y
end

module Nativeint = struct
  external neg :
    (nativeint[@local_opt]) -> (nativeint[@local_opt])
    @@ portable = "%nativeint_neg"

  external add :
    (nativeint[@local_opt]) -> (nativeint[@local_opt])
    -> (nativeint[@local_opt]) @@ portable = "%nativeint_add"

  external sub :
    (nativeint[@local_opt]) -> (nativeint[@local_opt])
    -> (nativeint[@local_opt]) @@ portable = "%nativeint_sub"

  external mul :
    (nativeint[@local_opt]) -> (nativeint[@local_opt])
    -> (nativeint[@local_opt]) @@ portable = "%nativeint_mul"

  external logand :
    (nativeint[@local_opt]) -> (nativeint[@local_opt])
    -> (nativeint[@local_opt]) @@ portable = "%nativeint_and"

  external logor :
    (nativeint[@local_opt]) -> (nativeint[@local_opt])
    -> (nativeint[@local_opt]) @@ portable = "%nativeint_or"

  external logxor :
    (nativeint[@local_opt]) -> (nativeint[@local_opt])
    -> (nativeint[@local_opt]) @@ portable = "%nativeint_xor"

  external shift_left :
    (nativeint[@local_opt]) -> int -> (nativeint[@local_opt])
    @@ portable = "%nativeint_lsl"

  external shift_right :
    (nativeint[@local_opt]) -> int -> (nativeint[@local_opt])
    @@ portable = "%nativeint_asr"

  external shift_right_logical :
    (nativeint[@local_opt]) -> int -> (nativeint[@local_opt])
    @@ portable = "%nativeint_lsr"

  external of_int : int -> (nativeint[@local_opt])
    @@ portable = "%nativeint_of_int"

  external to_int : (nativeint[@local_opt]) -> int
    @@ portable = "%nativeint_to_int"

  external of_int32 : int32 -> nativeint
    @@ portable = "%nativeint_of_int32"

  external to_int32 : nativeint -> int32
    @@ portable = "%nativeint_to_int32"

  external bswap :
    (nativeint[@local_opt]) -> (nativeint[@local_opt])
    @@ portable = "%bswap_native"
end

module Nativeint_u = struct
  type t = nativeint#

  external to_nativeint : t -> (nativeint[@local_opt]) @@ portable =
    "%box_nativeint" [@@warning "-187"]

  external of_nativeint : (nativeint[@local_opt]) -> t @@ portable =
    "%unbox_nativeint" [@@warning "-187"]

  let[@inline always] neg x =
    of_nativeint (Nativeint.neg (to_nativeint x))
  let[@inline always] add x y =
    of_nativeint (Nativeint.add (to_nativeint x) (to_nativeint y))
  let[@inline always] sub x y =
    of_nativeint (Nativeint.sub (to_nativeint x) (to_nativeint y))
  let[@inline always] mul x y =
    of_nativeint (Nativeint.mul (to_nativeint x) (to_nativeint y))
  let[@inline always] logand x y =
    of_nativeint (Nativeint.logand (to_nativeint x) (to_nativeint y))
  let[@inline always] logor x y =
    of_nativeint (Nativeint.logor (to_nativeint x) (to_nativeint y))
  let[@inline always] logxor x y =
    of_nativeint (Nativeint.logxor (to_nativeint x) (to_nativeint y))
  let[@inline always] shift_left x y =
    of_nativeint (Nativeint.shift_left (to_nativeint x) y)
  let[@inline always] shift_right x y =
    of_nativeint (Nativeint.shift_right (to_nativeint x) y)
  let[@inline always] of_int x =
    of_nativeint (Nativeint.of_int x)
  let[@inline always] to_int x =
    Nativeint.to_int (to_nativeint x)
  let[@inline always] of_int32 x =
    of_nativeint (Nativeint.of_int32 x)
  let[@inline always] to_int32 x =
    Nativeint.to_int32 (to_nativeint x)
  let[@inline always] bswap x =
    of_nativeint (Nativeint.bswap (to_nativeint x))
end

module Float = struct
  external neg :
    (float[@local_opt]) -> (float[@local_opt])
    @@ portable = "%negfloat"

  external add :
    (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt])
    @@ portable = "%addfloat"

  external sub :
    (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt])
    @@ portable = "%subfloat"

  external mul :
    (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt])
    @@ portable = "%mulfloat"

  external div :
    (float[@local_opt]) -> (float[@local_opt]) -> (float[@local_opt])
    @@ portable = "%divfloat"

  external abs :
    (float[@local_opt]) -> (float[@local_opt])
    @@ portable = "%absfloat"

  external sqrt : float -> float @@ portable =
    "caml_sqrt_float" "sqrt" [@@unboxed] [@@noalloc]

  external fma : float -> float -> float -> float @@ portable =
    "caml_fma_float" "caml_fma" [@@unboxed] [@@noalloc]

  external of_int : int -> (float[@local_opt]) @@ portable =
    "%floatofint"

  external to_int : (float[@local_opt]) -> int @@ portable =
    "%intoffloat"

  external compare :
    (float[@local_opt]) -> (float[@local_opt]) -> int
    @@ portable = "%compare"

  external sign_bit :
    (float[@unboxed]) -> bool @@ portable =
    "caml_signbit_float" "caml_signbit" [@@noalloc]

  let[@inline always] is_nan x = x <> x
end

module Float_u = struct
  type t = float#

  external to_float : t -> (float[@local_opt]) @@ portable =
    "%box_float" [@@warning "-187"]

  external of_float : (float[@local_opt]) -> t @@ portable =
    "%unbox_float" [@@warning "-187"]

  let[@inline always] neg x = of_float (Float.neg (to_float x))
  let[@inline always] add x y =
    of_float (Float.add (to_float x) (to_float y))
  let[@inline always] sub x y =
    of_float (Float.sub (to_float x) (to_float y))
  let[@inline always] mul x y =
    of_float (Float.mul (to_float x) (to_float y))
  let[@inline always] div x y =
    of_float (Float.div (to_float x) (to_float y))
  let[@inline always] abs x = of_float (Float.abs (to_float x))

  let[@inline always] sqrt x =
    of_float (Float.sqrt (to_float x))

  let[@inline always] fma x y z =
    of_float (Float.fma (to_float x) (to_float y) (to_float z))

  let[@inline always] of_int x = of_float (Float.of_int x)
  let[@inline always] to_int x = Float.to_int (to_float x)

  let[@inline always] compare x y =
    Float.compare (to_float x) (to_float y)

  let[@inline] min x y =
    let x' = to_float x and y' = to_float y in
    if y' > x' || (not (Float.sign_bit y') && Float.sign_bit x') then
      if Float.is_nan y' then of_float y' else of_float x'
    else if Float.is_nan x' then of_float x'
    else of_float y'

  let[@inline] max x y =
    let x' = to_float x and y' = to_float y in
    if y' > x' || (not (Float.sign_bit y') && Float.sign_bit x') then
      if Float.is_nan x' then of_float x' else of_float y'
    else if Float.is_nan y' then of_float y'
    else of_float x'

  let[@inline] min_num x y =
    let x' = to_float x and y' = to_float y in
    if y' > x' || (not (Float.sign_bit y') && Float.sign_bit x') then
      if Float.is_nan x' then of_float y' else of_float x'
    else if Float.is_nan y' then of_float x'
    else of_float y'

  let[@inline] max_num x y =
    let x' = to_float x and y' = to_float y in
    if y' > x' || (not (Float.sign_bit y') && Float.sign_bit x') then
      if Float.is_nan y' then of_float x' else of_float y'
    else if Float.is_nan x' then of_float y'
    else of_float x'
end

module Int64 = struct
  external neg :
    (int64[@local_opt]) -> (int64[@local_opt])
    @@ portable = "%int64_neg"

  external add :
    (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt])
    @@ portable = "%int64_add"

  external sub :
    (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt])
    @@ portable = "%int64_sub"

  external mul :
    (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt])
    @@ portable = "%int64_mul"

  external div :
    (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt])
    @@ portable = "%int64_div"

  external rem :
    (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt])
    @@ portable = "%int64_mod"

  external logand :
    (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt])
    @@ portable = "%int64_and"

  external logor :
    (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt])
    @@ portable = "%int64_or"

  external logxor :
    (int64[@local_opt]) -> (int64[@local_opt]) -> (int64[@local_opt])
    @@ portable = "%int64_xor"

  external shift_left :
    (int64[@local_opt]) -> int -> (int64[@local_opt])
    @@ portable = "%int64_lsl"

  external shift_right :
    (int64[@local_opt]) -> int -> (int64[@local_opt])
    @@ portable = "%int64_asr"

  external shift_right_logical :
    (int64[@local_opt]) -> int -> (int64[@local_opt])
    @@ portable = "%int64_lsr"

  external of_int : int -> (int64[@local_opt])
    @@ portable = "%int64_of_int"

  external to_int : (int64[@local_opt]) -> int
    @@ portable = "%int64_to_int"

  external of_float : float -> int64 @@ portable =
    "caml_int64_of_float" "caml_int64_of_float_unboxed"
    [@@unboxed] [@@noalloc] [@@builtin]

  external to_float : int64 -> float @@ portable =
    "caml_int64_to_float" "caml_int64_to_float_unboxed"
    [@@unboxed] [@@noalloc] [@@builtin]

  external of_int32 : int32 -> int64
    @@ portable = "%int64_of_int32"

  external to_int32 : int64 -> int32
    @@ portable = "%int64_to_int32"

  external of_nativeint : nativeint -> int64
    @@ portable = "%int64_of_nativeint"

  external to_nativeint : int64 -> nativeint
    @@ portable = "%int64_to_nativeint"

  external bits_of_float : float -> int64 @@ portable =
    "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed"
    [@@unboxed] [@@noalloc]

  external float_of_bits : int64 -> float @@ portable =
    "caml_int64_float_of_bits" "caml_int64_float_of_bits_unboxed"
    [@@unboxed] [@@noalloc]

  external compare :
    (int64[@local_opt]) -> (int64[@local_opt]) -> int
    @@ portable = "%compare"

  external bswap :
    (int64[@local_opt]) -> (int64[@local_opt])
    @@ portable = "%bswap_int64"
end

module Int64_u = struct
  type t = int64#

  external to_int64 : t -> (int64[@local_opt]) @@ portable =
    "%box_int64" [@@warning "-187"]

  external of_int64 : (int64[@local_opt]) -> t @@ portable =
    "%unbox_int64" [@@warning "-187"]

  let[@inline always] neg x = of_int64 (Int64.neg (to_int64 x))
  let[@inline always] add x y =
    of_int64 (Int64.add (to_int64 x) (to_int64 y))
  let[@inline always] sub x y =
    of_int64 (Int64.sub (to_int64 x) (to_int64 y))
  let[@inline always] mul x y =
    of_int64 (Int64.mul (to_int64 x) (to_int64 y))
  let[@inline always] div x y =
    of_int64 (Int64.div (to_int64 x) (to_int64 y))
  let[@inline always] rem x y =
    of_int64 (Int64.rem (to_int64 x) (to_int64 y))

  let[@inline always] logand x y =
    of_int64 (Int64.logand (to_int64 x) (to_int64 y))

  let[@inline always] logor x y =
    of_int64 (Int64.logor (to_int64 x) (to_int64 y))

  let[@inline always] logxor x y =
    of_int64 (Int64.logxor (to_int64 x) (to_int64 y))

  let[@inline always] lognot x =
    of_int64 (Int64.logxor (to_int64 x) (-1L))

  let[@inline always] shift_left x y =
    of_int64 (Int64.shift_left (to_int64 x) y)

  let[@inline always] shift_right x y =
    of_int64 (Int64.shift_right (to_int64 x) y)

  let[@inline always] shift_right_logical x y =
    of_int64 (Int64.shift_right_logical (to_int64 x) y)

  let[@inline always] of_int x = of_int64 (Int64.of_int x)
  let[@inline always] to_int x = Int64.to_int (to_int64 x)

  let[@inline always] succ x =
    of_int64 (Int64.add (to_int64 x) 1L)
  let[@inline always] pred x =
    of_int64 (Int64.sub (to_int64 x) 1L)

  let[@inline always] bswap x =
    of_int64 (Int64.bswap (to_int64 x))

  let[@inline always] abs x =
    let n = to_int64 x in
    if n >= 0L then x else of_int64 (Int64.neg n)

  let[@inline always] unsigned_to_int x =
    let n = to_int64 x in
    let max_int = Int64.of_int Stdlib.max_int in
    if n >= 0L && n <= max_int then Some (Int64.to_int n) else None

  let[@inline always] unsigned_compare x y =
    Int64.compare
      (Int64.sub (to_int64 x) 0x8000000000000000L)
      (Int64.sub (to_int64 y) 0x8000000000000000L)

  let[@inline always] unsigned_lt x y =
    Int64.compare
      (Int64.sub x 0x8000000000000000L)
      (Int64.sub y 0x8000000000000000L)
    < 0

  let[@inline always] unsigned_div x y =
    let n = to_int64 x and d = to_int64 y in
    if d < 0L then if unsigned_lt n d then of_int64 0L else of_int64 1L
    else
      let q =
        Int64.shift_left
          (Int64.div (Int64.shift_right_logical n 1) d) 1
      in
      let r = Int64.sub n (Int64.mul q d) in
      if unsigned_lt r d then of_int64 q
      else of_int64 (Int64.add q 1L)

  let[@inline always] unsigned_rem x y =
    let n = to_int64 x in
    let d = to_int64 y in
    Int64.sub n (Int64.mul (to_int64 (unsigned_div x y)) d)
    |> of_int64

  let[@inline always] of_float x = of_int64 (Int64.of_float x)
  let[@inline always] to_float x = Int64.to_float (to_int64 x)
  let[@inline always] of_int32 x = of_int64 (Int64.of_int32 x)
  let[@inline always] to_int32 x = Int64.to_int32 (to_int64 x)
  let[@inline always] of_nativeint x =
    of_int64 (Int64.of_nativeint x)
  let[@inline always] to_nativeint x =
    Int64.to_nativeint (to_int64 x)

  let[@inline always] of_int32_u x =
    of_int64 (Int64.of_int32 (Int32_u.to_int32 x))

  let[@inline always] to_int32_u x =
    Int32_u.of_int32 (Int64.to_int32 (to_int64 x))

  let[@inline always] of_nativeint_u x =
    of_int64 (Int64.of_nativeint (Nativeint_u.to_nativeint x))

  let[@inline always] to_nativeint_u x =
    Nativeint_u.of_nativeint (Int64.to_nativeint (to_int64 x))

  let[@inline always] bits_of_float x =
    of_int64 (Int64.bits_of_float x)

  let[@inline always] float_of_bits x =
    Int64.float_of_bits (to_int64 x)

  let[@inline always] compare x y =
    Int64.compare (to_int64 x) (to_int64 y)

  let[@inline always] equal x y = to_int64 x = to_int64 y

  let[@inline always] min x y =
    let x' = to_int64 x and y' = to_int64 y in
    if x' <= y' then x else y

  let[@inline always] max x y =
    let x' = to_int64 x and y' = to_int64 y in
    if x' >= y' then x else y
end

module Int16_u = struct
  type t = int16#

  external add : t -> t -> t @@ portable = "%int16#_add"
  external succ : t -> t @@ portable = "%int16#_succ"
  external mul : t -> t -> t @@ portable = "%int16#_mul"
  external equal : t -> t -> bool @@ portable = "%int16#_equal"
  external notequal : t -> t -> bool @@ portable = "%int16#_notequal"
  external shift_left : t -> int -> t @@ portable = "%int16#_asr"
  external shift_right : t -> int -> t @@ portable = "%int16#_lsl"
  external logical_shift_right : t -> int -> t @@ portable = "%int16#_lsr"
  external bit_and : t -> t -> t @@ portable = "%int16#_and"
  external bit_or : t -> t -> t @@ portable = "%int16#_or"
  external bit_xor : t -> t -> t @@ portable = "%int16#_xor"
  external bswap : t -> t @@ portable = "%int16#_bswap"
  external neg : t -> t @@ portable = "%int16#_neg"
  external pred : t -> t @@ portable = "%int16#_pred"
  external sub : t -> t -> t @@ portable = "%int16#_sub"
  external div : t -> t -> t @@ portable = "%int16#_div"
  external rem : t -> t -> t @@ portable = "%int16#_mod"
  external unsafe_div : t -> t -> t @@ portable = "%int16#_unsafe_div"
  external unsafe_rem : t -> t -> t @@ portable = "%int16#_unsafe_mod"
  external compare : t -> t -> int @@ portable = "%int16#_compare"
  external greaterequal : t -> t -> bool @@ portable = "%int16#_greaterequal"
  external greaterthan : t -> t -> bool @@ portable = "%int16#_greaterthan"
  external lessequal : t -> t -> bool @@ portable = "%int16#_lessequal"
  external lessthan : t -> t -> bool @@ portable = "%int16#_lessthan"
  external unsigned_compare : t -> t -> int @@ portable
    = "%int16#_unsigned_compare"
  external unsigned_greaterequal : t -> t -> bool @@ portable
    = "%int16#_unsigned_greaterequal"
  external unsigned_greaterthan : t -> t -> bool @@ portable
    = "%int16#_unsigned_greaterthan"
  external unsigned_lessequal : t -> t -> bool @@ portable
    = "%int16#_unsigned_lessequal"
  external unsigned_lessthan : t -> t -> bool @@ portable
    = "%int16#_unsigned_lessthan"
  external of_float : (float[@local_opt]) -> t @@ portable
    = "%int16#_of_float"
  external of_float_u : float# -> t @@ portable = "%int16#_of_float#"
  external of_float32 : (float32[@local_opt]) -> t @@ portable
    = "%int16#_of_float32"
  external of_float32_u : float32# -> t @@ portable = "%int16#_of_float32#"
  external of_int : int -> t @@ portable = "%int16#_of_int"
  external of_int_u : int# -> t @@ portable = "%int16#_of_int#"
  external of_int16 : int16 -> t @@ portable = "%int16#_of_int16"
  external of_int32 : (int32[@local_opt]) -> t @@ portable = "%int16#_of_int32"
  external of_int32_u : int32# -> t @@ portable = "%int16#_of_int32#"
  external of_int64 : (int64[@local_opt]) -> t @@ portable = "%int16#_of_int64"
  external of_int64_u : int64# -> t @@ portable = "%int16#_of_int64#"
  external of_int8 : int8 -> t @@ portable = "%int16#_of_int8"
  external of_int8_u : int8# -> t @@ portable = "%int16#_of_int8#"
  external of_nativeint : (nativeint[@local_opt]) -> t @@ portable
    = "%int16#_of_nativeint"
  external of_nativeint_u : nativeint# -> t @@ portable
    = "%int16#_of_nativeint#"
  external to_float : t -> (float[@local_opt]) @@ portable = "%float_of_int16#"
  external to_float_u : t -> float# @@ portable = "%float#_of_int16#"
  external to_float32 : t -> (float32[@local_opt]) @@ portable
    = "%float32_of_int16#"
  external to_float32_u : t -> float32# @@ portable = "%float32#_of_int16#"
  external to_int : t -> int @@ portable = "%int_of_int16#"
  external to_int_u : t -> int# @@ portable = "%int#_of_int16#"
  external to_int16 : t -> int16 @@ portable = "%int16_of_int16#"
  external to_int32 : t -> (int32[@local_opt]) @@ portable = "%int32_of_int16#"
  external to_int32_u : t -> int32# @@ portable = "%int32#_of_int16#"
  external to_int64 : t -> (int64[@local_opt]) @@ portable = "%int64_of_int16#"
  external to_int64_u : t -> int64# @@ portable = "%int64#_of_int16#"
  external to_int8 : t -> int8 @@ portable = "%int8_of_int16#"
  external to_int8_u : t -> int8# @@ portable = "%int8#_of_int16#"
  external to_nativeint : t -> (nativeint[@local_opt]) @@ portable
    = "%nativeint_of_int16#"
  external to_nativeint_u : t -> nativeint# @@ portable
    = "%nativeint#_of_int16#"

  external popcount : t -> t = "" "caml_popcnt_int16"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
  external ctz : t -> t = "" "caml_lzcnt_int16"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
  external clz : t -> t = "" "caml_bmi_tzcnt_int16"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external select : bool -> t -> t -> t = "" "caml_csel_int16_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
end

module Int8_u = struct
  type t = int8#

  external add : t -> t -> t @@ portable = "%int8#_add"
  external succ : t -> t @@ portable = "%int8#_succ"
  external mul : t -> t -> t @@ portable = "%int8#_mul"
  external equal : t -> t -> bool @@ portable = "%int8#_equal"
  external notequal : t -> t -> bool @@ portable = "%int8#_notequal"
  external shift_left : t -> int -> t @@ portable = "%int8#_asr"
  external shift_right : t -> int -> t @@ portable = "%int8#_lsl"
  external logical_shift_right : t -> int -> t @@ portable = "%int8#_lsr"
  external bit_and : t -> t -> t @@ portable = "%int8#_and"
  external bit_or : t -> t -> t @@ portable = "%int8#_or"
  external bit_xor : t -> t -> t @@ portable = "%int8#_xor"
  external bswap : t -> t @@ portable = "%int8#_bswap"
  external neg : t -> t @@ portable = "%int8#_neg"
  external pred : t -> t @@ portable = "%int8#_pred"
  external sub : t -> t -> t @@ portable = "%int8#_sub"
  external div : t -> t -> t @@ portable = "%int8#_div"
  external rem : t -> t -> t @@ portable = "%int8#_mod"
  external unsafe_div : t -> t -> t @@ portable = "%int8#_unsafe_div"
  external unsafe_rem : t -> t -> t @@ portable = "%int8#_unsafe_mod"
  external compare : t -> t -> int @@ portable = "%int8#_compare"
  external greaterequal : t -> t -> bool @@ portable = "%int8#_greaterequal"
  external greaterthan : t -> t -> bool @@ portable = "%int8#_greaterthan"
  external lessequal : t -> t -> bool @@ portable = "%int8#_lessequal"
  external lessthan : t -> t -> bool @@ portable = "%int8#_lessthan"
  external unsigned_compare : t -> t -> int @@ portable
    = "%int8#_unsigned_compare"
  external unsigned_greaterequal : t -> t -> bool @@ portable
    = "%int8#_unsigned_greaterequal"
  external unsigned_greaterthan : t -> t -> bool @@ portable
    = "%int8#_unsigned_greaterthan"
  external unsigned_lessequal : t -> t -> bool @@ portable
    = "%int8#_unsigned_lessequal"
  external unsigned_lessthan : t -> t -> bool @@ portable
    = "%int8#_unsigned_lessthan"
  external of_float : (float[@local_opt]) -> t @@ portable
    = "%int8#_of_float"
  external of_float_u : float# -> t @@ portable = "%int8#_of_float#"
  external of_float32 : (float32[@local_opt]) -> t @@ portable
    = "%int8#_of_float32"
  external of_float32_u : float32# -> t @@ portable = "%int8#_of_float32#"
  external of_int : int -> t @@ portable = "%int8#_of_int"
  external of_int_u : int# -> t @@ portable = "%int8#_of_int#"
  external of_int16 : int16 -> t @@ portable = "%int8#_of_int16"
  external of_int16_u : int16# -> t @@ portable = "%int8#_of_int16#"
  external of_int32 : (int32[@local_opt]) -> t @@ portable = "%int8#_of_int32"
  external of_int32_u : int32# -> t @@ portable = "%int8#_of_int32#"
  external of_int64 : (int64[@local_opt]) -> t @@ portable = "%int8#_of_int64"
  external of_int64_u : int64# -> t @@ portable = "%int8#_of_int64#"
  external of_int8 : int8 -> t @@ portable = "%int8#_of_int8"
  external of_nativeint : (nativeint[@local_opt]) -> t @@ portable
    = "%int8#_of_nativeint"
  external of_nativeint_u : nativeint# -> t @@ portable = "%int8#_of_nativeint#"
  external to_float : t -> (float[@local_opt]) @@ portable = "%float_of_int8#"
  external to_float_u : t -> float# @@ portable = "%float#_of_int8#"
  external to_float32 : t -> (float32[@local_opt]) @@ portable
    = "%float32_of_int8#"
  external to_float32_u : t -> float32# @@ portable = "%float32#_of_int8#"
  external to_int : t -> int @@ portable = "%int_of_int8#"
  external to_int_u : t -> int# @@ portable = "%int#_of_int8#"
  external to_int16 : t -> int16 @@ portable = "%int16_of_int8#"
  external to_int16_u : t -> int16# @@ portable = "%int16#_of_int8#"
  external to_int32 : t -> (int32[@local_opt]) @@ portable = "%int32_of_int8#"
  external to_int32_u : t -> int32# @@ portable = "%int32#_of_int8#"
  external to_int64 : t -> (int64[@local_opt]) @@ portable = "%int64_of_int8#"
  external to_int64_u : t -> int64# @@ portable = "%int64#_of_int8#"
  external to_int8 : t -> int8 @@ portable = "%int8_of_int8#"
  external to_nativeint : t -> (nativeint[@local_opt]) @@ portable
    = "%nativeint_of_int8#"
  external to_nativeint_u : t -> nativeint# @@ portable = "%nativeint#_of_int8#"

  external popcount : t -> t = "" "caml_int8_popcnt_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
  external ctz : t -> t = "" "caml_int8_ctz_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
  external clz : t -> t = "" "caml_int8_clz_untagged_to_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external select : bool -> t -> t -> t = "" "caml_csel_int8_untagged"
  [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]
end

module Bytes = struct
  external unsafe_get : (bytes[@local_opt]) -> int -> int
    @@ portable = "%bytes_unsafe_get"

  external unsafe_set : (bytes[@local_opt]) -> int -> int -> unit
    @@ portable = "%bytes_unsafe_set"

  external unsafe_get_int8 : (bytes[@local_opt]) -> int -> int
    @@ portable = "%caml_bytes_geti8u"

  external unsafe_get_uint16_ne : (bytes[@local_opt]) -> int -> int
    @@ portable = "%caml_bytes_get16u"

  external unsafe_set_uint16_ne : (bytes[@local_opt]) -> int -> int -> unit
    @@ portable = "%caml_bytes_set16u"

  external unsafe_get_int16_ne : (bytes[@local_opt]) -> int -> int
    @@ portable = "%caml_bytes_geti16u"

  external unsafe_get_int32_ne : (bytes[@local_opt]) -> int -> int32#
    @@ portable = "%caml_bytes_get32u#" [@@warning "-187"]

  external unsafe_set_int32_ne : (bytes[@local_opt]) -> int -> int32# -> unit
    @@ portable = "%caml_bytes_set32u#" [@@warning "-187"]

  external get_int32_ne : (bytes[@local_opt]) -> int -> int32#
    @@ portable = "%caml_bytes_get32#" [@@warning "-187"]

  external unsafe_get_int64_ne : (bytes[@local_opt]) -> int -> int64#
    @@ portable = "%caml_bytes_get64u#" [@@warning "-187"]

  external unsafe_set_int64_ne : (bytes[@local_opt]) -> int -> int64# -> unit
    @@ portable = "%caml_bytes_set64u#" [@@warning "-187"]

  external unsafe_get_int64_ne_indexed_by_int64 :
    (bytes[@local_opt]) -> int64# -> int64#
    @@ portable = "%caml_bytes_get64u#_indexed_by_int64#"
    [@@warning "-187"]

  external unsafe_set_int64_ne_indexed_by_int64 :
    (bytes[@local_opt]) -> int64# -> int64# -> unit
    @@ portable = "%caml_bytes_set64u#_indexed_by_int64#"
    [@@warning "-187"]

  external unsafe_get_int8_indexed_by_int64 :
    (bytes[@local_opt]) -> int64# -> int
    @@ portable = "%caml_bytes_geti8u_indexed_by_int64#"
    [@@warning "-187"]

  external unsafe_set_int8_indexed_by_int64 :
    (bytes[@local_opt]) -> int64# -> int -> unit
    @@ portable = "%caml_bytes_set8u_indexed_by_int64#"
    [@@warning "-187"]

  external unsafe_get_int32_ne_indexed_by_int64 :
    (bytes[@local_opt]) -> int64# -> int32#
    @@ portable = "%caml_bytes_get32u#_indexed_by_int64#"
    [@@warning "-187"]

  external unsafe_set_int32_ne_indexed_by_int64 :
    (bytes[@local_opt]) -> int64# -> int32# -> unit
    @@ portable = "%caml_bytes_set32u#_indexed_by_int64#"
    [@@warning "-187"]

  external unsafe_get_float32_ne : (bytes[@local_opt]) -> int -> float32#
    @@ portable = "%caml_bytes_getf32u#" [@@warning "-187"]

  external unsafe_set_float32_ne : (bytes[@local_opt]) -> int -> float32# -> unit
    @@ portable = "%caml_bytes_setf32u#" [@@warning "-187"]

  external unsafe_get_float32_ne_indexed_by_int64 :
    (bytes[@local_opt]) -> int64# -> float32#
    @@ portable = "%caml_bytes_getf32u#_indexed_by_int64#"
    [@@warning "-187"]

  external unsafe_set_float32_ne_indexed_by_int64 :
    (bytes[@local_opt]) -> int64# -> float32# -> unit
    @@ portable = "%caml_bytes_setf32u#_indexed_by_int64#"
    [@@warning "-187"]

  external length : (bytes[@local_opt]) -> int @@ portable = "%bytes_length"
end

module String = struct
  external unsafe_get_int32_ne : (string[@local_opt]) -> int -> int32#
    @@ portable = "%caml_string_get32u#" [@@warning "-187"]

  external unsafe_get_int64_ne : (string[@local_opt]) -> int -> int64#
    @@ portable = "%caml_string_get64u#" [@@warning "-187"]

  external unsafe_get_float32_ne : (string[@local_opt]) -> int -> float32#
    @@ portable = "%caml_string_getf32u#" [@@warning "-187"]

  external unsafe_get_int8_indexed_by_int64 :
    (string[@local_opt]) -> int64# -> int
    @@ portable = "%caml_string_geti8u_indexed_by_int64#"
    [@@warning "-187"]

  external unsafe_get_int32_ne_indexed_by_int64 :
    (string[@local_opt]) -> int64# -> int32#
    @@ portable = "%caml_string_get32u#_indexed_by_int64#"
    [@@warning "-187"]

  external unsafe_get_int64_ne_indexed_by_int64 :
    (string[@local_opt]) -> int64# -> int64#
    @@ portable = "%caml_string_get64u#_indexed_by_int64#"
    [@@warning "-187"]

  external unsafe_get_float32_ne_indexed_by_int64 :
    (string[@local_opt]) -> int64# -> float32#
    @@ portable = "%caml_string_getf32u#_indexed_by_int64#"
    [@@warning "-187"]

  external length : (string[@local_opt]) -> int @@ portable = "%string_length"
end

module Floatarray = struct
  external unsafe_get :
    (floatarray[@local_opt]) @ shared -> int -> (float[@local_opt])
    @@ portable = "%floatarray_unsafe_get"

  external unsafe_set :
    (floatarray[@local_opt]) -> int -> (float[@local_opt]) -> unit
    @@ portable = "%floatarray_unsafe_set"

  external length :
    (floatarray[@local_opt]) @ immutable -> int
    @@ stateless = "%floatarray_length"
end

module Array = struct
  external unsafe_get :
    ('a : any mod separable).
    ('a array[@local_opt]) -> int -> 'a
    @@ portable = "%array_unsafe_get"
    [@@layout_poly]

  external unsafe_set :
    ('a : any mod separable).
    ('a array[@local_opt]) -> int -> 'a -> unit
    @@ portable = "%array_unsafe_set"
    [@@layout_poly]

  external get :
    ('a : any mod separable).
    ('a array[@local_opt]) -> int -> 'a
    @@ portable = "%array_safe_get"
    [@@layout_poly]

  external set :
    ('a : any mod separable).
    ('a array[@local_opt]) -> int -> 'a -> unit
    @@ portable = "%array_safe_set"
    [@@layout_poly]

  external length :
    ('a : any mod separable).
    ('a array[@local_opt]) @ immutable -> int
    @@ stateless = "%array_length"
    [@@layout_poly]
end

module Float32 = struct
  external neg :
    (float32[@local_opt]) -> (float32[@local_opt])
    @@ portable = "%negfloat32"

  external add :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    @@ portable = "%addfloat32"

  external sub :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    @@ portable = "%subfloat32"

  external mul :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    @@ portable = "%mulfloat32"

  external div :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    @@ portable = "%divfloat32"

  external abs :
    (float32[@local_opt]) -> (float32[@local_opt])
    @@ portable = "%absfloat32"

  external sqrt : float32 -> float32 @@ portable =
    "caml_sqrt_float32_bytecode" "sqrtf" [@@unboxed] [@@noalloc]

  external of_float :
    (float[@local_opt]) -> (float32[@local_opt])
    @@ portable = "%float32offloat"

  external to_float :
    (float32[@local_opt]) -> (float[@local_opt])
    @@ portable = "%floatoffloat32"

  external of_int : int -> float32 @@ portable = "%float32ofint"
  external to_int : (float32[@local_opt]) -> int @@ portable = "%intoffloat32"

  external eq :
    (float32[@local_opt]) -> (float32[@local_opt]) -> bool
    @@ portable = "%eqfloat32"

  external ne :
    (float32[@local_opt]) -> (float32[@local_opt]) -> bool
    @@ portable = "%noteqfloat32"

  external lt :
    (float32[@local_opt]) -> (float32[@local_opt]) -> bool
    @@ portable = "%ltfloat32"

  external le :
    (float32[@local_opt]) -> (float32[@local_opt]) -> bool
    @@ portable = "%lefloat32"

  external gt :
    (float32[@local_opt]) -> (float32[@local_opt]) -> bool
    @@ portable = "%gtfloat32"

  external ge :
    (float32[@local_opt]) -> (float32[@local_opt]) -> bool
    @@ portable = "%gefloat32"
end

module Float32_u = struct
  type t = float32#

  external to_float32 : t -> (float32[@local_opt]) @@ portable =
    "%box_float32" [@@warning "-187"]

  external of_float32 : (float32[@local_opt]) -> t @@ portable =
    "%unbox_float32" [@@warning "-187"]

  let[@inline always] neg x =
    of_float32 (Float32.neg (to_float32 x))
  let[@inline always] add x y =
    of_float32 (Float32.add (to_float32 x) (to_float32 y))
  let[@inline always] sub x y =
    of_float32 (Float32.sub (to_float32 x) (to_float32 y))
  let[@inline always] mul x y =
    of_float32 (Float32.mul (to_float32 x) (to_float32 y))
  let[@inline always] div x y =
    of_float32 (Float32.div (to_float32 x) (to_float32 y))
  let[@inline always] abs x =
    of_float32 (Float32.abs (to_float32 x))
  let[@inline always] sqrt x =
    of_float32 (Float32.sqrt (to_float32 x))
  let[@inline always] of_float x =
    of_float32 (Float32.of_float x)
  let[@inline always] to_float x =
    Float32.to_float (to_float32 x)
  let[@inline always] of_int x =
    of_float32 (Float32.of_int x)
  let[@inline always] to_int x =
    Float32.to_int (to_float32 x)
  let[@inline always] eq x y =
    Float32.eq (to_float32 x) (to_float32 y)
  let[@inline always] lt x y =
    Float32.lt (to_float32 x) (to_float32 y)
end

module Int = struct
  external bswap16 : int -> int @@ portable = "%bswap16"
end

external reinterpret_tagged_int63_as_unboxed_int64 :
  int -> int64# @@ portable =
  "%reinterpret_tagged_int63_as_unboxed_int64" [@@warning "-187"]

external reinterpret_unboxed_int64_as_tagged_int63 :
  int64# -> int @@ portable =
  "%reinterpret_unboxed_int64_as_tagged_int63" [@@warning "-187"]

external cpu_relax : unit -> unit @@ portable = "%cpu_relax"

external opaque : 'a -> 'a = "%opaque"

external obj_is_int : 'a -> bool = "%obj_is_int"

external get_header : 'a -> nativeint = "%get_header"
  [@@warning "-187"]

external int_as_pointer : int -> 'a = "%int_as_pointer"

(* Stdlib primitives *)

external ignore :
  ('a : value_or_null). 'a -> unit @@ portable = "%ignore"

external fst :
  ('a * 'b[@local_opt]) -> ('a[@local_opt]) @@ portable = "%field0_immut"

external snd :
  ('a * 'b[@local_opt]) -> ('b[@local_opt]) @@ portable = "%field1_immut"

external ref :
  ('a : value_or_null). 'a -> ('a ref[@local_opt])
  @@ portable = "%makemutable"

external ( ! ) :
  ('a : value_or_null). ('a ref[@local_opt]) -> 'a
  @@ portable = "%field0"

external ( := ) :
  ('a : value_or_null). ('a ref[@local_opt]) -> 'a -> unit
  @@ portable = "%setfield0"

external incr : (int ref[@local_opt]) -> unit @@ portable = "%incr"
external decr : (int ref[@local_opt]) -> unit @@ portable = "%decr"

module Builtins = struct
  (* Conditional select *)

  external select :
    'a.
    bool -> ('a[@local_opt]) -> ('a[@local_opt])
    -> ('a[@local_opt])
    = "caml_csel_value"
    [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

  external select_int32 :
    bool -> (int32#[@unboxed]) -> (int32#[@unboxed])
    -> (int32#[@unboxed]) @@ portable
    = "caml_csel_value" "caml_csel_int32_unboxed"
    [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]
    [@@warning "-187"]

  external select_int64 :
    bool -> (int64#[@unboxed]) -> (int64#[@unboxed])
    -> (int64#[@unboxed]) @@ portable
    = "caml_csel_value" "caml_csel_int64_unboxed"
    [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]
    [@@warning "-187"]

  external select_nativeint :
    bool -> (nativeint#[@unboxed]) -> (nativeint#[@unboxed])
    -> (nativeint#[@unboxed]) @@ portable
    = "caml_csel_value" "caml_csel_nativeint_unboxed"
    [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]
    [@@warning "-187"]

  (* Count leading zeros *)

  external int_clz : int -> (int[@untagged])
    = "" "caml_int_clz_tagged_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external int64_clz :
    (int64[@unboxed]) -> (int[@untagged])
    = "" "caml_int64_clz_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external int64_clz_nonzero :
    (int64[@unboxed]) -> (int[@untagged])
    = "" "caml_int64_clz_nonzero_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external int32_clz :
    (int32[@unboxed]) -> (int[@untagged])
    = "" "caml_int32_clz_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external int32_clz_nonzero :
    (int32[@unboxed]) -> (int[@untagged])
    = "" "caml_int32_clz_nonzero_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external nativeint_clz :
    (nativeint[@unboxed]) -> (int[@untagged])
    = "" "caml_nativeint_clz_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external nativeint_clz_nonzero :
    (nativeint[@unboxed]) -> (int[@untagged])
    = "" "caml_nativeint_clz_nonzero_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (* Count trailing zeros *)

  external int_ctz :
    (int[@untagged]) -> (int[@untagged])
    = "" "caml_int_ctz_untagged_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external int64_ctz :
    (int64[@unboxed]) -> (int[@untagged])
    = "" "caml_int64_ctz_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external int64_ctz_nonzero :
    (int64[@unboxed]) -> (int[@untagged])
    = "" "caml_int64_ctz_nonzero_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external int32_ctz :
    (int32[@unboxed]) -> (int[@untagged])
    = "" "caml_int32_ctz_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external int32_ctz_nonzero :
    (int32[@unboxed]) -> (int[@untagged])
    = "" "caml_int32_ctz_nonzero_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external nativeint_ctz :
    (nativeint[@unboxed]) -> (int[@untagged])
    = "" "caml_nativeint_ctz_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external nativeint_ctz_nonzero :
    (nativeint[@unboxed]) -> (int[@untagged])
    = "" "caml_nativeint_ctz_nonzero_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (* Population count *)

  external int_popcnt : int -> (int[@untagged])
    = "" "caml_int_popcnt_tagged_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external int64_popcnt :
    (int64[@unboxed]) -> (int[@untagged])
    = "" "caml_int64_popcnt_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external int32_popcnt :
    (int32[@unboxed]) -> (int[@untagged])
    = "" "caml_int32_popcnt_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external nativeint_popcnt :
    (nativeint[@unboxed]) -> (int[@untagged])
    = "" "caml_nativeint_popcnt_unboxed_to_untagged"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (* High multiply *)

  external int64_mulhi_s :
    (int64[@unboxed]) -> (int64[@unboxed]) -> (int64[@unboxed])
    = "" "caml_signed_int64_mulh_unboxed"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  external int64_mulhi_u :
    (int64[@unboxed]) -> (int64[@unboxed]) -> (int64[@unboxed])
    = "" "caml_unsigned_int64_mulh_unboxed"
    [@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

  (* Prefetch *)

  external prefetch_read_high :
    'a -> unit = "" "caml_prefetch_read_high"
    [@@noalloc] [@@builtin]

  external prefetch_read_moderate :
    'a -> unit = "" "caml_prefetch_read_moderate"
    [@@noalloc] [@@builtin]

  external prefetch_read_low :
    'a -> unit = "" "caml_prefetch_read_low"
    [@@noalloc] [@@builtin]

  external prefetch_read_none :
    'a -> unit = "" "caml_prefetch_read_none"
    [@@noalloc] [@@builtin]

  external prefetch_write_high :
    'a -> unit = "" "caml_prefetch_write_high"
    [@@noalloc] [@@builtin]

  external prefetch_write_low :
    'a -> unit = "" "caml_prefetch_write_low"
    [@@noalloc] [@@builtin]

  (* Pause *)

  external pause_hint : unit -> unit
    = "" "caml_pause_hint"
    [@@noalloc] [@@builtin]

  (* Native pointer load/store *)

  external native_pointer_load_int64 :
    nativeint# -> int64#
    = "" "caml_native_pointer_load_unboxed_int64"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_store_int64 :
    nativeint# -> int64# -> unit
    = "" "caml_native_pointer_store_unboxed_int64"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_load_int32 :
    nativeint# -> int32#
    = "" "caml_native_pointer_load_unboxed_int32"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_store_int32 :
    nativeint# -> int32# -> unit
    = "" "caml_native_pointer_store_unboxed_int32"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_load_nativeint :
    nativeint# -> nativeint#
    = "" "caml_native_pointer_load_unboxed_nativeint"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_store_nativeint :
    nativeint# -> nativeint# -> unit
    = "" "caml_native_pointer_store_unboxed_nativeint"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_load_float :
    nativeint# -> float#
    = "" "caml_native_pointer_load_unboxed_float"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_store_float :
    nativeint# -> float# -> unit
    = "" "caml_native_pointer_store_unboxed_float"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_load_uint8 :
    nativeint# -> int
    = "" "caml_native_pointer_load_unsigned_int8"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_store_uint8 :
    nativeint# -> int -> unit
    = "" "caml_native_pointer_store_unsigned_int8"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_load_sint8 :
    nativeint# -> int
    = "" "caml_native_pointer_load_signed_int8"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_store_sint8 :
    nativeint# -> int -> unit
    = "" "caml_native_pointer_store_signed_int8"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_load_uint16 :
    nativeint# -> int
    = "" "caml_native_pointer_load_unsigned_int16"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_store_uint16 :
    nativeint# -> int -> unit
    = "" "caml_native_pointer_store_unsigned_int16"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_load_sint16 :
    nativeint# -> int
    = "" "caml_native_pointer_load_signed_int16"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  external native_pointer_store_sint16 :
    nativeint# -> int -> unit
    = "" "caml_native_pointer_store_signed_int16"
    [@@noalloc] [@@builtin] [@@warning "-187"]

  (* Native pointer atomics - int *)

  external native_pointer_fetch_add_int :
    (nativeint[@unboxed]) -> (int[@untagged])
    -> (int[@untagged])
    = "" "caml_native_pointer_fetch_and_add_int_untagged"
    [@@noalloc] [@@builtin]

  external native_pointer_fetch_sub_int :
    (nativeint[@unboxed]) -> (int[@untagged])
    -> (int[@untagged])
    = "" "caml_native_pointer_fetch_and_sub_int_untagged"
    [@@noalloc] [@@builtin]

  external native_pointer_cas_int :
    (nativeint[@unboxed]) -> (int[@untagged])
    -> (int[@untagged]) -> bool
    = "" "caml_native_pointer_compare_and_swap_int_untagged"
    [@@noalloc] [@@builtin]

  (* Native pointer atomics - int64 *)

  external native_pointer_fetch_add_int64 :
    (nativeint[@unboxed]) -> (int64[@unboxed])
    -> (int64[@unboxed])
    = "" "caml_native_pointer_fetch_and_add_int64_unboxed"
    [@@noalloc] [@@builtin]

  external native_pointer_fetch_sub_int64 :
    (nativeint[@unboxed]) -> (int64[@unboxed])
    -> (int64[@unboxed])
    = "" "caml_native_pointer_fetch_and_sub_int64_unboxed"
    [@@noalloc] [@@builtin]

  external native_pointer_cas_int64 :
    (nativeint[@unboxed]) -> (int64[@unboxed])
    -> (int64[@unboxed]) -> bool
    = "" "caml_native_pointer_compare_and_swap_int64_unboxed"
    [@@noalloc] [@@builtin]

  (* Native pointer atomics - int32 *)

  external native_pointer_fetch_add_int32 :
    (nativeint[@unboxed]) -> (int32[@unboxed])
    -> (int32[@unboxed])
    = "" "caml_native_pointer_fetch_and_add_int32_unboxed"
    [@@noalloc] [@@builtin]

  external native_pointer_fetch_sub_int32 :
    (nativeint[@unboxed]) -> (int32[@unboxed])
    -> (int32[@unboxed])
    = "" "caml_native_pointer_fetch_and_sub_int32_unboxed"
    [@@noalloc] [@@builtin]

  external native_pointer_cas_int32 :
    (nativeint[@unboxed]) -> (int32[@unboxed])
    -> (int32[@unboxed]) -> bool
    = "" "caml_native_pointer_compare_and_swap_int32_unboxed"
    [@@noalloc] [@@builtin]

  (* Native pointer atomics - nativeint *)

  external native_pointer_fetch_add_nativeint :
    (nativeint[@unboxed]) -> (nativeint[@unboxed])
    -> (nativeint[@unboxed])
    = "" "caml_native_pointer_fetch_and_add_nativeint_unboxed"
    [@@noalloc] [@@builtin]

  external native_pointer_fetch_sub_nativeint :
    (nativeint[@unboxed]) -> (nativeint[@unboxed])
    -> (nativeint[@unboxed])
    = "" "caml_native_pointer_fetch_and_sub_nativeint_unboxed"
    [@@noalloc] [@@builtin]

  external native_pointer_cas_nativeint :
    (nativeint[@unboxed]) -> (nativeint[@unboxed])
    -> (nativeint[@unboxed]) -> bool
    = "" "caml_native_pointer_compare_and_swap_nativeint_unboxed"
    [@@noalloc] [@@builtin]

  (* Ext_pointer: OCaml int encoding of 2-byte-aligned external
     pointer. The LSB is set (not shifted like normal int tagging). *)

  type ext_pointer = private int

  external ext_pointer_load_untagged_int :
    ext_pointer -> (int[@untagged])
    = "" "caml_ext_pointer_load_unboxed_nativeint"
    [@@noalloc] [@@builtin] [@@no_effects]

  external ext_pointer_store_untagged_int :
    ext_pointer -> (int[@untagged]) -> unit
    = "" "caml_ext_pointer_store_unboxed_nativeint"
    [@@noalloc] [@@builtin] [@@no_coeffects]

  external ext_pointer_load_unboxed_nativeint :
    ext_pointer -> (nativeint[@unboxed])
    = "" "caml_ext_pointer_load_unboxed_nativeint"
    [@@noalloc] [@@builtin] [@@no_effects]

  external ext_pointer_store_unboxed_nativeint :
    ext_pointer -> (nativeint[@unboxed]) -> unit
    = "" "caml_ext_pointer_store_unboxed_nativeint"
    [@@noalloc] [@@builtin] [@@no_coeffects]

  external ext_pointer_load_unboxed_int64 :
    ext_pointer -> (int64[@unboxed])
    = "" "caml_ext_pointer_load_unboxed_int64"
    [@@noalloc] [@@builtin] [@@no_effects]

  external ext_pointer_store_unboxed_int64 :
    ext_pointer -> (int64[@unboxed]) -> unit
    = "" "caml_ext_pointer_store_unboxed_int64"
    [@@noalloc] [@@builtin] [@@no_coeffects]

  external ext_pointer_load_unboxed_int32 :
    ext_pointer -> (int32[@unboxed])
    = "" "caml_ext_pointer_load_unboxed_int32"
    [@@noalloc] [@@builtin] [@@no_effects]

  external ext_pointer_store_unboxed_int32 :
    ext_pointer -> (int32[@unboxed]) -> unit
    = "" "caml_ext_pointer_store_unboxed_int32"
    [@@noalloc] [@@builtin] [@@no_coeffects]

  external ext_pointer_load_unboxed_float :
    ext_pointer -> (float[@unboxed])
    = "" "caml_ext_pointer_load_unboxed_float"
    [@@noalloc] [@@builtin] [@@no_effects]

  external ext_pointer_store_unboxed_float :
    ext_pointer -> (float[@unboxed]) -> unit
    = "" "caml_ext_pointer_store_unboxed_float"
    [@@noalloc] [@@builtin] [@@no_coeffects]

  external ext_pointer_load_unsigned_int8 :
    ext_pointer -> int
    = "" "caml_ext_pointer_load_unsigned_int8"
    [@@noalloc] [@@builtin]

  external ext_pointer_store_unsigned_int8 :
    ext_pointer -> int -> unit
    = "" "caml_ext_pointer_store_unsigned_int8"
    [@@noalloc] [@@builtin]

  external ext_pointer_load_signed_int16 :
    ext_pointer -> int
    = "" "caml_ext_pointer_load_signed_int16"
    [@@noalloc] [@@builtin]

  external ext_pointer_store_unsigned_int16 :
    ext_pointer -> int -> unit
    = "" "caml_ext_pointer_store_unsigned_int16"
    [@@noalloc] [@@builtin]

  (* Ext_pointer atomics *)

  external ext_pointer_fetch_add_int :
    ext_pointer -> (int[@untagged]) -> (int[@untagged])
    = "" "caml_ext_pointer_fetch_and_add_int_untagged"
    [@@noalloc] [@@builtin]

  external ext_pointer_cas_int :
    ext_pointer -> (int[@untagged])
    -> (int[@untagged]) -> bool
    = "" "caml_ext_pointer_compare_and_swap_int_untagged"
    [@@noalloc] [@@builtin]

  external ext_pointer_fetch_add_int64 :
    ext_pointer -> (int64[@unboxed])
    -> (int64[@unboxed])
    = "" "caml_ext_pointer_fetch_and_add_int64_unboxed"
    [@@noalloc] [@@builtin]

  external ext_pointer_fetch_add_int32 :
    ext_pointer -> (int32[@unboxed])
    -> (int32[@unboxed])
    = "" "caml_ext_pointer_fetch_and_add_int32_unboxed"
    [@@noalloc] [@@builtin]

  external ext_pointer_fetch_add_nativeint :
    ext_pointer -> (nativeint[@unboxed])
    -> (nativeint[@unboxed])
    = "" "caml_ext_pointer_fetch_and_add_nativeint_unboxed"
    [@@noalloc] [@@builtin]

  (* Bigstring: (char, int8_unsigned_elt, c_layout) Array1.t *)

  type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout)
    Bigarray.Array1.t

  external bigstring_fetch_add_int :
    bigstring -> (int[@untagged]) -> (int[@untagged])
    -> (int[@untagged])
    = "" "caml_bigstring_fetch_and_add_int_untagged"
    [@@noalloc] [@@builtin]

  external bigstring_fetch_add_int64 :
    bigstring -> (int[@untagged]) -> (int64[@unboxed])
    -> (int64[@unboxed])
    = "" "caml_bigstring_fetch_and_add_int64_unboxed"
    [@@noalloc] [@@builtin]

  external bigstring_fetch_add_int32 :
    bigstring -> (int[@untagged]) -> (int32[@unboxed])
    -> (int32[@unboxed])
    = "" "caml_bigstring_fetch_and_add_int32_unboxed"
    [@@noalloc] [@@builtin]

  external bigstring_cas_int :
    bigstring -> (int[@untagged]) -> (int[@untagged])
    -> (int[@untagged]) -> bool
    = "" "caml_bigstring_compare_and_swap_int_untagged"
    [@@noalloc] [@@builtin]

  external bigstring_cas_int64 :
    bigstring -> (int[@untagged]) -> (int64[@unboxed])
    -> (int64[@unboxed]) -> bool
    = "" "caml_bigstring_compare_and_swap_int64_unboxed"
    [@@noalloc] [@@builtin]
end
