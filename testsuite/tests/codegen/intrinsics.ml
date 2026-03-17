external select
  : 'a.
  bool -> ('a[@local_opt]) -> ('a[@local_opt]) -> ('a[@local_opt])
  = "caml_csel_value"
[@@noalloc]
[@@no_effects]
[@@no_coeffects]
[@@builtin]

external select_int64
  :  bool
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  -> (int64[@unboxed])
  @@ portable
  = "caml_csel_value" "caml_csel_int64_unboxed"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

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

  let[@inline always] min x y =
    let x' = to_int32 x in
    let y' = to_int32 y in
    if x' <= y' then x else y
end

module Nativeint_u = struct
  type t = nativeint#

  external to_nativeint : t -> (nativeint[@local_opt]) @@ portable =
    "%box_nativeint" [@@warning "-187"]

  external of_nativeint : (nativeint[@local_opt]) -> t @@ portable =
    "%unbox_nativeint" [@@warning "-187"]
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
