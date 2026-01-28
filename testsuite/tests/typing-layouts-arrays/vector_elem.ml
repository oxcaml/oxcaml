module Int64x2 = struct

  type t = int64x2

  external box : int64x2# -> int64x2 = "%box_vec128"
  external unbox : int64x2 -> int64x2# = "%unbox_vec128"

  external interleave_low_64 : t -> t -> t = "caml_vec128_unreachable" "caml_simd_vec128_interleave_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_64 : t -> t -> t = "caml_vec128_unreachable" "caml_simd_vec128_interleave_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_of : int64 -> t = "caml_vec128_unreachable" "caml_int64x2_low_of_int64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_to : t -> int64 = "caml_vec128_unreachable" "caml_int64x2_low_to_int64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external const1 : int64 -> t = "caml_vec128_unreachable" "caml_int64x2_const1"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add : t -> t -> t = "caml_vec128_unreachable" "caml_simd_int64x2_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "caml_vec128_unreachable" "caml_simd_int64x2_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  let neg x = sub (const1 0L) x

  let high_to x =
    let x = interleave_high_64 x x in
    low_to x

  let of_i64s x y =
    let x = low_of x in
    let y = low_of y in
    interleave_low_64 x y

  let mul x y =
    let xl, yl = low_to x, low_to y in
    let xh, yh = high_to x, high_to y in
    of_i64s Int64.(mul xh yh) Int64.(mul xl yl)

  let of_int i = of_i64s (Int64.of_int i) (Int64.of_int i)

  let max_val =
    match Sys.backend_type with
    | Bytecode | Other _ -> Obj.magic ()
    | Native ->
      of_i64s Int64.max_int Int64.max_int
  let min_val =
    match Sys.backend_type with
    | Bytecode | Other _ -> Obj.magic ()
    | Native ->
      of_i64s Int64.min_int Int64.min_int

  let rand x =
    let h, l = high_to x, low_to x in
    of_i64s (Random.int64 h) (Random.int64 l)

  let print v = Format.printf "%Ld:%Ld" (high_to v) (low_to v)

  let compare v1 v2 =
    let v1h, v2h = high_to v1, high_to v2 in
    let v1l, v2l = low_to v1, low_to v2 in
    let h = Int64.compare v1h v2h in
    if h = 0 then Int64.compare v1l v2l else h
end
