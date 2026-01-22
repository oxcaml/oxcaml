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

module Int64x4 = struct

  type t = int64x4

  external box : int64x4# -> int64x4 = "%box_vec256"
  external unbox : int64x4 -> int64x4# = "%unbox_vec256"

  external box_128 : int64x2# -> int64x2 = "%box_vec128"
  external unbox_128 : int64x2 -> int64x2# = "%unbox_vec128"

  external interleave_low_64 : int64x2 -> int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_simd_vec128_interleave_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_64 : int64x2 -> int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_simd_vec128_interleave_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external join : int64x2# -> int64x2# -> int64x4# = "%join_vec256"

  external split : int64x4# -> #(int64x2# * int64x2#) = "%split_vec256"

  let join l h = box (join (unbox_128 l) (unbox_128 h))

  let split v =
    let #(l,h) = split (unbox v) in
    box_128 l, box_128 h

  external const1_128 : int64# -> int64x2 = "caml_vec128_unreachable" "caml_int64x2_const1"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_of : int64 -> int64x2 = "caml_vec128_unreachable" "caml_int64x2_low_of_int64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_to : int64x2 -> int64 = "caml_vec128_unreachable" "caml_int64x2_low_to_int64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add_128 : int64x2 -> int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_simd_int64x2_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub_128 : int64x2 -> int64x2 -> int64x2 = "caml_vec128_unreachable" "caml_simd_int64x2_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  let add l r =
    let ll,lh = split l in
    let rl,rh = split r in
    join (add_128 ll rl) (add_128 lh rh)

  let sub l r =
    let ll,lh = split l in
    let rl,rh = split r in
    join (sub_128 ll rl) (sub_128 lh rh)

  let neg x = sub (join (const1_128 #0L) (const1_128 #0L)) x

  let high_to x =
    let x = interleave_high_64 x x in
    low_to x

  let of_i64s x y z w =
    let x = low_of x in
    let y = low_of y in
    let z = low_of z in
    let w = low_of w in
    let xy = interleave_low_64 x y in
    let zw = interleave_low_64 z w in
    let i = join xy zw in
    i

  let mul x y =
    let (xl,xh) = split x in
    let (yl,yh) = split y in
    let xll, yll = low_to xl, low_to yl in
    let xlh, ylh = high_to xl, high_to yl in
    let xhl, yhl = low_to xh, low_to yh in
    let xhh, yhh = high_to xh, high_to yh in
    of_i64s Int64.(mul xhh yhh) Int64.(mul xhl yhl)
            Int64.(mul xlh ylh) Int64.(mul xll yll)

  let of_int i = of_i64s (Int64.of_int i) (Int64.of_int i)
                         (Int64.of_int i) (Int64.of_int i)
  let max_val = of_i64s Int64.max_int Int64.max_int
                        Int64.max_int Int64.max_int
  let min_val = of_i64s Int64.min_int Int64.min_int
                        Int64.min_int Int64.min_int
  let rand x =
    let (l, h) = split x in
    let ll, lh = low_to l, high_to l in
    let hl, hh = low_to h, high_to h in
    of_i64s (Random.int64 hh) (Random.int64 hl)
            (Random.int64 lh) (Random.int64 ll)

  let print x =
    let (l, h) = split x in
    let ll, lh = low_to l, high_to l in
    let hl, hh = low_to h, high_to h in
    Format.printf "%Ld:%Ld:%Ld:%Ld" hh hl lh ll

  let compare x y =
    let (xl,xh) = split x in
    let (yl,yh) = split y in
    let xll, yll = low_to xl, low_to yl in
    let xlh, ylh = high_to xl, high_to yl in
    let xhl, yhl = low_to xh, low_to yh in
    let xhh, yhh = high_to xh, high_to yh in
    let h = Int64.compare xhh yhh in
    if h <> 0 then h else
      let h = Int64.compare xhl yhl in
      if h <> 0 then h else
        let h = Int64.compare xlh ylh in
        if h <> 0 then h else
          Int64.compare xll yll
end