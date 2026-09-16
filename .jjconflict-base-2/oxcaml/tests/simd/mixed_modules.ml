open Stdlib_upstream_compatible
external [@layout_poly] id : ('a : any). 'a -> 'a = "%opaque"

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "vec128_of_int64s"
  [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "" "vec128_low_int64"
  [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "vec128_high_int64"
  [@@noalloc] [@@unboxed]
external box : int64x2# -> int64x2 = "%box_vec128"
external unbox : int64x2 -> int64x2# = "%unbox_vec128"

let make_int64x2_u a b = unbox (int64x2_of_int64s a b)

let eq l r = if l <> r then Printf.printf "not equal"
let eq_i64 l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v l h =
  let v = box v in
  let vl, vh = int64x2_low_int64 v, int64x2_high_int64 v in
  eq_i64 vl l;
  eq_i64 vh h


module Simd_module = struct
  let regular_int = 42
  let regular_string = "hello"
  let vec_int = make_int64x2_u 100L 200L
  let unboxed_float = #1.5
  let another_vec = make_int64x2_u 300L 400L
end

let _ = eq (id Simd_module.regular_int) 42
let _ = eq (id Simd_module.regular_string) "hello"
let _ = check (id Simd_module.vec_int) 100L 200L
let _ = eq (Float_u.to_float (id Simd_module.unboxed_float)) 1.5
let _ = check (id Simd_module.another_vec) 300L 400L


module type Simd_sig = sig
  val int_vec : int64x2#
  val mixed_tuple : #(string * int64x2# * float)
end

module Simd_impl : Simd_sig = struct
  let int_vec = make_int64x2_u 10L 20L

  let mixed_tuple = #("simd", make_int64x2_u 30L 40L, 5.5)
end

let _ = check (id Simd_impl.int_vec) 10L 20L
let _ =
  let #(s, v, f) = Simd_impl.mixed_tuple in
  eq s "simd";
  check v 30L 40L;
  eq f 5.5


module Outer = struct
  module Inner = struct
    let simd_val = make_int64x2_u 1000L 2000L
    let regular_val = "nested"
  end

  let outer_simd = make_int64x2_u 3000L 4000L
  let get_inner () = Inner.simd_val
end

let _ =
  check (id Outer.Inner.simd_val) 1000L 2000L;
  eq (id Outer.Inner.regular_val) "nested";
  check (id Outer.outer_simd) 3000L 4000L;
  check ((id Outer.get_inner) ()) 1000L 2000L;
