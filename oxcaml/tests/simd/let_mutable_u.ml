open Stdlib

external box_int64x2 : int64x2# -> int64x2 = "%box_vec128"
external unbox_int64x2 : int64x2 -> int64x2# = "%unbox_vec128"

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "vec128_of_int64s"
  [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "" "vec128_low_int64"
  [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "vec128_high_int64"
  [@@noalloc] [@@unboxed]

let triangle_i64x2 n =
  let mutable sum = int64x2_of_int64s 0L 0L in
  for i = 1 to n do
    let i_u = Int64.of_int i in
    sum <- Int64x2.add sum (Int64x2.const i_u i_u)
  done;
  #(Int64x2.extract ~ind:0 sum, Int64x2.extract ~ind:1 sum)

let () =
  let #(a, b) = triangle_i64x2 10 in
  Printf.printf "%d %d\n" (Int64.to_int a) (Int64.to_int b)
