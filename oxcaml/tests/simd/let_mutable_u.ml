open Stdlib

external int64x2_of_int64s : int64 -> int64 -> int64x2 = "" "vec128_of_int64s"
  [@@noalloc] [@@unboxed]
external int64x2_low_int64 : int64x2 -> int64 = "" "vec128_low_int64"
  [@@noalloc] [@@unboxed]
external int64x2_high_int64 : int64x2 -> int64 = "" "vec128_high_int64"
  [@@noalloc] [@@unboxed]
external boxed_combine : int64x2 -> int64x2 -> int64x2 = "" "boxed_combine"
  [@@noalloc]
external box : int64x2# -> int64x2 = "%box_vec128"
external unbox : int64x2 -> int64x2# = "%unbox_vec128"


let combine : int64x2# -> int64x2# -> int64x2# = fun x y ->
  unbox (boxed_combine (box x) (box y))

let eq l r = if l <> r then Printf.printf "%Ld <> %Ld\n" l r

let[@inline never] check v l h =
  let vl, vh = int64x2_low_int64 v, int64x2_high_int64 v in
  eq vl l;
  eq vh h

let triangle_i64x2_u n =
  let mutable sum = unbox (int64x2_of_int64s 0L 0L) in
  for i = 1 to n do
    let i_u = Int64.of_int i in
    sum <- combine sum (unbox (int64x2_of_int64s i_u i_u))
  done;
  sum

let () =
  check (box (triangle_i64x2_u 10)) 55L 55L
