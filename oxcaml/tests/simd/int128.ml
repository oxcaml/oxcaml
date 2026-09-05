module I64 = Stdlib_upstream_compatible.Int64_u

let eq x y =
  if I64.equal x y then ()
  else Printf.printf "%Lx <> %Lx\n" (I64.to_int64 x) (I64.to_int64 y)

let maxi () = #0x7fff_ffff_ffff_ffffL
let mini () = #0x8000_0000_0000_0000L

(* (low,high) + (low,high) -> (low,high) *)
external add_int128 : x0:int64# -> x1:int64# -> y0:int64# -> y1:int64# -> #(int64# * int64#) = "" "caml_int128_add"
[@@noalloc] [@@builtin]

let () =
  let #(z0,z1) = add_int128 ~x0:#1L ~x1:#2L ~y0:#2L ~y1:#1L in
  eq z0 #3L;
  eq z1 #3L

let () =
  let #(z0,z1) = add_int128 ~x0:(maxi ()) ~x1:(mini ()) ~y0:#1L ~y1:(-#1L) in
  eq z0 (mini ());
  eq z1 (maxi ())

let () = (* (1<<64-1) + 1 *)
  let #(z0,z1) = add_int128 ~x0:(-#1L) ~x1:#0L ~y0:#1L ~y1:#0L in
  eq z0 #0L;
  eq z1 #1L

let () = (* 1<<64 + (-1) *)
  let #(z0,z1) = add_int128 ~x0:(#0L) ~x1:(#1L) ~y0:(-#1L) ~y1:(-#1L) in
  eq z0 (-#1L);
  eq z1 #0L

let () = (* (1<<127-1) + 1 *)
  let #(z0,z1) = add_int128 ~x0:(-#1L) ~x1:(maxi ()) ~y0:#1L ~y1:#0L in
  eq z0 #0L;
  eq z1 (mini ())

let () = (* -(1<<127) - 1 *)
  let #(z0,z1) = add_int128 ~x0:#0L ~x1:(mini ()) ~y0:(-#1L) ~y1:(-#1L) in
  eq z0 (-#1L);
  eq z1 (maxi ())

(* (low,high) - (low,high) -> (low,high) *)
external sub_int128 : x0:int64# -> x1:int64# -> y0:int64# -> y1:int64# -> #(int64# * int64#) = "" "caml_int128_sub"
[@@noalloc] [@@builtin]

let () =
  let #(z0,z1) = sub_int128 ~x0:#2L ~x1:#1L ~y0:#1L ~y1:#2L in
  eq z0 #1L;
  eq z1 (-#1L)

let () =
  let #(z0,z1) = sub_int128 ~x0:(maxi ()) ~x1:(mini ()) ~y0:(#1L) ~y1:(-#1L) in
  eq z0 I64.(sub (maxi ()) #1L);
  eq z1 I64.(add (mini ()) #1L)

let () = (* (1<<64-1) - (-1) *)
  let #(z0,z1) = sub_int128 ~x0:(-#1L) ~x1:#0L ~y0:(-#1L) ~y1:(-#1L) in
  eq z0 #0L;
  eq z1 #1L

let () = (* 1<<64 - 1 *)
  let #(z0,z1) = sub_int128 ~x0:(#0L) ~x1:(#1L) ~y0:(#1L) ~y1:#0L in
  eq z0 (-#1L);
  eq z1 #0L

let () = (* (1<<127-1) - (-1) *)
  let #(z0,z1) = sub_int128 ~x0:(-#1L) ~x1:(maxi ()) ~y0:(-#1L) ~y1:(-#1L) in
  eq z0 #0L;
  eq z1 (mini ())

let () = (* -(1<<127) - 1 *)
  let #(z0,z1) = sub_int128 ~x0:#0L ~x1:(mini ()) ~y0:(#1L) ~y1:#0L in
  eq z0 (-#1L);
  eq z1 (maxi ())

(* low * low -> (low,high) *)
external mul_int64 : int64# -> int64# -> #(int64# * int64#) = "" "caml_int64_mul128"
[@@noalloc] [@@builtin]

let () =
  let #(z0,z1) = mul_int64 #2L #3L in
  eq z0 #6L;
  eq z1 #0L

let () = (* -(1<<63) * 2 *)
  let #(z0,z1) = mul_int64 (mini ()) #2L in
  eq z0 #0L;
  eq z1 (-#1L)

let () = (* -(1<<63) * (-1) *)
  let #(z0,z1) = mul_int64 (mini ()) (-#1L) in
  eq z0 (mini ());
  eq z1 #0L

let () = (* (1<<63-1) * -2 *)
  let #(z0,z1) = mul_int64 (maxi ()) (-#2L) in
  eq z0 #2L;
  eq z1 (-#1L)

(* low * low -> (low,high) *)
external unsigned_mul_int64 : int64# -> int64# -> #(int64# * int64#) = "" "caml_unsigned_int64_mul128"
[@@noalloc] [@@builtin]

let () =
  let #(z0,z1) = unsigned_mul_int64 #2L #3L in
  eq z0 #6L;
  eq z1 #0L

let () = (* 1<<63 * 2 *)
  let #(z0,z1) = unsigned_mul_int64 (mini ()) #2L in
  eq z0 #0L;
  eq z1 #1L

let () = (* (1<<64-1) * 2 *)
  let #(z0,z1) = unsigned_mul_int64 (-#1L) #2L in
  eq z0 I64.(shift_left (-#1L) 1);
  eq z1 #1L
