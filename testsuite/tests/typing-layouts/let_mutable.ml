(* TEST
 include stdlib_upstream_compatible;
 include stdlib_stable;
 flambda2;
 {
   flags = "-extension let_mutable";
   { expect; expect.opt; }
 }{
   flags = "-extension layouts_alpha -extension let_mutable";
   { expect; expect.opt; }
 }{
   flags = "-extension layouts_beta -extension let_mutable";
   { expect; expect.opt; }
 }*)

open Stdlib_upstream_compatible
module Float32_u = Stdlib_stable.Float32_u

let triangle_f64 n =
  let mutable sum = #0.0 in
  for i = 1 to n do
    sum <- Float_u.add sum (Float_u.of_int i)
  done;
  sum
[%%expect.ignore_echo]

let _ = triangle_f64 10 |> Float_u.to_float
[%%expect{|
- : float = 55.
|}]

let triangle_f32 n =
  let mutable sum = #0.0s in
  for i = 1 to n do
    sum <- Float32_u.add sum (Float32_u.of_int i)
  done;
  sum
[%%expect.ignore_echo]

let _ = triangle_f32 10 |> Float32_u.to_float |> Float_u.to_float
[%%expect{|
- : float = 55.
|}]

let triangle_i64 n =
  let mutable sum = #0L in
  for i = 1 to n do
    sum <- Int64_u.add sum (Int64_u.of_int i)
  done;
  sum
[%%expect.ignore_echo]

let () = triangle_i64 10 |> Int64_u.to_int
[%%expect{|
Line 1, characters 9-42:
1 | let () = triangle_i64 10 |> Int64_u.to_int
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "int" but an expression was expected of type
         "unit"
|}]

let triangle_i32 n =
  let mutable sum = #0l in
  for i = 1 to n do
    sum <- Int32_u.add sum (Int32_u.of_int i)
  done;
  sum
[%%expect.ignore_echo]

let _ = triangle_i32 10 |> Int32_u.to_int
[%%expect{|
- : int = 55
|}]

let triangle_i64_i32_f64 n =
  let mutable sum = #(#0L, #(#0l, #0.)) in
  for i = 1 to n do
    let #(a, #(b, c)) = sum in
    sum <- #(Int64_u.add a (Int64_u.of_int i),
             #(Int32_u.add b (Int32_u.of_int i),
               Float_u.add c (Float_u.of_int i)))
  done;
  sum
[%%expect.ignore_echo]

let _ =
  let #(a, #(b, c)) = triangle_i64_i32_f64 10 in
  (Int64_u.to_int a, Int32_u.to_int b, Float_u.to_float c)
[%%expect{|
- : int * int * float = (55, 55, 55.)
|}]
