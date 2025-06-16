(* TEST
 reference = "${test_source_directory}/let_mutable_native.reference";
 include stdlib_upstream_compatible;
 flambda2;
 {
   flags = "-extension let_mutable";
   native;
 }{
   flags = "-extension layouts_alpha -extension let_mutable";
   native;
 }{
   flags = "-extension layouts_beta -extension let_mutable";
   native;
 }*)

open Stdlib_upstream_compatible

let triangle_i64x2 n =
  let mutable sum = Int64x2.const 0L 0L in
  for i = 1 to n do
    let i_u = Int64.of_int i in
    sum <- Int64x2.add sum (Int64x2.const i_u i_u)
  done;
  #(Int64x2.extract ~ind:0 sum, Int64x2.extract ~ind:1 sum)

let () =
  let #(a, b) = triangle_i64x2 10 in
  Printf.printf "%d %d\n" (Int64.to_int a) (Int64.to_int b)

