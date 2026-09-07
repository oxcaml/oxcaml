(* TEST
   modules = "specialise_lifted_escaping_lib.ml";
   flambda2;
   flags += "-O4 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
   { native; }
 *)

module Lib = Specialise_lifted_escaping_lib

let square x = x * x
let double x = 2 * x

let run n =
  let f1, v1 = Lib.single square n in
  let f2, v2 = Lib.single double n in
  let g1, w1 = Lib.mixed square double n in
  let g2, w2 = Lib.mixed double square n in
  Printf.printf "%d: %d %d; %d %d; %d %d; %d %d\n"
    n v1 (f1 n) v2 (f2 n) w1 (g1 n) w2 (g2 n)

let () = List.iter (fun n -> run (Sys.opaque_identity n)) [0; 1; 4]
