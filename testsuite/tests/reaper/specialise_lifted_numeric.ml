(* TEST
   modules = "specialise_lifted_numeric_lib.ml specialise_lifted_numeric_mid.ml";
   flambda2;
   setup-ocamlopt.opt-build-env;
   all_modules = "specialise_lifted_numeric.ml";
   binary_modules = "specialise_lifted_numeric_lib specialise_lifted_numeric_mid";
   flags = "-O4 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
   module = "specialise_lifted_numeric_lib.ml";
   ocamlopt.opt;
   {
     flags = "-O3 -no-flambda2-reaper -dflambda-invariants";
     module = "specialise_lifted_numeric_mid.ml";
     ocamlopt.opt;
     module = "";
     {
       ocamlopt.opt;
       check-ocamlopt.opt-output;
       run;
       check-program-output;
     }{
       flags += " -g -flambda2-expert-phantom-lets";
       ocamlopt.opt;
       check-ocamlopt.opt-output;
       run;
       check-program-output;
     }
   }{
     module = "specialise_lifted_numeric_mid.ml";
     ocamlopt.opt;
     module = "";
     ocamlopt.opt;
     check-ocamlopt.opt-output;
     run;
     check-program-output;
   }
 *)

[@@@ocaml.flambda_o3]

module Lib = Specialise_lifted_numeric_lib
module Mid = Specialise_lifted_numeric_mid

(* Different callbacks and numeric constants must not share assumptions. *)
let[@zero_alloc] captured n =
  let a =
    Mid.captured
      (fun x y -> Lib.float_to_int x + Lib.int64_to_int y) #2.5 #9L n
  in
  let b =
    Mid.captured
      (fun x y -> Lib.float_to_int x - Lib.int64_to_int y) #7.5 #3L n
  in
  #(a, b)

let[@zero_alloc] boxed n =
  Mid.boxed
    (fun x y -> Lib.float_to_int x + Lib.int64_to_int y) 2.5 9L n

let () =
  List.iter
    (fun n ->
      let n = Sys.opaque_identity n in
      let #(a, b) = captured n in
      Printf.printf "%d: %d %d %d\n" n a b (boxed n))
    [0; 1; 4]
