(* TEST
   modules = "specialise_lifted_nested_captures_lib.ml specialise_lifted_nested_captures_mid.ml";
   flambda2;
   flags += "-O4 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
   setup-ocamlopt.opt-build-env;
   module = "specialise_lifted_nested_captures_lib.ml";
   ocamlopt.opt;
   module = "specialise_lifted_nested_captures_mid.ml";
   ocamlopt.opt;
   module = "";
   all_modules = "specialise_lifted_nested_captures.ml";
   binary_modules = "specialise_lifted_nested_captures_lib specialise_lifted_nested_captures_mid";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 *)

[@@@ocaml.flambda_o3]

module Lib = Specialise_lifted_nested_captures_mid

let square x = x * x
let double x = 2 * x

(* Each pair instantiates the same code twice with different assumptions. *)
let[@zero_alloc] values n =
  let first = Lib.values square double n in
  let second = Lib.values double square n in
  #(first, second)

let[@zero_alloc] closures n =
  let first = Lib.closures square double n in
  let second = Lib.closures double square n in
  #(first, second)

let[@zero_alloc] nested n =
  let first = Lib.nested square double n in
  let second = Lib.nested double square n in
  #(first, second)

let[@zero_alloc] backedge n =
  let first = Lib.backedge square n in
  let second = Lib.backedge double n in
  #(first, second)

let () =
  List.iter
    (fun n ->
      let n = Sys.opaque_identity n in
      let #(v1, v2) = values n in
      let #(c1, c2) = closures n in
      let #(n1, n2) = nested n in
      let #(b1, b2) = backedge n in
      Printf.printf "%d: %d %d; %d %d; %d %d; %d %d\n"
        n v1 v2 c1 c2 n1 n2 b1 b2)
    [0; 1; 2; 4]
