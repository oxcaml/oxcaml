(* TEST
   modules = "specialise_lifted_imported_infix_lib.ml";
   flambda2;
   setup-ocamlopt.opt-build-env;
   flags = "-O3 -no-flambda2-reaper -dflambda-invariants";
   module = "specialise_lifted_imported_infix_lib.ml";
   ocamlopt.opt;
   flags = "-O4 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
   module = "";
   all_modules = "specialise_lifted_imported_infix.ml";
   binary_modules = "specialise_lifted_imported_infix_lib";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 *)

[@@@ocaml.flambda_o3]

(* Specialisation removes [second]. Unknown [bias] prevents static lifting;
   reaper builds a single-function specialisation site inside [run], which
   must not reuse [first]'s imported offset of four words. *)
let[@inline never][@zero_alloc] run bias n =
  let first, _ =
    Specialise_lifted_imported_infix_lib.make false (fun x -> x + bias)
  in
  first n 1

let () = Printf.printf "%d %d\n" (run 10 2) (run (-3) 7)
