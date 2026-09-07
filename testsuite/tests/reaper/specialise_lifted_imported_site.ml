(* TEST
   modules = "specialise_lifted_imported_site_lib.ml";
   flambda2;
   setup-ocamlopt.opt-build-env;
   flags = "-O4 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
   module = "specialise_lifted_imported_site_lib.ml";
   ocamlopt.opt;
   flags = "-O3 -no-flambda2-reaper -dflambda-invariants";
   module = "";
   all_modules = "specialise_lifted_imported_site.ml";
   binary_modules = "specialise_lifted_imported_site_lib";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 *)

[@@@ocaml.flambda_o3]

(* The site already exists in the producer's .cmx. Either entry selection must
   keep a valid layout when the other sibling's code becomes dead. *)
let[@inline never] first callback n =
  Specialise_lifted_imported_site_lib.run false callback n

let[@inline never] second callback n =
  Specialise_lifted_imported_site_lib.run true callback n

let () =
  List.iter
    (fun n ->
      let callback x = x + 10 in
      Printf.printf "%d: %d %d\n" n (first callback n) (second callback n))
    [0; 1; 4]
