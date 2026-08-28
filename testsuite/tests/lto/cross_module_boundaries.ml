(* TEST
 readonly_files = "boundary_dep.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 all_modules = "boundary_dep.ml";
 ocamlopt.opt;

 all_modules = "cross_module_boundaries.ml";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve boundary_dep.cmr cross_module_boundaries.cmr";
 last_flags = "-o cross_module_boundaries.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild boundary_dep.cmr cross_module_boundaries.ltosol";
 last_flags = "-o boundary_dep.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild cross_module_boundaries.cmr cross_module_boundaries.ltosol";
 last_flags = "-o cross_module_boundaries.reaped.cmx";
 ocamlopt.opt;

 flags = "";
 last_flags = "";
 program = "${test_build_directory}/cross_module_boundaries.exe";
 all_modules = "boundary_dep.reaped.cmx cross_module_boundaries.reaped.cmx";
 ocamlopt.opt;

 run;
 check-program-output;
*)

(* Exercises whole-program Reaper behaviour across unit boundaries: projecting
   and calling closures defined in another unit, exceptions defined and raised
   in another unit, passing closures to units outside the participating set
   (the stdlib), and garbage collection while the dependency's module block has
   poisoned (dead) fields. *)

let () =
  let f, g = Boundary_dep.adder_pair in
  Printf.printf "%d\n" (f (Sys.opaque_identity 1) + g 2);
  (try Boundary_dep.raise_custom 5
   with Boundary_dep.Custom n -> Printf.printf "caught %d\n" n);
  let doubled = List.map (fun x -> x * 2) Boundary_dep.used_list in
  Gc.full_major ();
  print_endline (String.concat " " (List.map string_of_int doubled))
