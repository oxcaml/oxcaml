(* TEST
 readonly_files = "cu_dep_a.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 all_modules = "cu_dep_a.ml";
 ocamlopt.opt;

 all_modules = "cross_unit_calls.ml";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve cu_dep_a.cmr cross_unit_calls.cmr";
 last_flags = "-o cross_unit_calls.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild cu_dep_a.cmr cross_unit_calls.ltosol";
 last_flags = "-o cu_dep_a.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild cross_unit_calls.cmr cross_unit_calls.ltosol";
 last_flags = "-o cross_unit_calls.reaped.cmx";
 ocamlopt.opt;

 flags = "";
 last_flags = "";
 program = "${test_build_directory}/cross_unit_calls.exe";
 all_modules = "cu_dep_a.reaped.cmx cross_unit_calls.reaped.cmx";
 ocamlopt.opt;

 run;
 check-program-output;
*)

(* Calling conventions across unit boundaries must be rewritten consistently:
   the callee's rebuild and the caller's rebuild each decide from the shared
   solution. See cu_dep_a.ml. *)

let () =
  Printf.printf "%d\n" (Cu_dep_a.sum_pair (Sys.opaque_identity (3, 4)));
  let a, b = Cu_dep_a.make_pair (Sys.opaque_identity 2) in
  Printf.printf "%d\n" (a + b)
