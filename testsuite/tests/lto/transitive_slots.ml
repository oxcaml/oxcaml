(* TEST
 readonly_files = "tr_dep_a.ml tr_dep_b.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 all_modules = "tr_dep_a.ml";
 ocamlopt.opt;

 all_modules = "tr_dep_b.ml";
 ocamlopt.opt;

 all_modules = "transitive_slots.ml";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve tr_dep_a.cmr tr_dep_b.cmr transitive_slots.cmr";
 last_flags = "-o transitive_slots.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild tr_dep_a.cmr transitive_slots.ltosol";
 last_flags = "-o tr_dep_a.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild tr_dep_b.cmr transitive_slots.ltosol";
 last_flags = "-o tr_dep_b.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild transitive_slots.cmr transitive_slots.ltosol";
 last_flags = "-o transitive_slots.reaped.cmx";
 ocamlopt.opt;

 flags = "";
 last_flags = "";
 program = "${test_build_directory}/transitive_slots.exe";
 all_modules = "tr_dep_a.reaped.cmx tr_dep_b.reaped.cmx transitive_slots.reaped.cmx";
 ocamlopt.opt;

 run;
 check-program-output;
*)

(* This module only mentions [Tr_dep_b], but inlining can compile projections
   of [Tr_dep_a]'s value slots into this module's code, so this module's
   rebuild needs [Tr_dep_a]'s reaped metadata even though the dependency is
   indirect. *)

let () =
  let f = Tr_dep_b.get () in
  Printf.printf "%d\n" (f 6)
