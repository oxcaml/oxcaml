(* TEST
 readonly_files = "dep_a.ml dep_b.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto -flambda2-result-types-all-functions";
 compile_only = "true";
 all_modules = "dep_a.ml";
 ocamlopt.opt;

 all_modules = "dep_b.ml";
 ocamlopt.opt;

 all_modules = "multi_module_rebuild.ml";
 ocamlopt.opt;

 file = "dep_a.cmr";
 file-exists;

 file = "dep_b.cmr";
 file-exists;

 file = "multi_module_rebuild.cmr";
 file-exists;

 compile_only = "false";
 flags = "-reaper-solve dep_a.cmr dep_b.cmr multi_module_rebuild.cmr";
 last_flags = "-o multi_module_rebuild.ltosol";
 all_modules = "";
 ocamlopt.opt;

 file = "multi_module_rebuild.ltosol";
 file-exists;

 last_flags = "-o multi_module_rebuild.second.ltosol";
 ocamlopt.opt;

 script = "cmp multi_module_rebuild.ltosol multi_module_rebuild.second.ltosol";
 script;

 flags = "-reaper-rebuild dep_a.cmr multi_module_rebuild.ltosol";
 last_flags = "-o dep_a.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild dep_b.cmr multi_module_rebuild.ltosol";
 last_flags = "-o dep_b.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild multi_module_rebuild.cmr multi_module_rebuild.ltosol";
 last_flags = "-o multi_module_rebuild.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-solve dep_a.cmr";
 last_flags = "-o dep_a_only.ltosol";
 ocamlopt.opt;

 flags = "-reaper-rebuild dep_a.cmr dep_a_only.ltosol";
 last_flags = "-o dep_a.solo.cmx";
 ocamlopt.opt;

 script = "cmp dep_a.reaped.o dep_a.solo.o";
 script;

 flags = "";
 last_flags = "";
 program = "${test_build_directory}/multi_module_rebuild.exe";
 all_modules = "dep_a.reaped.cmx dep_b.reaped.cmx multi_module_rebuild.reaped.cmx";
 ocamlopt.opt;

 run;
 check-program-output;
*)

(* End-to-end test of the split Reaper flow on a multi-module program: compile
   each module with -support-lto, run one whole-program solve (twice, checking
   the output is deterministic), rebuild each unit from the shared .ltosol,
   then link the .reaped.cmx files and run the executable.

   Additionally, dep_a (which depends on no other unit in the set) is solved
   and rebuilt again on its own, and the result compared with the rebuild from
   the whole-program solution: adding unrelated units to the solve must not
   perturb a unit's rebuilt code.

   The program exercises values crossing unit boundaries: direct calls into
   dep_a, a closure created in dep_a whose body is inlined into dep_b (so
   dep_b projects dep_a's value slots), and a unit-local closure with a dead
   value slot (kept, with a poisoned value, so the layout never changes). *)

let () =
  let a = Dep_a.read (Dep_a.make 20) in
  let b = Dep_b.add_via_a 4 (Sys.opaque_identity 18) in
  Printf.printf "%d\n" (a + b);
  Printf.printf "%d\n" (Dep_a.sum_with_dead_capture 3);
  Printf.printf "%d\n" (Dep_b.apply_adder 5 10)
