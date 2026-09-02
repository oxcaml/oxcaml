(* TEST
 readonly_files = "di_dep_a.ml di_dep_b.ml di_dep_c.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 all_modules = "di_dep_a.ml";
 ocamlopt.opt;

 all_modules = "di_dep_b.ml";
 ocamlopt.opt;

 all_modules = "di_dep_c.ml";
 ocamlopt.opt;

 all_modules = "diamond.ml";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve di_dep_a.cmr di_dep_b.cmr di_dep_c.cmr diamond.cmr";
 last_flags = "-o diamond.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild di_dep_a.cmr diamond.ltosol";
 last_flags = "-o di_dep_a.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild di_dep_b.cmr diamond.ltosol";
 last_flags = "-o di_dep_b.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild di_dep_c.cmr diamond.ltosol";
 last_flags = "-o di_dep_c.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild diamond.cmr diamond.ltosol";
 last_flags = "-o diamond.reaped.cmx";
 ocamlopt.opt;

 flags = "";
 last_flags = "";
 program = "${test_build_directory}/diamond.exe";
 all_modules = "di_dep_a.reaped.cmx di_dep_b.reaped.cmx di_dep_c.reaped.cmx diamond.reaped.cmx";
 ocamlopt.opt;

 run;
 check-program-output;
*)

(* Diamond dependency graph: this module depends on [Di_dep_b] and
   [Di_dep_c], which both depend on [Di_dep_a]. Rebuilds run in a valid
   topological order (a, b, c, main). *)

let () = Printf.printf "%d %d\n" (Di_dep_b.left 4) (Di_dep_c.right 4)
