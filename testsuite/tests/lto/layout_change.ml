(* TEST
 readonly_files = "lc_dep_a.ml lc_dep_b.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto";
 compile_only = "true";
 all_modules = "lc_dep_a.ml";
 ocamlopt.opt;

 all_modules = "lc_dep_b.ml";
 ocamlopt.opt;

 all_modules = "layout_change.ml";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve lc_dep_a.cmr lc_dep_b.cmr layout_change.cmr";
 last_flags = "-o layout_change.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild lc_dep_a.cmr layout_change.ltosol";
 last_flags = "-o lc_dep_a.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild lc_dep_b.cmr layout_change.ltosol";
 last_flags = "-o lc_dep_b.reaped.cmx";
 ocamlopt.opt;

 flags = "-reaper-rebuild layout_change.cmr layout_change.ltosol";
 last_flags = "-o layout_change.reaped.cmx";
 ocamlopt.opt;

 flags = "";
 last_flags = "";
 program = "${test_build_directory}/layout_change.exe";
 all_modules = "lc_dep_a.reaped.cmx lc_dep_b.reaped.cmx layout_change.reaped.cmx";
 ocamlopt.opt;

 run;
 check-program-output;
*)

(* Checks that code compiled into this module which projects value slots from
   the set of closures defined in [Lc_dep_a] agrees with the layout of that
   set in [Lc_dep_a]'s rebuilt form. See lc_dep_a.ml. *)

let () =
  (* [used 5] takes the [x - y] branch; [used (-1)] takes [x + y]. *)
  Printf.printf "%d %d\n" (Lc_dep_b.via 5) (Lc_dep_a.used (-1))
