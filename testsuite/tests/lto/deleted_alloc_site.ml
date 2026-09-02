(* TEST
 readonly_files = "ds_dep.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto -O3";
 compile_only = "true";
 all_modules = "ds_dep.ml";
 ocamlopt.opt;

 all_modules = "deleted_alloc_site.ml";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve ds_dep.cmr deleted_alloc_site.cmr";
 last_flags = "-o deleted_alloc_site.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild ds_dep.cmr deleted_alloc_site.ltosol";
 last_flags = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild deleted_alloc_site.cmr deleted_alloc_site.ltosol";
 ocamlopt.opt;

 flags = "";
 last_flags = "";
 program = "${test_build_directory}/deleted_alloc_site.exe";
 all_modules = "ds_dep.reaped.cmx deleted_alloc_site.reaped.cmx";
 ocamlopt.opt;

 run;
 check-program-output;
*)

(* Regression test: the whole-program Reaper deletes ds_dep's own copy of the
   set of closures allocated inside [Ds_dep.make] (its code is dead once every
   call has been inlined), while this unit's inlined copy of the allocation
   survives. Rebuilding this unit then needs offsets for ds_dep's slots, which
   ds_dep's rebuild can no longer assign from its own sets; they must come from
   the pass-1 assignment stored in ds_dep's .cmr and be re-exported live from
   its reaped .cmx. *)

let () =
  let f = Ds_dep.make (Sys.opaque_identity 41) in
  Printf.printf "%d\n" (f ())
