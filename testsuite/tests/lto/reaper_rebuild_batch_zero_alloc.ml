(* TEST
 modules = "reaper_rebuild_batch_zero_alloc_dep.ml";
 flambda2;
 setup-ocamlopt.opt-build-env;

 flags = "-flambda2-reaper -support-lto -O3";
 compile_only = "true";
 ocamlopt.opt;

 compile_only = "false";
 flags = "-reaper-solve reaper_rebuild_batch_zero_alloc_dep.cmr reaper_rebuild_batch_zero_alloc.cmr";
 last_flags = "-o reaper_rebuild_batch_zero_alloc.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild reaper_rebuild_batch_zero_alloc_dep.cmr reaper_rebuild_batch_zero_alloc.cmr reaper_rebuild_batch_zero_alloc.ltosol";
 last_flags = "";
 ocamlopt.opt;

 file = "reaper_rebuild_batch_zero_alloc.reaped.cmx";
 file-exists;

 check-ocamlopt.opt-output;
*)

(* The zero_alloc check on [check] needs Stdlib's zero_alloc info (to know that
   [failwith] diverges). This unit is a later member of the batch, whose
   rebuild does not itself read Stdlib's cmx because the first member already
   loaded it into the batch's shared caches, so the info must survive the
   per-member [Compilenv.reset]. *)

let[@inline never] [@zero_alloc] check x =
  if x < 0 then (failwith [@inlined never]) "neg" else x + 1

let () =
  print_int (check 1);
  Reaper_rebuild_batch_zero_alloc_dep.g ()
