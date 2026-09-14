(* TEST
 flambda2;
 setup-ocamlopt.opt-build-env;
 ocamlrunparam = "b=0";

 flags = "-Oclassic -flambda2-reaper -support-lto";
 compile_only = "true";
 ocamlopt.opt;

 file = "support_lto_overrides_oclassic.cmr";
 file-exists;

 compile_only = "false";
 flags = "-reaper-solve support_lto_overrides_oclassic.cmr";
 last_flags = "-o support_lto_overrides_oclassic.ltosol";
 all_modules = "";
 ocamlopt.opt;

 flags = "-reaper-rebuild support_lto_overrides_oclassic.cmr support_lto_overrides_oclassic.ltosol";
 last_flags = "";
 all_modules = "";
 ocamlopt.opt;

 file = "support_lto_overrides_oclassic.reaped.cmx";
 file-exists;
*)

(* Classic mode does not run Simplify and hence cannot produce the .cmr files
   needed for LTO, so -support-lto must override -Oclassic. Without the
   override, the compilation below would not emit a .cmr file. *)

let[@inline never] f x = x + 1

let () = ignore (Sys.opaque_identity (f 3) : int)
