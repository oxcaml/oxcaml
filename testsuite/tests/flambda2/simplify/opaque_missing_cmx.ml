(* TEST
   compile_only = "true";
   flambda2;
   readonly_files = "opaque_missing_cmx_a.mli opaque_missing_cmx_b.mli \
     opaque_missing_cmx_b.ml";
   setup-ocamlopt.byte-build-env;
   { all_modules = "opaque_missing_cmx_a.mli";
     ocamlopt_flags += " -opaque";
     ocamlopt.byte;
   }{
     all_modules = "opaque_missing_cmx_b.mli opaque_missing_cmx_b.ml";
     ocamlopt.byte;
   }{
     all_modules = "opaque_missing_cmx.ml";
     ocamlopt.byte;
     check-ocamlopt.byte-output;
   }
 *)

(* Regression test for oxcaml/oxcaml#7524.

   [Opaque_missing_cmx_a] has an interface compiled with [-opaque] and no
   implementation, so no .cmx file exists for it. [Opaque_missing_cmx_b.f]
   mentions it, but [Opaque_missing_cmx_b]'s interface does not, so when this
   file is compiled the only reference to [Opaque_missing_cmx_a] comes from
   [opaque_missing_cmx_b.cmx], and [Persistent_env] never reads
   [opaque_missing_cmx_a.cmi]. Simplifying the call to [f] below makes
   Flambda 2 try to load [opaque_missing_cmx_a.cmx]; we check that this does
   not emit a spurious warning 58 (the interface was compiled with [-opaque],
   which [Compilenv] now determines from the .cmi when the interface was not
   imported during compilation). *)

let x = Opaque_missing_cmx_b.f ()
