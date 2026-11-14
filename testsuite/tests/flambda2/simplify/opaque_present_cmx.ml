(* TEST
   compile_only = "true";
   flambda2;
   readonly_files = "opaque_present_cmx_a.mli opaque_present_cmx_a.ml \
     opaque_present_cmx_b.mli opaque_present_cmx_b.ml \
     opaque_present_cmx.cmx.objinfo.reference";
   setup-ocamlopt.byte-build-env;
   { all_modules = "opaque_present_cmx_a.mli";
     ocamlopt_flags += " -opaque";
     ocamlopt.byte;
   }{
     all_modules = "opaque_present_cmx_a.ml opaque_present_cmx_b.mli \
       opaque_present_cmx_b.ml";
     ocamlopt.byte;
   }{
     all_modules = "opaque_present_cmx.ml";
     ocamlopt.byte;
     check-ocamlopt.byte-output;
   }{
     program = "-no-code -no-approx opaque_present_cmx.cmx";
     output = "opaque_present_cmx.cmx.objinfo.output";
     ocamlobjinfo;

     reference = "opaque_present_cmx.cmx.objinfo.reference";
     check-program-output;
   }
 *)

(* Companion to opaque_missing_cmx.ml (regression test for oxcaml/oxcaml#7524),
   covering the case where the .cmx file of the opaque-interfaced unit exists.

   [Opaque_present_cmx_a] has an interface compiled with [-opaque] and an
   implementation, so opaque_present_cmx_a.cmx exists. When this file is
   compiled, the only reference to [Opaque_present_cmx_a] comes from
   [opaque_present_cmx_b.cmx] (its interface does not mention it), so
   [Env.is_imported_opaque] does not know that it is opaque. Flambda 2 must
   nevertheless ignore opaque_present_cmx_a.cmx, which it determines from the
   opacity flag recorded in that file. The ocamlobjinfo check below verifies
   that this unit's .cmx records no CRC for [Opaque_present_cmx_a] (printed as
   dashes), i.e. that the .cmx was not consumed. *)

let x = Opaque_present_cmx_b.f ()
