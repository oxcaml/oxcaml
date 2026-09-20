(* TEST
 flags = "-extension layout_poly -nocwd -Ix . -w -58";
 readonly_files = "static_only_intf.mli no_cmx_for_static.reference";
 setup-ocamlopt.byte-build-env;
 module = "static_only_intf.mli";
 ocamlopt.byte;
 {
   (* Producing only the .cmi stops before slambda eval and so does not need
      the static data of static dependencies. *)
   flags += " -stop-after tlambda";
   module = "no_cmx_for_static.ml";
   ocamlopt.byte;
 }{
   (* A full compilation does need it, and reports its absence. *)
   module = "no_cmx_for_static.ml";
   compiler_output = "no_cmx_for_static.output";
   ocamlopt_byte_exit_status = "2";
   ocamlopt.byte;
   compiler_reference = "no_cmx_for_static.reference";
   check-ocamlopt.byte-output;
 }
*)

(* [-Ix .] declares that every .cmi in the directory has a .cmx, so
   [Static_only_intf.f] is static and this instantiation typechecks; evaluating
   it then finds no .cmx. *)
let _ = Static_only_intf.f 1
