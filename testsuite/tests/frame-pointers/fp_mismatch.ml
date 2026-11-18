(* TEST
 codegen_variant = "fp";
 variant_available;
 codegen_variant = "base";
 variant_available;
 readonly_files = "fp_unit.ml";
 setup-ocamlopt.byte-build-env;
 codegen_variant = "fp";
 module = "fp_unit.ml";
 ocamlopt.byte;
 codegen_variant = "base";
 module = "";
 binary_modules = "fp_unit";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Linking a unit compiled with -frame-pointers into a -no-frame-pointers
   link must be rejected with a clear codegen-configuration error. *)

let () = print_endline "should not link"
