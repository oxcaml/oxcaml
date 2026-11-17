(* TEST
 codegen_variant = "sc";
 variant_available;
 codegen_variant = "base";
 variant_available;
 readonly_files = "sc_unit.ml";
 setup-ocamlopt.byte-build-env;
 codegen_variant = "sc";
 module = "sc_unit.ml";
 ocamlopt.byte;
 codegen_variant = "base";
 module = "";
 binary_modules = "sc_unit";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Linking a unit compiled with -stack-checks into a -no-stack-checks link
   must be rejected with a clear codegen-configuration error. *)

let () = print_endline "should not link"
