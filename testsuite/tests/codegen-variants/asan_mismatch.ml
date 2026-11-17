(* TEST
 codegen_variant = "asan-fp";
 variant_available;
 codegen_variant = "fp";
 variant_available;
 readonly_files = "asan_unit.ml";
 setup-ocamlopt.byte-build-env;
 codegen_variant = "asan-fp";
 module = "asan_unit.ml";
 ocamlopt.byte;
 codegen_variant = "fp";
 module = "";
 binary_modules = "asan_unit";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Linking a unit compiled with -asan into a -no-asan link must be rejected
   with a clear codegen-configuration error: the instrumented unit
   references the caml_asan_report_* wrappers and shadow machinery that
   only an asan runtime and an asan link provide. *)

let () = print_endline (Asan_unit.greeting : string)
