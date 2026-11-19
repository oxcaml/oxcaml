(* TEST
 flambda2;
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-extension layouts_beta";
 compile_only = "true";
 ocamlopt_byte_exit_status = "2";
 {
   arch_amd64;
   compiler_reference = "${test_source_directory}/unsupported_too_large_return.amd64.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
 {
   arch_arm64;
   compiler_reference = "${test_source_directory}/unsupported_too_large_return.arm64.reference";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }
*)

(* A 24-byte struct return is returned in memory on both ABIs. *)
external f : unit -> #(int64_u * int64_u * int64_u)
  = "" "unsupported_too_large_return_stub"

let g () = f ()
