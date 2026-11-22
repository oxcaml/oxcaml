(* TEST
 readonly_files = "cc_bytecode_.c";
 codegen_variant = "asan-fp";
 variant_available;
 setup-ocamlc.byte-build-env;
 modules = "cc_bytecode_.c";
 arguments = "-c";
 program = "cc_bytecode_.${objext}";
 cc;
 flags = "-custom cc_bytecode_.${objext}";
 arguments = "";
 program = "${test_build_directory}/cc_bytecode.byte";
 ocamlc.byte;
 check-ocamlc.byte-output;
 run;
*)

(* In a block set up for a bytecode compiler, the cc action compiles for
   the bytecode runtime, which is the default variant's, whatever variant
   is selected: an object instrumented for AddressSanitizer would not link
   with that runtime. *)

external answer : unit -> int = "cc_bytecode_answer"

let () = assert (answer () = 42)
