(* TEST
 readonly_files = "cc_bytecode_.c";
 flags = "-custom cc_bytecode_.${objext}";
 {
   codegen_variant = "+asan";
   variant_available;
   setup-ocamlc.byte-build-env;
   modules = "cc_bytecode_.c";
   arguments = "-c";
   program = "cc_bytecode_.${objext}";
   cc;
   arguments = "";
   program = "${test_build_directory}/cc_bytecode.byte";
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
 }{
   codegen_variant = "+asan";
   variant_available;
   setup-ocaml-build-env;
   modules = "cc_bytecode_.c";
   arguments = "-c";
   program = "cc_bytecode_.${objext}";
   cc;
   arguments = "";
   program = "${test_build_directory}/cc_bytecode.byte";
   ocamlc.byte;
   check-ocamlc.byte-output;
   run;
 }{
 (* No compiler has AddressSanitizer and stack checks together, and
    nothing here guards the block: the cc action and the bytecode
    compiler do not need the variant. *)
   codegen_variant = "+asan +sc";
   compiler_directory_suffix = ".unavailable";
   setup-ocamlc.byte-build-env;
   modules = "cc_bytecode_.c";
   arguments = "-c";
   program = "cc_bytecode_.${objext}";
   cc;
   arguments = "";
   program = "${test_build_directory}/cc_bytecode.byte";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }
*)

(* In a block set up for a bytecode compiler or for the bytecode toplevel,
   the cc action compiles for the bytecode runtime, which is the default
   variant's, whatever variant is selected: an object instrumented for
   AddressSanitizer would not link with that runtime.  It has no use for
   the selected variant there, so it does not fail either where the
   compiler under test lacks that variant. *)

external answer : unit -> int = "cc_bytecode_answer"

let () = assert (answer () = 42)
