(* TEST
 codegen_variant = "base";
 variant_available;
 shared-libraries;
 setup-ocamlopt.byte-build-env;
 flags = "-shared";
 all_modules = "codegen_fingerprint.ml";
 program = "codegen_fingerprint.cmxs";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 program = "-no-code -no-approx codegen_fingerprint.cmx";
 ocamlobjinfo;
 check-program-output;
*)

(* Checks that ocamlobjinfo prints the codegen fingerprint recorded in
   native artifacts.  The reference shows every axis, so the test names
   its variant, and runs wherever the compiler under test has that
   variant; question.ml covers the rest of the output for every variant. *)
let answer () = 42
