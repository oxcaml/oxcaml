(* TEST
 no-frame_pointers;
 no-address-sanitizer;
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
   native artifacts.  Gated to configurations where the default variant
   is known (no frame pointers, no address sanitizer); question.ml
   covers the rest of the output on all configurations. *)
let answer () = 42
