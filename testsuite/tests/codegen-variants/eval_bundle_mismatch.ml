(* TEST
 include eval;
 helper = "eval_bundle_helper";
 readonly_files = "${helper}.ml";
 codegen_variant = "+fp";
 variant_available;
 codegen_variant = "-fp";
 variant_available;
 setup-ocamlopt.byte-build-env;
 script = "mkdir -p linked other";
 script;
 extension = "-extension runtime_metaprogramming";
 module = "${helper}.ml";
 flags = "${extension} -o linked/${helper}.cmx";
 ocamlopt.byte;
 codegen_variant = "+fp";
 flags = "${extension} -o other/${helper}.cmx";
 ocamlopt.byte;
 codegen_variant = "-fp";
 module = "eval_bundle_mismatch.ml";
 flags = "${extension} -I linked";
 ocamlopt.byte;
 module = "";
 all_modules = "linked/${helper}.cmx eval_bundle_mismatch.cmx";
 flags = "${extension} -I linked -dstartup";
 program = "${test_build_directory}/bundle.exe";
 ocamlopt.byte;
 script = "sh ${test_source_directory}/link_guard.sh refers \
   bundle.exe.bundled_cm.${asmext}";
 output = "bundle.output";
 script;
 reference = "${test_source_directory}/eval_bundle_mismatch.bundle.reference";
 check-program-output;
 flags = "${extension} -I other -I linked";
 program = "${test_build_directory}/mismatch.exe";
 compiler_output = "compiler-output.raw";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 script = "sh ${test_source_directory}/link_guard.sh variant-error \
   compiler-output.raw";
 output = "refusal.output";
 script;
 reference = "${test_source_directory}/eval_bundle_mismatch.reference";
 check-program-output;
*)

#syntax quotations on

(* A program that uses eval carries the .cmx files of the units that its
   quotations mention, and the linker finds those by name on the search
   path. A .cmx of that name compiled for another codegen variant, found
   before the one that is linked, is refused like any other file of another
   variant: the code that eval compiles would otherwise take what it inlines
   from it.

   The bundle is an object file that the compiler generates, and refers to
   the link-time guard of the codegen variant like the others: the first
   link keeps its assembly file to show it. *)

let () =
  let quoted : bool =
    Eval.eval <[ Eval_bundle_helper.compiled_with_frame_pointers () ]>
  in
  Printf.printf "linked: %b, quoted: %b\n"
    (Eval_bundle_helper.compiled_with_frame_pointers ())
    quoted
