(* TEST
 arch_arm64;
 function_sections;
 setup-ocamlopt.byte-build-env;
 ocamlopt_flags = "-verify-binary-emitter -function-sections";
 module = "long_section_names.ml";
 ocamlopt.byte;
*)

(* Regression test for long section names with [-verify-binary-emitter].

   With function sections, the code of each function goes in a section
   [.text.caml.<symbol>], which the binary emitter saves in the
   [<prefix>.binary-sections] directory as
   [section_text.caml.<symbol>.bin] and [.relocs]. The symbol of the
   function below is long enough for those file names to exceed the
   255-byte limit on file names, so they must be saved under a hash of the
   section name instead (with the name itself in a [.name] file), and the
   verifier must read them back under the original section name. *)

let[@inline never] callee x = x * 3

(* The call to [callee] puts a relocation in the long-named section. *)
let[@inline never] a_function_with_a_name_long_enough_that_the_file_names_the_binary_emitter_saves_for_its_function_section_go_over_the_255_byte_limit_on_file_names_unless_they_are_hashed_which_is_what_this_test_checks_for_the_verify_binary_emitter_flag_on_arm64_systems x = callee x + 1

let g x = a_function_with_a_name_long_enough_that_the_file_names_the_binary_emitter_saves_for_its_function_section_go_over_the_255_byte_limit_on_file_names_unless_they_are_hashed_which_is_what_this_test_checks_for_the_verify_binary_emitter_flag_on_arm64_systems (x * 2)
