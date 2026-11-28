(* TEST
 set misspelled = "${test_source_directory}/misspelled_codegen_variant";
 set unavailable = "${test_source_directory}/unavailable_codegen_variant";
 script = "sh ${test_source_directory}/child-fails.sh ${tsl} ${ocamlsrcdir}/ocamltest/ocamltest ${reason}";
 stdout = "child-fails.output";
 stderr = "child-fails.output";
 {
   set tsl = "${misspelled}.tsl";
   set reason = "${misspelled}.reason";
   script;
 }{
   set tsl = "${misspelled}_script.tsl";
   set reason = "${misspelled}.reason";
   script;
 }{
   set tsl = "${misspelled}_predicate.tsl";
   set reason = "${misspelled}.reason";
   script;
 }{
   set tsl = "${unavailable}.tsl";
   set reason = "${unavailable}.reason";
   script;
 }{
   script = "sh ${test_source_directory}/codegen-variant-selection.sh ${ocamlsrcdir}/ocamltest/ocamltest";
   script;
 }{
   codegen_variant = "+fp-sc";
   variant_available;
   frame_pointers;
   no-stack-checks;
   script = "true";
   script;
 }
*)

(* A value of codegen_variant that is neither the name of a variant nor
   requirements is an error of the test, wherever the selection is read:
   variant_available fails on it, where it skips for a variant that the
   compiler under test does not have, and so does a block that has only a
   script, or predicates that the misspelled value would otherwise
   satisfy.

   A block that selects a variant that the compiler under test does not
   have, and that variant_available does not guard, fails as well: its
   script would run for another variant.

   Requirements are signed axes, as in the warning options of the
   compilers: the blanks between them are optional.  The script checks how
   values with and without blanks are read, that a value without a leading
   sign is a name whatever hyphens it has, and what the error says for the
   values that are neither requirements nor a name.  The last block
   runs where the compiler under test has a variant with frame pointers
   and without stack checks. *)
