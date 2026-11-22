(* TEST
 codegen_variant = "+fp";
 variant_available;
 script = "sh ${test_source_directory}/nested-ocamltest.sh ${ocamlsrcdir}/ocamltest/ocamltest";
 stdout = "nested-ocamltest.output";
 stderr = "nested-ocamltest.output";
 script;
*)

(* An ocamltest that a test starts runs its own tests for their own
   variants, whatever the variant of the test that started it: that one
   reaches it as defaults in OCAMLPARAM, which are not for its tests. *)
