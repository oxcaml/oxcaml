(* TEST
 readonly_files = "a1.ml a2.ml b.ml c_sub1.ml c_sub2.ml c_inter.ml";
 setup-ocamlc.byte-build-env;
 module = "a1.ml";
 ocamlc.byte;
 module = "a2.ml";
 ocamlc.byte;
 module = "b.ml";
 ocamlc.byte;

 (* This checks that our various [c.ml]s compile with the relevant cmi in
    place *)
 module = "c_sub1.ml";
 ocamlc.byte;
 module = "c_sub2.ml";
 ocamlc.byte;
 module = "c_inter.ml";
 ocamlc.byte;

 (* Remove the cmi for [a], and the artifacts for the [c*]s *)
 script = "rm -f a1.cmi a1.cmo a2.cmi a2.cmo \
                   c_sub1.cmi c_sub1.cmo \
                   c_sub2.cmi c_sub2.cmo \
                   c_inter.cmi c_inter.cmo";
 script;

 (* Test a subkind check that fails due to a kind defined in a missing cmi. *)
 module = "c_sub1.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/test_sub1.missing.reference";
 check-ocamlc.byte-output;

 (* Test a subkind check that fails due to a two missing kinds from the same
    cmi. *)
 setup-ocamlc.byte-build-env;
 module = "c_sub2.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/test_sub2.missing.reference";
 check-ocamlc.byte-output;

 (* Test an intersection that fails due to two different kinds from two
    different cmis. *)
 setup-ocamlc.byte-build-env;
 module = "c_inter.ml";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "${test_source_directory}/test_inter.missing.reference";
 check-ocamlc.byte-output;
*)

(* Note this tests also checks that serialization of kinds works as expected,
   as we check that things work before deleting a. *)
