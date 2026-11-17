(* TEST
 codegen_variant = "fp";
 variant_available;
 codegen_variant = "base";
 variant_available;
 readonly_files = "archive_mismatch_unit.ml";
 setup-ocamlopt.byte-build-env;
 codegen_variant = "fp";
 module = "archive_mismatch_unit.ml";
 ocamlopt.byte;
 module = "";
 flags = "-a";
 program = "archive_mismatch_unit.cmxa";
 all_modules = "archive_mismatch_unit.cmx";
 ocamlopt.byte;
 codegen_variant = "base";
 flags = "";
 program = "${test_build_directory}/archive_mismatch.exe";
 all_modules = "archive_mismatch_unit.cmxa archive_mismatch.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* An archive that was built for another codegen variant is refused with
   the error that names it and the two configurations, like a .cmx file.
   This program does not use the unit of the archive, so the link would
   only check what the archive imports, and its unit imports the standard
   library of the variant it was compiled for: without the check of the
   fingerprints of the archive's units, the link fails on the CRC of an
   implementation of the standard library. *)

let () = print_endline "should not link"
