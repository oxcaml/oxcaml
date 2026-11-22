(* TEST
 codegen_variant = "+fp";
 variant_available;
 codegen_variant = "-fp";
 variant_available;
 readonly_files = "archive_unused_member_unit.ml";
 setup-ocamlopt.byte-build-env;
 codegen_variant = "+fp";
 module = "archive_unused_member_unit.ml";
 ocamlopt.byte;
 module = "";
 flags = "-a";
 program = "archive_unused_member_unit.cmxa";
 all_modules = "archive_unused_member_unit.cmx";
 ocamlopt.byte;
 codegen_variant = "-fp";
 flags = "";
 program = "${test_build_directory}/archive_unused_member.exe";
 all_modules = "archive_unused_member_unit.cmxa archive_unused_member.ml";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* The settings of a unit that a link leaves in its archive are not
   compared: this program does not use the unit of an archive compiled for
   another codegen variant, and links. What a link does check for an
   archive, whether or not it takes its units, is what they import: the
   unit of archive_mismatch.ml imports the implementation of units of the
   standard library, of the variant it was compiled for, and the same link
   is refused there. The unit of this archive imports no implementation. *)

let () = print_endline "linked without the unit of the archive"
