(* TEST
 set OCAMLLIB = "${test_build_directory}/root";
 tool = "sh ${test_source_directory}/cached_generic_functions_root.sh";
 codegen_variant = "+fp";
 variant_available;
 codegen_variant = "-fp";
 variant_available;
 setup-ocamlopt.byte-build-env;
 script = "${tool} root ${ocamlsrcdir}/stdlib root";
 script;
 compiler = "${ocamlsrcdir} ${ocamlrun} ${ocamlopt_byte}";
 script = "${tool} links ${compiler}";
 script;
 check-program-output;
 script = "${tool} direct ${stdlib} ${compiler}";
 script;
 codegen_variant = "+fp";
 setup-ocamlopt.byte-build-env;
 script = "${tool} links ${compiler}";
 script;
 check-program-output;
 script = "${tool} direct ${stdlib} ${compiler}";
 script;
 output = "direct.output";
 reference =
   "${test_source_directory}/cached_generic_functions_root.direct.reference";
 check-program-output;
 output = "roots.output";
 reference =
   "${test_source_directory}/cached_generic_functions_root.roots.reference";
 check-program-output;
*)

(* The archive of cached generic functions of a codegen variant is the file
   cached-generic-functions.a of the variant's library tree, and an
   installation does not necessarily have it. This test works in a library
   root of its own, made of links to the installation's, and does the same
   under two variants, of which at most one is base:

   - Without the archive, a link with -use-cached-generic-functions fails
     with an error that gives the command that generates it. That command,
     run as it is printed, writes the archive, and the same link then
     succeeds.
   - The archive may also be named, with -cached-generic-functions-path. A
     build system that names the one in the library root, as it names the
     libraries there, gets the one of the selected variant's tree: the
     archive of the root itself is that of base, which the linker refuses
     for any other variant.

   The command of the error names the variant, and is run in an environment
   whose OCAMLPARAM selects the other frame-pointer setting, and the LLVM
   backend for ordinary compilations: the generator generates the archive
   of the variant that it is given by name whatever OCAMLPARAM selects, and
   always emits the archive itself.

   The library root may also be the tree of a variant itself, where the
   archive is the root's own. The first point is tried again with a copy of
   the tree of whichever of the two variants is not base as the root: the
   command of the error names the archive where the linker then finds it.

   The error gives no command where there is no place for the archive of
   the selected variant: when the installation has no library tree for the
   variant, and when the library root that stands for that tree is built
   for another variant, whose archive the command would write over. Nor
   does it for a missing archive that is named: the error says which files
   were looked up. For base itself, a root is built for another variant
   when the unit info of its standard library says so: one without that
   file, as an installation that only links with -output-obj may be, is
   used. For base with poll insertion, which the root also stands for,
   that file has to be read and have the variant's other settings: its
   poll bit is not compared. *)

let apply3 f x y z = (Sys.opaque_identity f) x y z

let add3 x y z = x + y + z

let () = Printf.printf "%d\n%!" (apply3 add3 1 2 3)
