(* TEST
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 readonly_files = "link_guard_unit.ml";
 tool = "sh ${test_source_directory}/root_diagnostics.sh";
 codegen_variant = "+fp";
 variant_available;
 codegen_variant = "-fp";
 variant_available;
 setup-ocamlopt.byte-build-env;
 script = "${tool} ${stdlib} ${ocamlrun} ${ocamlopt_byte}";
 script;
 codegen_variant = "+fp";
 setup-ocamlopt.byte-build-env;
 script = "${tool} ${stdlib} ${ocamlrun} ${ocamlopt_byte}";
 script;
 output = "diagnostics.output";
 check-program-output;
*)

(* What the errors of a link say about the installation, for each of two
   variants that has other frame-pointer, stack-check or AddressSanitizer
   settings than the default one (at least one of the two has):

   - With the tree of the variant itself as the library root (OCAMLLIB), a
     unit compiled for the other variant is refused for what it was
     compiled for. The root has no tree for the variant under variants/,
     but it serves the variant: the error does not say that the root lacks
     it.
   - With a root that has no tree for the variant and is not one, the same
     error does say so.
   - With -runtime-variant and a runtime library that only exists for the
     default variant, what the error reports as missing is the file of
     that runtime library for the selected variant, and not the variant,
     which the installation provides. *)

let () = print_endline "not run"
