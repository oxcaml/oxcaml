(* TEST
 set OCAMLLIB = "${ocamlsrcdir}/stdlib";
 set MANIFEST_FILES_ROOT = "/";
 script = "sh ${test_source_directory}/manifest_library.sh \
   ${ocamlsrcdir}/stdlib";
 libraries = "unix";
 flags = "-I-manifest ${test_build_directory}/unix.manifest -noautolink \
   -cclib ${ocamlsrcdir}/stdlib/unix/libunix_stubs.a";
 {
   setup-ocamlopt.opt-build-env;
   script;
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 }{
   codegen_variant = "base";
   variant_available;
   compiler_directory_suffix = ".base";
   setup-ocamlopt.opt-build-env;
   script;
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 }{
   codegen_variant = "fp";
   variant_available;
   compiler_directory_suffix = ".fp";
   setup-ocamlopt.opt-build-env;
   script;
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 }{
   codegen_variant = "asan-fp";
   variant_available;
   compiler_directory_suffix = ".asan-fp";
   setup-ocamlopt.opt-build-env;
   script;
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 }
*)

(* A build system that gives the compiler manifests instead of include
   directories names the files of the installation's libraries by their
   paths inside the default library root, as others do with -I, whatever
   the codegen variant.  The manifest here, which manifest_library.sh
   writes, lists the interface, the .cmx, the .cmxa and the archive of the
   unix library in the root that OCAMLLIB names; the compiler has to use
   the copies in the tree of the variant that it compiles for, and the
   default tree's copies are refused under every variant with a
   fingerprint of its own.  The C stubs are named by their path in the
   root too, with -cclib.  The first block is the variant of the test run;
   the others each select one. *)

let () =
  Unix.handle_unix_error
    (fun () -> Printf.printf "pid > 0: %b\n" (Unix.getpid () > 0))
    ()
