(* TEST
 readonly_files = "stub_archive_path_lib.ml";
 script = "sh ${test_source_directory}/stub_archive_path.sh \
   ${ocamlsrcdir}/stdlib ${codegen_variant}";
 {
   codegen_variant = "base";
   variant_available;
   compiler_directory_suffix = ".base";
   setup-ocamlopt.opt-build-env;
   script;
   set OCAMLLIB = "${test_build_directory}/root";
   flags = "-a ${test_build_directory}/root/probe/librecorded.a";
   all_modules = "stub_archive_path_lib.ml";
   program = "stub_archive_path_lib.cmxa";
   ocamlopt.opt;
   flags = "${test_build_directory}/root/probe/libprobe.a \
     ${test_build_directory}/root/probe/probe.o \
     -cclib ${test_build_directory}/root/probe/libcclib.a \
     ${test_build_directory}/root/probe/libonly.a";
   all_modules = "stub_archive_path_lib.cmxa stub_archive_path.ml";
   program = "${test_build_directory}/stub_archive_path.exe";
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
   set OCAMLLIB = "${test_build_directory}/root";
   flags = "-a ${test_build_directory}/root/probe/librecorded.a";
   all_modules = "stub_archive_path_lib.ml";
   program = "stub_archive_path_lib.cmxa";
   ocamlopt.opt;
   flags = "${test_build_directory}/root/probe/libprobe.a \
     ${test_build_directory}/root/probe/probe.o \
     -cclib ${test_build_directory}/root/probe/libcclib.a \
     ${test_build_directory}/root/probe/libonly.a";
   all_modules = "stub_archive_path_lib.cmxa stub_archive_path.ml";
   program = "${test_build_directory}/stub_archive_path.exe";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
   run;
   check-program-output;
 }
*)

(* A C object or archive that a link is given by a path inside the default
   library root is taken from the tree of the variant being compiled, like
   the .cmxa files that build systems name in the same way: nothing would
   refuse the default tree's copy, which has no fingerprint.  The root is
   one that stub_archive_path.sh makes, OCAMLLIB names it, and each function
   below says which tree its definition comes from.  One block compiles for
   the default variant, whose tree is the root itself, whatever the
   configure default.

   One archive is the argument of -cclib, and the one of
   [Stub_archive_path_lib] is recorded in its .cmxa, by the path given to
   ocamlopt -a.  The last archive exists in the default tree only and has
   to be linked from there: a variant with a runtime of its own has no
   counterpart for the default tree's runtime archive either. *)

external probe_archive : unit -> (int[@untagged])
  = "probe_archive_bytecode" "probe_archive"
[@@noalloc]

external probe_object : unit -> (int[@untagged])
  = "probe_object_bytecode" "probe_object"
[@@noalloc]

external probe_cclib : unit -> (int[@untagged])
  = "probe_cclib_bytecode" "probe_cclib"
[@@noalloc]

external probe_default_tree_only : unit -> (int[@untagged])
  = "probe_default_tree_only_bytecode" "probe_default_tree_only"
[@@noalloc]

let report description probe =
  Printf.printf "%s: %s\n" description
    (if probe () = 1 then "the variant's tree" else "another tree")

let () =
  report "An archive" probe_archive;
  report "An object" probe_object;
  report "An archive given with -cclib" probe_cclib;
  report "An archive recorded in a .cmxa" Stub_archive_path_lib.probe;
  Printf.printf "An archive of the default tree only: %s\n"
    (if probe_default_tree_only () = 1 then "linked" else "not linked")
