(* TEST
(* This tests -persistent-deps for compilation unit descriptions (.cmx).

   libc depends on libb, which depends on liba. With the flag, b.cmx records
   where the a.cmx it was compiled against is, as a path relative to the
   manifest files root ($MANIFEST_FILES_ROOT), provided a.cmx was found
   through a manifest (-I-manifest or -H-manifest) rather than a directory.
   Users of b.cmx whose optimisation needs a.cmx then load it from the
   recorded location rather than their load path; a recorded location that
   cannot be loaded is an error.

   c5.ml calls B.h, which calls A.f: inlining B.h needs A's description, and
   warning 58 reports that it could not be found.

   The liba_alt directory has an alternate version of liba used for testing
   that the recorded location prevails over the load path.
*)

subdirectories = "liba liba_alt libb libc manifests";
set MANIFEST_FILES_ROOT = ".";
setup-ocamlopt.byte-build-env;

flags = "-I liba -nocwd";
module = "liba/a.ml";
ocamlopt.byte;

flags = "-I liba_alt -nocwd";
module = "liba_alt/a.ml";
ocamlopt.byte;

{
  (* Without the flag, nothing is recorded, even for a cmx found through a
     manifest: users of B need A on their load path. *)
  flags = "-I-manifest manifests/liba.txt -I libb -nocwd";
  module = "libb/b.ml";
  ocamlopt.byte;
  {
    flags = "-I libb -nocwd";
    module = "libc/c5.ml";
    setup-ocamlopt.byte-build-env;
    ocamlopt.byte;
    compiler_reference =
      "${test_source_directory}/missing_cmx.ocamlopt.reference";
    check-ocamlopt.byte-output;
  }
}
{
  (* With the flag, a cmx found through a directory has no location to
     record. *)
  flags = "-persistent-deps -I liba -I libb -nocwd";
  module = "libb/b.ml";
  ocamlopt.byte;
  {
    flags = "-I libb -nocwd";
    module = "libc/c5.ml";
    setup-ocamlopt.byte-build-env;
    ocamlopt.byte;
    compiler_reference =
      "${test_source_directory}/missing_cmx.ocamlopt.reference";
    check-ocamlopt.byte-output;
  }
}
{
  (* With the flag, the location of a cmx found through a manifest is
     recorded. *)
  flags = "-persistent-deps -I-manifest manifests/liba.txt -I libb -nocwd";
  module = "libb/b.ml";
  ocamlopt.byte;
  {
    (* A's description is loaded through the recorded location, with no liba
       on the load path at all. *)
    flags = "-I libb -nocwd";
    module = "libc/c5.ml";
    setup-ocamlopt.byte-build-env;
    ocamlopt.byte;
    check-ocamlopt.byte-output;
  }
  {
    (* The recorded location prevails over whatever version of A the load
       path offers: linking against liba succeeds, which it would not if c5
       had been compiled against liba_alt's description. *)
    split [
    | flags = "-I liba_alt -I libb -nocwd";
    | flags = "-H liba_alt -I libb -nocwd";
    | flags = "-I-manifest manifests/liba_alt.txt -I libb -nocwd";
    | flags = "-H-manifest manifests/liba_alt.txt -I libb -nocwd";
    ]
    module = "libc/c5.ml";
    setup-ocamlopt.byte-build-env;
    ocamlopt.byte;
    check-ocamlopt.byte-output;

    flags = "";
    module = "";
    all_modules = "liba/a.cmx libb/b.cmx libc/c5.cmx";
    program = "${test_build_directory}/c5.exe";
    ocamlopt.byte;
  }
  (* A recorded location that cannot be loaded is an error, even when the
     load path could provide the cmx. *)
  {
    MANIFEST_FILES_ROOT = "nowhere";
    flags = "-I liba -I libb -nocwd";
    module = "libc/c5.ml";
    setup-ocamlopt.byte-build-env;
    ocamlopt_byte_exit_status = "2";
    ocamlopt.byte;
    compiler_reference =
      "${test_source_directory}/recorded_cmx_missing.ocamlopt.reference";
    check-ocamlopt.byte-output;
  }
  {
    unset MANIFEST_FILES_ROOT;
    flags = "-I liba -I libb -nocwd";
    module = "libc/c5.ml";
    setup-ocamlopt.byte-build-env;
    ocamlopt_byte_exit_status = "2";
    ocamlopt.byte;
    compiler_reference =
      "${test_source_directory}/recorded_cmx_no_root.ocamlopt.reference";
    check-ocamlopt.byte-output;
  }
}

*)
