(* TEST
(* This tests -persistent-deps and -use-persistent-deps.

   libc depends on libb, which depends on liba. With the flag, b.cmi records
   where the a.cmi it was compiled against is, as a path relative to the
   manifest files root ($MANIFEST_FILES_ROOT), provided a.cmi was found
   through a manifest rather than a directory.
   Users of b.cmi compiled with -use-persistent-deps then resolve its
   references to A through the recorded location rather than their load path,
   without checking visibility; a recorded location that cannot be loaded is
   an error. Direct references to A in user code still go through the load
   path. Users compiled without -use-persistent-deps disregard the recorded
   locations.

   The liba_alt directory has an alternate version of liba used for testing
   that the recorded location prevails over the load path.

   With the flag, the module alias in libd loads a.cmi even under
   -no-alias-deps, which still decides whether A is a runtime dependency
   of D, and records its location like any other mention of A.
*)

subdirectories = "liba liba_alt libb libc libd manifests";
set MANIFEST_FILES_ROOT = ".";
setup-ocamlc.byte-build-env;

flags = "-I liba -nocwd";
module = "liba/a.ml";
ocamlc.byte;

flags = "-I liba_alt -nocwd";
module = "liba_alt/a.ml";
ocamlc.byte;

{
  (* Without the flag, nothing is recorded, even for a cmi found through a
     manifest: users of B need A on their load path. *)
  flags = "-I-manifest manifests/liba.txt -I libb -nocwd";
  module = "libb/b.ml";
  ocamlc.byte;
  {
    flags = "-use-persistent-deps -I libb -nocwd";
    module = "libc/c2.ml";
    setup-ocamlc.byte-build-env;
    ocamlc_byte_exit_status = "2";
    ocamlc.byte;
    compiler_reference =
      "${test_source_directory}/missing_cmi_layout.ocamlc.reference";
    check-ocamlc.byte-output;
  }
}
{
  (* With the flag, a cmi found through a directory has no location to
     record. *)
  flags = "-persistent-deps -I liba -I libb -nocwd";
  module = "libb/b.ml";
  ocamlc.byte;
  {
    flags = "-use-persistent-deps -I libb -nocwd";
    module = "libc/c2.ml";
    setup-ocamlc.byte-build-env;
    ocamlc_byte_exit_status = "2";
    ocamlc.byte;
    compiler_reference =
      "${test_source_directory}/missing_cmi_layout.ocamlc.reference";
    check-ocamlc.byte-output;
  }
}
{
  (* With the flag, the location of a cmi found through a manifest is
     recorded. *)
  flags = "-persistent-deps -I-manifest manifests/liba.txt -I libb -nocwd";
  module = "libb/b.ml";
  ocamlc.byte;
  {
    (* Values whose types come from A can be used with no liba on the load
       path at all. *)
    split [
    | module = "libc/c2.ml";
    | module = "libc/c1.ml";
    ]
    flags = "-use-persistent-deps -I libb -nocwd";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
  }
  {
    (* The recorded location prevails over whatever version of A the load
       path offers. *)
    split [
    | flags = "-use-persistent-deps -I liba_alt -I libb -nocwd";
    | flags = "-use-persistent-deps -H liba_alt -I libb -nocwd";
    | flags = "-use-persistent-deps -I-manifest manifests/liba_alt.txt \
               -I libb -nocwd";
    | flags = "-use-persistent-deps -H-manifest manifests/liba_alt.txt \
               -I libb -nocwd";
    ]
    module = "libc/c1.ml";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
  }
  {
    (* Direct references to A still go through the load path. *)
    split [
    | flags = "-use-persistent-deps -I libb -nocwd";
    | flags = "-use-persistent-deps -H liba -I libb -nocwd";
    ]
    module = "libc/c3.ml";
    setup-ocamlc.byte-build-env;
    ocamlc_byte_exit_status = "2";
    ocamlc.byte;
    compiler_reference =
      "${test_source_directory}/cant_reference_hidden.ocamlc.reference";
    check-ocamlc.byte-output;
  }
  {
    (* A cmi loaded through a recorded location is not checked for
       visibility: once B's reference has loaded A, A can be referenced
       directly. *)
    flags = "-use-persistent-deps -I libb -nocwd";
    module = "libc/c4.ml";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
  }
  (* A recorded location that cannot be loaded is an error, even when the
     load path could provide the cmi. *)
  {
    MANIFEST_FILES_ROOT = "nowhere";
    flags = "-use-persistent-deps -I liba -I libb -nocwd";
    module = "libc/c1.ml";
    setup-ocamlc.byte-build-env;
    ocamlc_byte_exit_status = "2";
    ocamlc.byte;
    compiler_reference =
      "${test_source_directory}/recorded_location_missing.ocamlc.reference";
    check-ocamlc.byte-output;
  }
  {
    unset MANIFEST_FILES_ROOT;
    flags = "-use-persistent-deps -I liba -I libb -nocwd";
    module = "libc/c1.ml";
    setup-ocamlc.byte-build-env;
    ocamlc_byte_exit_status = "2";
    ocamlc.byte;
    compiler_reference =
      "${test_source_directory}/recorded_location_no_root.ocamlc.reference";
    check-ocamlc.byte-output;
  }
  (* Without -use-persistent-deps, recorded locations are ignored: users of B
     need A on their load path, but an unusable recorded location is not an
     error. *)
  {
    MANIFEST_FILES_ROOT = "nowhere";
    flags = "-I liba -I libb -nocwd";
    module = "libc/c1.ml";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
  }
  {
    flags = "-I libb -nocwd";
    module = "libc/c2.ml";
    setup-ocamlc.byte-build-env;
    ocamlc_byte_exit_status = "2";
    ocamlc.byte;
    compiler_reference =
      "${test_source_directory}/missing_cmi_layout.ocamlc.reference";
    check-ocamlc.byte-output;
  }
}

{
  (* With the flag, a module alias loads the cmi of its target, even under
     -no-alias-deps. *)
  flags = "-persistent-deps -no-alias-deps -nocwd";
  module = "libd/d.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/alias_missing_cmi.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  (* With the flag, the location is also recorded on the target of a module
     alias, which users of D resolve through it. *)
  split [
  | flags = "-persistent-deps -I-manifest manifests/liba.txt -nocwd";
  | flags = "-persistent-deps -no-alias-deps -I-manifest manifests/liba.txt \
             -nocwd";
  ]
  module = "libd/d.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  {
    split [
    | flags = "-use-persistent-deps -I libd -nocwd";
    | flags = "-use-persistent-deps -I liba_alt -I libd -nocwd";
    ]
    module = "libc/c5.ml";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
  }
}
{
  (* The flag does not affect runtime dependencies: with -no-alias-deps, a
     module alias does not make its target one, so D links without A. *)
  flags = "-persistent-deps -no-alias-deps -I liba -nocwd";
  module = "libd/d.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  module = "";
  flags = "";
  program = "./d.byte";
  all_modules = "libd/d.cmo";
  ocamlc.byte;
}
{
  (* Without -no-alias-deps, it does, with or without the flag. *)
  split [
  | flags = "-I liba -nocwd";
  | flags = "-persistent-deps -I liba -nocwd";
  ]
  module = "libd/d.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  module = "";
  flags = "";
  program = "./d.byte";
  all_modules = "libd/d.cmo";
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/alias_required_global.ocamlc.reference";
  check-ocamlc.byte-output;
}
*)
