(* TEST
(* This tests -persistent-deps.

   libc depends on libb, which depends on liba. With the flag, b.cmi records
   where the a.cmi it was compiled against is, as a path relative to the
   manifest files root ($MANIFEST_FILES_ROOT), provided a.cmi was found
   through a manifest (-I-manifest or -H-manifest) rather than a directory.
   Users of b.cmi then resolve its references to A through the recorded
   location rather than their load path, without checking visibility; a
   recorded location that cannot be loaded is an error. Direct references to
   A in user code still go through the load path.

   The liba_alt directory has an alternate version of liba used for testing
   that the recorded location prevails over the load path.
*)

subdirectories = "liba liba_alt libb libc manifests";
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
  (* With the flag, a cmi found through a directory has no location to
     record. *)
  flags = "-persistent-deps -I liba -I libb -nocwd";
  module = "libb/b.ml";
  ocamlc.byte;
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
    flags = "-I libb -nocwd";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
  }
  {
    (* The recorded location prevails over whatever version of A the load
       path offers. *)
    split [
    | flags = "-I liba_alt -I libb -nocwd";
    | flags = "-H liba_alt -I libb -nocwd";
    | flags = "-I-manifest manifests/liba_alt.txt -I libb -nocwd";
    | flags = "-H-manifest manifests/liba_alt.txt -I libb -nocwd";
    ]
    module = "libc/c1.ml";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
  }
  {
    (* Direct references to A still go through the load path. *)
    split [
    | flags = "-I libb -nocwd";
    | flags = "-H liba -I libb -nocwd";
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
    flags = "-I libb -nocwd";
    module = "libc/c4.ml";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
  }
  (* A recorded location that cannot be loaded is an error, even when the
     load path could provide the cmi. *)
  {
    MANIFEST_FILES_ROOT = "nowhere";
    flags = "-I liba -I libb -nocwd";
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
    flags = "-I liba -I libb -nocwd";
    module = "libc/c1.ml";
    setup-ocamlc.byte-build-env;
    ocamlc_byte_exit_status = "2";
    ocamlc.byte;
    compiler_reference =
      "${test_source_directory}/recorded_location_no_root.ocamlc.reference";
    check-ocamlc.byte-output;
  }
}

*)
