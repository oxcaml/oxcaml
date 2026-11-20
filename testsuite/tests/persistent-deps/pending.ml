(* TEST
(* This tests the consistency checks left pending by -persistent-deps.

   Y (an interface only) depends on Z, and X depends on Y. X is compiled
   with -persistent-deps, finding y.cmi through a manifest: x.cmi then
   records Y with its CRC and location, but not Y's imports, whose
   consistency is left to be checked by the linker, which loads y.cmi from
   the recorded location under -use-persistent-deps.

   M uses X and a version of Z, from libz or libz_alt.
*)

subdirectories = "libx liby libz libz_alt manifests";
set MANIFEST_FILES_ROOT = ".";
setup-ocamlc.byte-build-env;

flags = "-I libz -nocwd";
module = "libz/z.ml";
ocamlc.byte;

flags = "-I libz_alt -nocwd";
module = "libz_alt/z.ml";
ocamlc.byte;

flags = "-I libz -I liby -nocwd";
module = "liby/y.mli";
ocamlc.byte;

{
  (* Without the flag, x.cmi imports Z through Y, so M cannot use another
     version of Z. *)
  flags = "-I-manifest manifests/liby.txt -I libx -nocwd";
  module = "libx/x.ml";
  ocamlc.byte;
  program = "-null-crc libx/x.cmi";
  ocamlobjinfo;
  reference =
    "${test_source_directory}/pending_objinfo_without_flag.reference";
  check-program-output;
  flags = "-I libx -I libz_alt -nocwd";
  module = "libx/m.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/pending_inconsistent_compile.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  flags = "-persistent-deps -I-manifest manifests/liby.txt -I libx -nocwd";
  module = "libx/x.ml";
  ocamlc.byte;
  {
    (* With the flag, x.cmi imports Y with its location, and not Z. *)
    program = "-null-crc libx/x.cmi";
    output = "x.objinfo.output";
    ocamlobjinfo;
    reference = "${test_source_directory}/pending_objinfo.reference";
    check-program-output;
  }
  {
    (* Y's imports are checked at link time. *)
    flags = "-I libx -I libz -nocwd";
    module = "libx/m.ml";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
    module = "";
    flags = "-use-persistent-deps";
    program = "./m.byte";
    all_modules = "libz/z.cmo libx/x.cmo libx/m.cmo";
    ocamlc.byte;
    output = "m.output";
    run;
    reference = "${test_source_directory}/pending_run.reference";
    check-program-output;
  }
  {
    (* Which requires -use-persistent-deps. *)
    flags = "-I libx -I libz -nocwd";
    module = "libx/m.ml";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
    module = "";
    flags = "";
    program = "./m.byte";
    all_modules = "libz/z.cmo libx/x.cmo libx/m.cmo";
    ocamlc_byte_exit_status = "2";
    ocamlc.byte;
    compiler_reference =
      "${test_source_directory}/pending_without_flag.ocamlc.reference";
    check-ocamlc.byte-output;
  }
  {
    (* M compiles against another version of Z, since X does not import Z;
       the linker then finds that y.cmi disagrees with M over Z. *)
    flags = "-I libx -I libz_alt -nocwd";
    module = "libx/m.ml";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
    module = "";
    flags = "-use-persistent-deps";
    program = "./m.byte";
    all_modules = "libz_alt/z.cmo libx/x.cmo libx/m.cmo";
    ocamlc_byte_exit_status = "2";
    ocamlc.byte;
    compiler_reference =
      "${test_source_directory}/pending_inconsistent_link.ocamlc.reference";
    check-ocamlc.byte-output;
  }
  {
    (* A recorded location that cannot be loaded is an error. *)
    flags = "-I libx -I libz -nocwd";
    module = "libx/m.ml";
    setup-ocamlc.byte-build-env;
    ocamlc.byte;
    MANIFEST_FILES_ROOT = "nowhere";
    module = "";
    flags = "-use-persistent-deps";
    program = "./m.byte";
    all_modules = "libz/z.cmo libx/x.cmo libx/m.cmo";
    ocamlc_byte_exit_status = "2";
    ocamlc.byte;
    compiler_reference =
      "${test_source_directory}/pending_location_missing.ocamlc.reference";
    check-ocamlc.byte-output;
  }
  {
    (* The same, linking natively. *)
    native-compiler;
    setup-ocamlopt.byte-build-env;
    flags = "-I libz -nocwd";
    module = "libz/z.ml";
    ocamlopt.byte;
    flags = "-I libz_alt -nocwd";
    module = "libz_alt/z.ml";
    ocamlopt.byte;
    flags = "-I libz -I liby -nocwd";
    module = "liby/y.mli";
    ocamlopt.byte;
    flags = "-persistent-deps -I-manifest manifests/liby.txt -I libx -nocwd";
    module = "libx/x.ml";
    ocamlopt.byte;
    flags = "-I libx -I libz_alt -nocwd";
    module = "libx/m.ml";
    ocamlopt.byte;
    module = "";
    flags = "-use-persistent-deps";
    program = "./m.exe";
    all_modules = "libz_alt/z.cmx libx/x.cmx libx/m.cmx";
    compiler_output = "native_link.output";
    ocamlopt_byte_exit_status = "2";
    ocamlopt.byte;
    compiler_reference =
      "${test_source_directory}/pending_inconsistent_link.ocamlopt.reference";
    check-ocamlopt.byte-output;
  }
}
*)
