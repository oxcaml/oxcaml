(* TEST
(* This tests the -Hx and -open-cmi-x flags.

   -Hx behaves like -H for module resolution, but additionally marks the
   directory as one where cmx files are guaranteed to be available, like -Ix.
   Similarly, -open-cmi-x behaves like -open-cmi, but marks the opened module's
   cmx as guaranteed.

   libc contains a static, layout-polymorphic module C.  libb contains two
   modules that depend on C: B, which calls [C.id], and B_alias, which
   aliases C.

   1. transitive.ml instantiates [B.id], which transitively needs C's static
      data.
   2. alias.ml instantiates [B_alias.M.id].  Because of the alias, this looks
      up C itself, so C must be static, which requires a guaranteed cmx.
   3. open_cmi.ml instantiates [id] from C opened with -open-cmi-x, which
      succeeds, or with -open-cmi, which fails because C is forced dynamic.
*)

subdirectories = "libb libc";
readonly_files = "transitive.ml alias.ml open_cmi.ml";
setup-ocamlopt.byte-build-env;

flags = "-extension layout_poly_alpha -Ix libc -nocwd";
module = "libc/c.mli";
ocamlopt.byte;
module = "libc/c.ml";
ocamlopt.byte;

flags = "-extension layout_poly_alpha -Ix libc -Ix libb -nocwd";
module = "libb/b.mli";
ocamlopt.byte;
module = "libb/b.ml";
ocamlopt.byte;
module = "libb/b_alias.mli";
ocamlopt.byte;
module = "libb/b_alias.ml";
ocamlopt.byte;

(* The paths passed to -H-file and -Hx-file are relative to this. *)
set MANIFEST_FILES_ROOT = "${test_build_directory}";

{
  flags = "-extension layout_poly_alpha -Ix libb -Hx libc -nocwd";
  module = "transitive.ml";
  setup-ocamlopt.byte-build-env;
  ocamlopt.byte;
}
{
  (* It's okay if C doesn't have guaranteed cmx; we checked that it did when
     B was compiled. *)
  flags = "-extension layout_poly_alpha -Ix libb -H libc -nocwd";
  module = "transitive.ml";
  setup-ocamlopt.byte-build-env;
  ocamlopt.byte;
}
{
  flags = "-extension layout_poly_alpha -Ix libb -Hx libc -nocwd";
  module = "alias.ml";
  setup-ocamlopt.byte-build-env;
  ocamlopt.byte;
}
{
  (* The module alias causes us to look up C. Its cmx needs to be guaranteed. *)
  flags = "-extension layout_poly_alpha -Ix libb -H libc -nocwd";
  module = "alias.ml";
  setup-ocamlopt.byte-build-env;
  ocamlopt_byte_exit_status = "2";
  ocamlopt.byte;
  compiler_reference = "${test_source_directory}/alias.ocamlopt.reference";
  check-ocamlopt.byte-output;
}
{
  (* When the same directory is passed to both -Hx and -H, the earlier flag
     determines whether its cmx files are guaranteed. *)
  flags = "-extension layout_poly_alpha -Ix libb -Hx libc -H libc -nocwd";
  module = "alias.ml";
  setup-ocamlopt.byte-build-env;
  ocamlopt.byte;
}
{
  flags = "-extension layout_poly_alpha -Ix libb -H libc -Hx libc -nocwd";
  module = "alias.ml";
  setup-ocamlopt.byte-build-env;
  ocamlopt_byte_exit_status = "2";
  ocamlopt.byte;
  compiler_reference = "${test_source_directory}/alias.ocamlopt.reference";
  check-ocamlopt.byte-output;
}
{
  (* -Hx-file propagates cmx presence just like -Hx. *)
  flags = "-extension layout_poly_alpha -Ix libb -nocwd \
           -Hx-file c.cmi:libc/c.cmi -Hx-file c.cmx:libc/c.cmx";
  module = "alias.ml";
  setup-ocamlopt.byte-build-env;
  ocamlopt.byte;
}
{
  (* -H-file, like -H, does not guarantee C's cmx. *)
  flags = "-extension layout_poly_alpha -Ix libb -nocwd \
           -H-file c.cmi:libc/c.cmi -H-file c.cmx:libc/c.cmx";
  module = "alias.ml";
  setup-ocamlopt.byte-build-env;
  ocamlopt_byte_exit_status = "2";
  ocamlopt.byte;
  compiler_reference = "${test_source_directory}/alias.ocamlopt.reference";
  check-ocamlopt.byte-output;
}
{
  flags =
    "-extension layout_poly_alpha -H libc -nocwd -open-cmi-x libc/c.cmi";
  module = "open_cmi.ml";
  setup-ocamlopt.byte-build-env;
  ocamlopt.byte;
}
{
  flags = "-extension layout_poly_alpha -H libc -nocwd -open-cmi libc/c.cmi";
  module = "open_cmi.ml";
  setup-ocamlopt.byte-build-env;
  ocamlopt_byte_exit_status = "2";
  ocamlopt.byte;
  compiler_reference = "${test_source_directory}/open_cmi.ocamlopt.reference";
  check-ocamlopt.byte-output;
}
*)
