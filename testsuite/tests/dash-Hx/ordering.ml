(* TEST
(* This tests command-line handling of the -Hx flag.

   -Hx behaves like -H for module resolution: modules in -Hx directories can
   be used transitively but not referenced directly, -I and -Ix directories
   take priority over -H and -Hx directories regardless of command-line order,
   and among -H and -Hx directories the earlier flag takes priority.

   libd depends on liba.  liba_alt provides an alternate, incompatible version
   of module A, so seeing it when compiling against D causes inconsistent
   assumptions.
*)

subdirectories = "liba liba_alt libd";
readonly_files = "uses_d.ml uses_a.ml";
setup-ocamlc.byte-build-env;

flags = "-I liba -nocwd";
module = "liba/a.ml";
ocamlc.byte;

flags = "-I liba_alt -nocwd";
module = "liba_alt/a.ml";
ocamlc.byte;

flags = "-I liba -I libd -nocwd";
module = "libd/d.ml";
ocamlc.byte;

{
  (* -Hx works for transitive dependency resolution. *)
  flags = "-Hx liba -I libd -nocwd";
  module = "uses_d.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  (* Modules in -Hx directories can't be referenced directly. *)
  flags = "-Hx liba -I libd -nocwd";
  module = "uses_a.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/cant_reference_hidden.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* -I and -Ix take priority over -Hx regardless of command-line order. *)
{
  split [
  | flags = "-Hx liba_alt -I liba -I libd -nocwd";
  | flags = "-I liba -Hx liba_alt -I libd -nocwd";
  | flags = "-Hx liba_alt -Ix liba -I libd -nocwd";
  | flags = "-Ix liba -Hx liba_alt -I libd -nocwd";
  ]
  module = "uses_d.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  not-target-windows;
  split [
  | flags = "-Hx liba -I liba_alt -I libd -nocwd";
  | flags = "-I liba_alt -Hx liba -I libd -nocwd";
  | flags = "-Hx liba -Ix liba_alt -I libd -nocwd";
  | flags = "-Ix liba_alt -Hx liba -I libd -nocwd";
  ]
  module = "uses_d.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/wrong_include_order.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Among -H and -Hx, earlier flags take priority over later ones. *)
{
  split [
  | flags = "-Hx liba -H liba_alt -I libd -nocwd";
  | flags = "-H liba -Hx liba_alt -I libd -nocwd";
  | flags = "-Hx liba -Hx liba_alt -I libd -nocwd";
  ]
  module = "uses_d.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  not-target-windows;
  split [
  | flags = "-Hx liba_alt -H liba -I libd -nocwd";
  | flags = "-H liba_alt -Hx liba -I libd -nocwd";
  | flags = "-Hx liba_alt -Hx liba -I libd -nocwd";
  ]
  module = "uses_d.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/wrong_include_order.ocamlc.reference";
  check-ocamlc.byte-output;
}
*)
