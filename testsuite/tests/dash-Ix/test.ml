(* TEST
(* This tests the -Ix flag.

   -Ix behaves like -I for module resolution, but additionally marks the
   directory as one where cmx files are guaranteed to be available.  For now,
   these tests verify that -Ix works identically to -I for visibility and
   ordering purposes.

   The structure mirrors testsuite/tests/hidden_includes: libb depends on liba,
   and libc depends on libb (and transitively on liba).  liba_alt provides an
   alternate, incompatible version of module A.

   We test:
   1. -Ix works for basic module resolution
   2. Multiple -Ix flags work together
   3. -I and -Ix can be mixed, with earlier flags taking priority in terms of
      which version of a module is seen.
*)

subdirectories = "liba liba_alt libb libc";
setup-ocamlopt.opt-build-env;

(* Compile both versions of liba *)
flags = "-I liba -nocwd";
module = "liba/a.ml";
ocamlopt.opt;

flags = "-I liba_alt -nocwd";
module = "liba_alt/a.ml";
ocamlopt.opt;

(* Compile libb against liba *)
flags = "-I liba -I libb -nocwd";
module = "libb/b.ml";
ocamlopt.opt;

{
  (* Test: Basic -Ix works for transitive dependency resolution.
     -Ix liba makes A's cmi visible, so B's dependency on A is satisfied. *)
  flags = "-Ix liba -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlopt.opt-build-env;
  ocamlopt.opt;
}
{
  (* Test: -Ix works for direct module references. *)
  flags = "-Ix liba -nocwd";
  module = "libc/c2.ml";
  setup-ocamlopt.opt-build-env;
  ocamlopt.opt;
}
{
  (* Test: Multiple -Ix flags work together.
     Both liba and libb are provided via -Ix. *)
  flags = "-Ix liba -Ix libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlopt.opt-build-env;
  ocamlopt.opt;
}

(* Ordering of -I and -Ix determines which version of A is seen, just like
   ordering of -I flags.  B was compiled against liba, so using liba_alt for A
   causes inconsistent assumptions. *)
{
  split [
  | (* Test: -Ix liba before -I liba_alt: liba wins, compiles fine. *)
    flags = "-Ix liba -I liba_alt -I libb -nocwd";
  | (* Test: -I liba before -Ix liba_alt: liba wins, compiles fine. *)
    flags = "-I liba -Ix liba_alt -I libb -nocwd";
  ]
  module = "libc/c1.ml";
  setup-ocamlopt.opt-build-env;
  ocamlopt.opt;
}
{
  not-windows;
  split [
  | (* Test: -Ix liba_alt before -I liba: liba_alt wins, inconsistent. *)
    flags = "-Ix liba_alt -I liba -I libb -nocwd";
  | (* Test: -I liba_alt before -Ix liba: liba_alt wins, inconsistent. *)
    flags = "-I liba_alt -Ix liba -I libb -nocwd";
  ]
  module = "libc/c1.ml";
  setup-ocamlopt.opt-build-env;
  ocamlopt_opt_exit_status = "2";
  ocamlopt.opt;
  compiler_reference2 =
    "${test_source_directory}/wrong_include_order.ocamlc.reference";
  check-ocamlopt.opt-output;
}

(* Ordering among multiple -Ix flags *)
{
  (* -Ix liba before -Ix liba_alt: liba wins, compiles fine. *)
  flags = "-Ix liba -Ix liba_alt -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlopt.opt-build-env;
  ocamlopt.opt;
}
{
  (* -Ix liba_alt before -Ix liba: liba_alt wins, inconsistent. *)
  not-windows;
  flags = "-Ix liba_alt -Ix liba -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlopt.opt-build-env;
  ocamlopt_opt_exit_status = "2";
  ocamlopt.opt;
  compiler_reference2 =
    "${test_source_directory}/wrong_include_order.ocamlc.reference";
  check-ocamlopt.opt-output;
}

*)
