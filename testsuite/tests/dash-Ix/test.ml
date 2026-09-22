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
setup-ocamlc.byte-build-env;

(* Compile both versions of liba *)
flags = "-I liba -nocwd";
module = "liba/a.ml";
ocamlc.byte;

flags = "-I liba_alt -nocwd";
module = "liba_alt/a.ml";
ocamlc.byte;

(* Compile libb against liba *)
flags = "-I liba -I libb -nocwd";
module = "libb/b.ml";
ocamlc.byte;

{
  (* Test: Basic -Ix works for transitive dependency resolution.
     -Ix liba makes A's cmi visible, so B's dependency on A is satisfied. *)
  flags = "-Ix liba -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  (* Test: -Ix works for direct module references. *)
  flags = "-Ix liba -nocwd";
  module = "libc/c2.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  (* Test: Multiple -Ix flags work together.
     Both liba and libb are provided via -Ix. *)
  flags = "-Ix liba -Ix libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

(* B's transitive reference to A resolves through the cmi path attached in
   b.cmi - the a.cmi b was compiled against - so mixing -I and -Ix versions of
   liba in any order stays consistent. *)
{
  split [
  | flags = "-Ix liba -I liba_alt -I libb -nocwd";
  | flags = "-I liba -Ix liba_alt -I libb -nocwd";
  | flags = "-Ix liba_alt -I liba -I libb -nocwd";
  | flags = "-I liba_alt -Ix liba -I libb -nocwd";
  | flags = "-Ix liba -Ix liba_alt -I libb -nocwd";
  | flags = "-Ix liba_alt -Ix liba -I libb -nocwd";
  ]
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

*)
