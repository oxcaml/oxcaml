(* TEST
(* This tests the -H flag and attached cmi paths.

   The basic structure is that libc depends on libb, which depends on liba.  We
   want to test a few things:

   - b.cmi records the path of the a.cmi it was compiled against, so compiling
     libc resolves transitive references to A through that path, whether or not
     any version of liba is on the include path and regardless of -I/-H order.

   - Compiling libc with -I liba allows c.ml to reference A directly.

   - Compiling libc with -H liba (or with A reachable only through b.cmi's
     attached path) does not allow c.ml to reference A directly.

   The liba_alt directory has an alternate version of liba used for testing
   that the attached path prevails for transitive references.
*)

subdirectories = "liba liba_alt libb libc";
setup-ocamlc.byte-build-env;

flags = "-I liba -nocwd";
module = "liba/a.ml";
ocamlc.byte;

flags = "-I liba_alt -nocwd";
module = "liba_alt/a.ml";
ocamlc.byte;

flags = "-I liba -I libb -nocwd";
module = "libb/b.ml";
ocamlc.byte;

flags = "-nocwd";
module = "libb/with_sub.ml";
ocamlc.byte;
{
  (* Test using values whose types come from A with no liba on the include
     path at all: a.cmi is found through the path attached in b.cmi. *)
  flags = "-I libb -nocwd";
  module = "libc/c2.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  flags = "-I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
(* Test transitive use of A's cmi, both with -I and with -H. *)
{
  split [
  | flags = "-I liba -I libb -nocwd";
  | flags = "-H liba -I libb -nocwd";
  ]
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  (* Test direct use of A cmi with -H. *)
  flags = "-H liba -I libb -nocwd";
  module = "libc/c3.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/cant_reference_hidden.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* The next tests check that transitive references to A resolve through the
   path attached in b.cmi - the one b was compiled against - regardless of
   which alternate versions of liba appear on the include path, in any -I/-H
   combination and order. (Direct references still go through the include
   path: see the c3 and c4 tests.) *)
{
  split [
  | flags = "-H liba_alt -I liba -I libb -nocwd";
  | flags = "-I liba -H liba_alt -I libb -nocwd";
  | flags = "-H liba -I liba_alt -I libb -nocwd";
  | flags = "-I liba_alt -H liba -I libb -nocwd";
  | flags = "-H liba_alt -H liba -I libb -nocwd";
  | flags = "-H liba -H liba_alt -I libb -nocwd";
  ]
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

(* Test that a hidden `A` doesn't become visible as a result of the typechecker
   using it. *)
{
  flags = "-H liba -I libb -nocwd";
  module = "libc/c4.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/hidden_stays_hidden.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Test that a hidden `A` doesn't become visible to -no-alias-deps checking
   just because the typechecker loaded it earlier. *)
{
  flags = "-H liba -I libb -no-alias-deps -nocwd";
  module = "libc/c6.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/alias_after_hidden_load.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Test that type-directed constructor disambiguation works through -H (at
   least, for now). *)
{
  flags = "-H liba -I libb -nocwd";
  module = "libc/c5.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

(* Test that [-open-cmi] reads the cmi from the given path without
   consulting the include path, and that it works alongside -H. *)
{
  split [
  | flags = "-nocwd -open-cmi liba/a.cmi";
  | flags = "-H liba -I libb -nocwd -open-cmi liba/a.cmi";
  ]
  module = "libb/b_open.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

(* Test that [-open-cmi] of a hidden module does not make user-code
   references to that module legal. *)
{
  flags = "-H liba -I libb -nocwd -open-cmi liba/a.cmi";
  module = "libc/c3.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/cant_reference_hidden.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Test that an [-open] following an earlier [-open-cmi] can refer to a
   module brought into scope by it: command-line order is preserved. *)
{
  flags = "-nocwd -open-cmi libb/with_sub.cmi -open A";
  module = "libb/uses_float.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

(* Test that [-open-cmi] loads the cmi at the given path and ignores any
   in-scope module of the same name: the sub-module [A] brought into scope
   by [-open-cmi libb/with_sub.cmi] does not shadow the subsequent
   [-open-cmi liba/a.cmi]. *)
{
  flags = "-nocwd -open-cmi libb/with_sub.cmi -open-cmi liba/a.cmi";
  module = "libb/uses_int.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

(* Test that a trailing [-open-cmi] overrides an earlier [-open]:
   command-line order is preserved across the two flag kinds. *)
{
  flags = "-I liba -nocwd -open A -open-cmi libb/with_sub.cmi";
  module = "libb/uses_string.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

*)
