(* TEST
(* This tests the -H flag.

   The basic structure is that libc depends on libb, which depends on liba.  We
   want to test a few things:

   - Compiling libc with -I liba allows the compiler to see the type definitions
     in liba and allows c.ml to reference it directly.

   - Compiling libc with -H liba allows the compiler to see the type definitions
     in liba, but doesn't allow c.ml to reference it directly.

   - If -H and -I are are passed for two different versions of liba, the -I one
     takes priority.

   - If -H is passed twice with two different versions of liba, the first takes
     priority.

   The liba_alt directory has an alternate versions of liba used for testing the
   precedence order of the includes.
*)

subdirectories = "liba liba_alt libb libc libd";
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

flags = "-I liba -no-alias-deps -w -49 -nocwd";
module = "libd/aliases.ml";
ocamlc.byte;

flags = "-I liba -no-alias-deps -w -49 -nocwd";
module = "libd/aliases_a.ml";
ocamlc.byte;
{
  (* Test hiding A completely. You can't do much with types from it because
     their layouts are unknown. *)
  flags = "-I libb -nocwd";
  module = "libc/c2.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/missing_cmi_layout.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  (* Test hiding A completely, but using it *)
  flags = "-I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference = "${test_source_directory}/not_included.ocamlc.reference";
  check-ocamlc.byte-output;
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

(* The next four tests check that -I takes priority over -H regardless of the
   order on the command line.
*)
{
  split [
  | flags = "-H liba_alt -I liba -I libb -nocwd";
  | flags = "-I liba -H liba_alt -I libb -nocwd";
  ]
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}
{
  not-target-windows;
  split [
  | flags = "-H liba -I liba_alt -I libb -nocwd";
  | flags = "-I liba_alt -H liba -I libb -nocwd";
  ]
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/wrong_include_order.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* The next two tests show that earlier -Hs take priority over later -Hs *)
{
  not-target-windows;
  flags = "-H liba_alt -H liba -I libb -nocwd";
  module = "libc/c1.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/wrong_include_order.ocamlc.reference";
  check-ocamlc.byte-output;
}
{
  flags = "-H liba -H liba_alt -I libb -nocwd";
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

(* Test that [-open-cmi] reads the cmi from the given path without consulting
   the include path ([libd] is never on it), that its members resolve directly
   to their alias targets, and that this works whether the targets are on the
   visible or the hidden include path... *)
{
  split [
  | flags = "-I liba -nocwd -open-cmi libd/aliases.cmi";
  | flags = "-H liba -nocwd -open-cmi libd/aliases.cmi";
  ]
  module = "libc/c8.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

(* ... without making the hidden targets' own names legal in user code. *)
{
  flags = "-H liba -nocwd -open-cmi libd/aliases.cmi";
  module = "libc/c10.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/cant_reference_hidden_target.ocamlc.reference";
  check-ocamlc.byte-output;
}

(* Test that [-open] and [-open-cmi] are processed in command-line order: a
   trailing [-open] shadows a rebinding from an earlier [-open-cmi]... *)
{
  flags =
    "-H liba -I libb -nocwd -open-cmi libd/aliases_a.cmi -open With_sub";
  module = "libb/order_open_last.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

(* ... and vice versa. *)
{
  flags =
    "-H liba -I libb -nocwd -open With_sub -open-cmi libd/aliases_a.cmi";
  module = "libb/order_open_cmi_last.ml";
  setup-ocamlc.byte-build-env;
  ocamlc.byte;
}

(* Test that the opened interface itself is not nameable, since [libd] is not
   on the include path. *)
{
  flags = "-H liba -nocwd -open-cmi libd/aliases.cmi";
  module = "libc/c9.ml";
  setup-ocamlc.byte-build-env;
  ocamlc_byte_exit_status = "2";
  ocamlc.byte;
  compiler_reference =
    "${test_source_directory}/cant_reference_open_cmi.ocamlc.reference";
  check-ocamlc.byte-output;
}

*)
