(* TEST
 native-compiler;
 no-address-sanitizer;
 setup-ocamlopt.byte-build-env;
 flags = "-g -gupstream-dwarf -S";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 script = "sh ${test_source_directory}/branch_debuginfo.sh";
 script;
*)

(* The code for the tests of a conditional or match, and for arms that only
   produce a constant or a variable, must carry debuginfo.  The script checks
   that the lines marked below appear in the line number table, and that the
   code at every local label starts with a location.  (This file must not
   contain calls: the code after a call does not start with a location.  For
   the same reason the test is not run with AddressSanitizer, which checks the
   load from the table in [lookup] and reports failures with a call.)

   With [-gno-upstream-dwarf] the assembly has further labels, which delimit
   the ranges of variable locations inside straight-line code and need not be
   followed by a location: the test does not apply to them, hence
   [-gupstream-dwarf].

   The marks must stay on the lines of the code they follow.  ocamlformat is
   not enabled for this directory. *)

let[@inline never] classify x =
  if x > 3 (* LINE *)
  then "big"
  else "small"

let[@inline never] name = function
  | 0 -> "zero" (* LINE *)
  | 1 -> "one"
  | n when n < 0 (* LINE *) -> "negative"
  | _ -> "many"

let[@inline never] pick x y z =
  match x with
  | None (* LINE *) -> y
  | Some _ -> z

(* When all arms of a match are constants, Simplify can replace it by a load
   from a table or by arithmetic on the scrutinee.  That code replaces the
   match and must carry its debuginfo. *)

type t = A | B | C | D

let[@inline never] lookup x =
  match x with
  | A (* LINE *) -> "a"
  | B -> "b"
  | C -> "c"
  | D -> "d"

let[@inline never] affine x =
  match x with
  | A (* LINE *) -> 10
  | B -> 12
  | C -> 14
  | D -> 16
