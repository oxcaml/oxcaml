(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-g -S";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 script = "sh ${test_source_directory}/branch_debuginfo.sh";
 script;
*)

(* The code for the tests of a conditional or match, and for arms that only
   produce a constant or a variable, must carry debuginfo.  The script checks
   that the lines marked below appear in the line number table, and that the
   code at every local label starts with a location.  (This file must not
   contain calls: the code after a call does not start with a location.) *)

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
