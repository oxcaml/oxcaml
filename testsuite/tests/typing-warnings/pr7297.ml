(* TEST
   flags = " -w +A -strict-sequence ";
   expect;
*)

(* Ignore OCAMLRUNPARAM=b to be reproducible *)
Printexc.record_backtrace false

[%%expect {|
- : unit = ()
|}]

let () =
  raise Exit;
  ()

(* warn *)
[%%expect
{|
Line 2, characters 2-12:
2 |   raise Exit;
      ^^^^^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)

Exception: Stdlib.Exit.
|}]
