(* TEST_BELOW
(* Blank lines added here to preserve locations. *)











*)

(*
  ocamlc -c pr3918a.mli pr3918b.mli
  rm -f pr3918a.cmi
  ocamlc -c pr3918c.ml
*)

open Pr3918b

let f x = (x : 'a vlist :> 'b vlist)
let f (x : 'a vlist) = (x : 'b vlist)

(* TEST
 readonly_files = "pr3918a.mli pr3918b.mli";
 setup-ocamlc.byte-build-env;
 module = "pr3918a.mli";
 ocamlc.byte;
 module = "pr3918b.mli";
 ocamlc.byte;
 script = "rm -f pr3918a.cmi";
 script;
 module = "pr3918c.ml";
 ocamlc.byte;
*)

(* This program used to unnecessarily raise the error:
File "pr3918c.ml", line 24, characters 11-12:
24 | let f x = (x : 'a vlist :> 'b vlist)
                ^
Error: This expression has type "'b Pr3918b.vlist"
       but an expression was expected of type "'b Pr3918b.vlist"
*)
