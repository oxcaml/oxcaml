(* TEST_BELOW
(* Blank lines added here to preserve locations. *)







*)

(* Just ensure that we're running the check on mli files too *)

type t [@@alert foo "Foo!"]

val useful : t [@@alert "-foo"]     (* fulfilled: no warning *)
val useless : int [@@alert "-foo"]  (* useless: warning 221 *)

(* TEST
 flags = "-w +A-70";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
