(* TEST
 flambda2;
 setup-ocamlopt.jsir-js-build-env;
 module = "poll_jsir.ml";
 ocamlopt.jsir-js;
 script = "cp poll_jsir.cmx-js without-flag.cmx-js";
 script;
 flags = "-enable-poll-insertion";
 ocamlopt.jsir-js;
 script = "cmp poll_jsir.cmx-js without-flag.cmx-js";
 script;
*)

(* A compilation to JavaScript generates no native code, so the flags for
   poll insertion change nothing in what it produces: not the unit info
   either, whose bytes are compared here. *)

let f x = x + 1
