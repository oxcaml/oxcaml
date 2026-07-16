(* TEST_BELOW
(* Blank lines added here to preserve locations. *)










*)

(* Fulfilled disable: suppresses the alert on [x]; no warning. *)
let _ = W221_lib.x [@@alert "-foo"]

(* Useless disable: [y] carries no alert; warning 221. *)
let _ = W221_lib.y [@@alert "-foo"]

(* Useless disable of an alert that is never triggered; warning 221. *)
let _ = 0 [@@alert "-never_fires"]

(* Useless disables silenced by [@warning "-221"], in either order. *)
let _ = 0 [@@alert "-never_fires"] [@@warning "-221"]
let _ = 0 [@@warning "-221"] [@@alert "-never_fires"]

(* Expression-level disables: fulfilled, then useless. *)
let _ = (W221_lib.x [@alert "-foo"])
let _ = (W221_lib.y [@alert "-foo"])

(* Fulfilled file-level disable: suppresses the alert on [z]; no warning. *)
[@@@alert "-bar"]
let _ = W221_lib.z

(* Useless file-level disable; warning 221. *)
[@@@alert "-quux"]

(* TEST
 modules = "w221_lib.mli w221_lib.ml";
 setup-ocamlc.byte-build-env;
 flags = "-w -a";
 module = "w221_lib.mli";
 ocamlc.byte;
 module = "w221_lib.ml";
 ocamlc.byte;
 flags = "-w +A-70";
 module = "w221.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
