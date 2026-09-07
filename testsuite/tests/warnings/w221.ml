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

(* Fulfilled disable: suppresses the alert on [x]; no warning. *)
let _ = W221_lib.x [@@alert "-foo"]

(* Useless disable: [y] carries no alert; warning 221. *)
let _ = W221_lib.y [@@alert "-foo"]

(* Useless disable of [foo] on a use whose other alert [bar] does fire:
   the [bar] alert is reported and warning 221 fires for [foo]. *)
let _ = W221_lib.z [@@alert "-foo"]

(* Useless disable of an alert that is never triggered; warning 221. *)
let _ = 0 [@@alert "-never_fires"]

(* Silencing warning 221 on the same item.  Attributes are processed
   right-to-left, so only a [@warning] coming after the [@alert] in source
   order affects it. *)
let _ = 0 [@@alert "-never_fires"] [@@warning "-221"]
let _ = 0 [@@warning "-221"] [@@alert "-never_fires"]

(* Expression-level disables: fulfilled, then useless. *)
let _ = (W221_lib.x [@alert "-foo"])
let _ = (W221_lib.y [@alert "-foo"])

(* Fulfilled file-level disable: suppresses the alert on [z]; no warning. *)
[@@@alert "-bar"]
let _ = W221_lib.z

(* A disable of "all" is a wildcard, fulfilled by a suppressed occurrence
   of any alert; no warning. *)
[@@@alert "-all"]
let _ = W221_lib.x

(* An idle disable of "all" suppresses nothing; warning 221. *)
[@@@alert "-all"]

(* An item-level [@warning "-221"] does not affect a preceding file-level
   alert disable; warning 221. *)
[@@@alert "-quux"]
let _ = 0 [@@warning "-221"]

(* A file-level [@@@warning "-221"] silences subsequent disables. *)
[@@@warning "-221"]
[@@@alert "-quux2"]
