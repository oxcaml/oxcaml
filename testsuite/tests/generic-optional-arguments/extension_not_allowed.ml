(* TEST
 flags = "-extension-universe upstream_compatible";

 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Interface *)
module type S = sig
  val concat : (?sep):string or_null -> string list -> string
end
