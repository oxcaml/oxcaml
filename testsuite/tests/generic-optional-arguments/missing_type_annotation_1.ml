(* TEST
 flags = "-extension-universe alpha";

 setup-ocamlc.byte-build-env;
 (* This test should fail after type checking is implemented. *)
 (*= ocamlc_byte_exit_status = "2"; *)
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Interface *)
module type S = sig
  val concat : Stdlib.Option.?'sep:string -> string list -> string
end

(* Implementation *)
module M : S = struct
let concat Stdlib.Option.?'(sep=" ") xs =
  String.concat sep xs
end
