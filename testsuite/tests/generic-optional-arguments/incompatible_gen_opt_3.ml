(* TEST
 flags = "-extension-universe alpha";

 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Module interface vs implementation mismatch *)
module type S = sig
  val f : Stdlib.Option.?'x:int -> unit -> int
end

module M : S = struct
  (* This should fail - implementation uses Stdlib.Or_null.?' but interface
     declares Stdlib.Option.?' *)
  let f Stdlib.Or_null.?'(x : int = 42) () = x
end