(* TEST
 flags = "-extension-universe alpha";

 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Module interface vs implementation mismatch *)
module type S = sig
  val f : (?x):int option -> unit -> int
end

module M : S = struct
  (* This should fail - implementation uses or_null but interface declares
     option *)
  let f (?(x = 42) : int or_null) () = x
end
