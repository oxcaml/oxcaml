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
  val concat : ?'sep:string option -> string list -> string
end

(* Implementation *)
module M : S = struct
let concat ?'(sep  <- " ") xs =
  String.concat sep xs
end

(* Usage *)
let default_concat ys = M.concat ys
let comma_concat zs = M.concat ~sep:(Some " ") zs
let chain_call ?'(sep : string option) arg = M.concat ?'sep arg
