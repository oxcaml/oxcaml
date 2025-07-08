(* TEST
 flags = "-extension-universe upstream_compatible";

 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* CR generic-optiona: Reject this file in type checking *)

(* Interface *)
module type S = sig
  val concat : ?'sep:string -> string list -> string
end

(* Implementation *)
module M : S = struct
  let concat ?'(sep : string option <- " ") xs =
    String.concat sep xs
end

(* Usage *)
let default_concat ys = M.concat ys
let comma_concat zs = M.concat ~sep:" " zs
let chain_call ?'(sep : string option) arg = M.concat ?'sep arg
