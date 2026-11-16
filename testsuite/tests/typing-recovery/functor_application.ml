(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

module M = struct end

module N = M (struct end)

module type S = module type of Unbound (struct end)

module F (X : sig type t end) = struct type t = X.t end

type t = F(M).t

let x : int = "not an int"
