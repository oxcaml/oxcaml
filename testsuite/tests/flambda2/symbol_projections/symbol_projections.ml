(* TEST
   compile_only = "true";
   flambda2;
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
   check-ocamlopt.opt-output;
*)

[@@@ocaml.flambda_o3]

external getenv : string -> string = "caml_sys_getenv"

external ( + ) : int -> int -> int = "%addint"

let foo = match getenv "FOO" with exception _ -> false | _ -> true

let f x =
  let g y = if foo then y + y else y in
  x, g
