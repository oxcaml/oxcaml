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

(* f is lifted first, captures foo *)
let f x =
  let g y = if foo then y + y else y in
  x, g

(* h calls f, gets g, then creates another closure using something from g's environment *)
let h z =
  let (_, g) = f z in
  (* Now we need to access something from g that would create a projection *)
  let result = g z in
  result
