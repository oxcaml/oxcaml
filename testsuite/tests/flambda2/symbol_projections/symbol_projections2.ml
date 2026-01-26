(* TEST
   compile_only = "true";
   flambda2;
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
   check-ocamlopt.opt-output;
*)

(* From pchambart 2020-07-22, ocaml-flambda/ocaml issue #214 *)

[@@@ocaml.flambda_o3]

external opaque_identity : 'a -> 'a = "%opaque"

external ( + ) : int -> int -> int = "%addint"

let[@inline never] ignore _ = ()

let v = opaque_identity 33

let g () =
  let () = ignore () in
  let f x = x + v in
  f
