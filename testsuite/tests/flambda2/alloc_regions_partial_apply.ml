(* TEST
 compile_only = "true";
 ocamlopt_flags = "-O3";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

let[@inline never] add3 x y z = x + y + z

(* Tail position: the application's alloc checks close the region on normal
   return. *)
let[@zero_alloc assume] partial_tail x = add3 x

(* Non-tail position. *)
let partial_nontail x =
  let f = add3 x 1 in
  let () = () in
  f

let use x = partial_tail x 1 2 + (partial_nontail x) 2
