(* TEST
 compile_only = "true";
 flambda2;
 ocamlopt_flags = "-O3 -flambda2-match-in-match";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

(* The continuation after the definition of [y] is lifted out of the
   continuation after the definition of [x], which is specialized, and is
   then itself specialized. The continuation after the definition of [w],
   which is lifted out of it, uses the continuation after the definition of
   [r], which was lifted out of the first one: it must be given the arguments
   for the lifted params of that continuation. *)

external opaque : 'a -> 'a = "%opaque"

let[@inline never] g x = opaque x

let f c1 c2 (z : int) =
  let x = if c1 then Some z else None in
  let r =
    let y =
      match x with
      | Some v -> if c2 then Some (g v) else None
      | None -> Some (g z)
    in
    let w = match y with Some u -> g (u + 1) | None -> g (z + 2) in
    g (w * 2)
  in
  g r + 1
