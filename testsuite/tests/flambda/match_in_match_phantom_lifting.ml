(* TEST
 compile_only = "true";
 flambda2;
 flags += "-O3 -g -flambda2-expert-phantom-lets -flambda2-match-in-match";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-simplify;
 check-fexpr-dump;
*)

[@@@ocaml.flambda_o3]

[@@@ocaml.warning "-26"]

(* Once [h] is inlined in [f], the continuation after the definition of [x] is
   specialized, and its handler binds the phantom variable [dbg], which the
   continuations lifted out of it refer to (in the definitions of other
   phantom variables). Phantom variables cannot be passed as arguments: they
   are bound as optimised out in the lifted handlers. *)

external opaque : 'a -> 'a = "%opaque"

let[@inline never] g x = opaque x

let[@inline always] h x c2 (z : int) =
  let dbg = z + 1 in
  let y =
    match x with
    | Some v -> if c2 then Some (g v) else None
    | None -> Some (g z)
  in
  let dbg2 = dbg + 2 in
  let w = match y with Some u -> g (u + 1) | None -> g (z + 2) in
  let dbg3 = dbg2 + dbg in
  g (w * 2)

let f c1 c2 (z : int) =
  let x = if c1 then Some z else None in
  let r = h x c2 z in
  g r + 1
