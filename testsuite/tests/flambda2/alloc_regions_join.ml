(* TEST
 compile_only = "true";
 ocamlopt_flags = "-O3";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

(* Join points whose incoming jumps carry region-closing check actions:
   regions used only by check actions must be seen by the flow analysis, the
   [Switch] simplifications may only merge arms with identical check actions,
   and continuations lifted out of a handler that defines (and removes)
   regions must have the corresponding entries renamed along with their lifted
   parameters. *)

let[@inline always] abs' x = if x > 0 then x else -x

let[@zero_alloc] join1 b t =
  let r = if b then abs' t else abs' (t + 1) in
  if r = 0 then 1 else r * 2

let[@zero_alloc] switch_arms t =
  match t with 0 -> 10 | 1 -> 20 | 2 -> 30 | _ -> 40

let[@zero_alloc assume] tail_if b x y = if b then abs' x else abs' y
