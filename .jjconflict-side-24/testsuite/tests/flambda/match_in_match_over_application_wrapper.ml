(* TEST
 flambda;
 ocamlopt_flags = "-O3 -flambda2-match-in-match -flambda2-expert-cont-specialization-threshold 200";
 native;
*)

type t = A | B | C
type s = { x : int; }

let[@inline never] bar_a _ = ()
let[@inline never] bar_b _ = ()
let[@inline never] bar_c _ = ()

let foo t =
  match t with
  | A -> bar_a
  | B -> bar_b
  | C -> bar_c

let test f b x =
  let t = if b then A else C in
  let y = { x; } in
  f t y

let bug b =
  test foo b 5
