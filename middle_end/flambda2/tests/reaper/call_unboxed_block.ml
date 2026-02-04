(* ocamlopt -nostdlib -nopervasives -flambda2-reaper -c call_unboxed_block.ml *)

(* Core test case, with magic *)
external magic : 'a -> 'b = "%identity"

let f x =
  let y = (x, x) in
  let[@inline never][@local never] hide_from_simplify z = z in
  (hide_from_simplify (magic y)) ()

(* Without magic *)
type t = A of ((int * int) * int) | B of (unit -> int)

let f2 x =
  let[@inline never][@local never] g = function
    | A ((u, _), _) -> u
    | B f -> f ()
  in
  g (A ((x, x), x))