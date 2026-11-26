(* The counters in Linear, which must be those of the Cfg it is linearized
   from. *)

let[@inline never] side x = x * 3

type t =
  | A
  | B
  | C

(* A three-way switch becomes a single comparison, with a jump for each case
   other than the one that falls through. *)
let[@inline never] three_way v =
  match v with A -> side 1 | B -> side 2 | C -> side 3

(* A conditional branch jumps to one case and falls through to the other. *)
let[@inline never] if_then_else b x = if b then side x else x + 1
