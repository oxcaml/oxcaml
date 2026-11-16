(* The counters To_cmm takes off the edges of the switches it turns into a table
   lookup or a single jump, which it finds here after the reaper: once [g]'s
   first parameter is removed, the arms of each match pass nothing and jump to
   the same place. The counters go onto the edges into the code the switch is
   in. *)

let[@inline never] side x = x * 3

type t =
  | A
  | B
  | C
  | D

let[@inline never] g _a b = side b

(* The entry of the function. *)
let[@inline never] at_entry v x y z w =
  let a = match v with A -> x | B -> y | C -> z | D -> w in
  g a x

(* A join handler: the arms that jump to it. *)
let[@inline never] in_join b v x y z w =
  let s = if b then side x else side y in
  let a = match v with A -> x | B -> y | C -> z | D -> w in
  g a s

(* An exception handler: the raise. *)
let[@inline never] in_exn_handler c v x y z w =
  try if c then raise Exit else side x
  with _ ->
    let a = match v with A -> x | B -> y | C -> z | D -> w in
    g a x

(* A loop body: the entry of the loop, but not its back edge (see
   [Fdo_to_cmm_region_counters]). *)
let[@inline never] in_loop n v x y z w =
  let r = ref 0 in
  for i = 0 to n do
    let a = match v with A -> x | B -> y | C -> z | D -> w in
    r := !r + g a i
  done;
  !r
