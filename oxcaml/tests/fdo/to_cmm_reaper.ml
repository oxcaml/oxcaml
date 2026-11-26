(* The counters To_cmm takes off the edges of the switches it turns into a table
   lookup or a single jump, which it finds here after the reaper: once [g]'s
   first parameter is removed, the arms of the match pass nothing and jump to
   the same place. The counters are dropped (see [Fdo_dropped_counters]). *)

let[@inline never] side x = x * 3

type t =
  | A
  | B
  | C
  | D

let[@inline never] g _a b = side b

let[@inline never] collapsed v x y z w =
  let a = match v with A -> x | B -> y | C -> z | D -> w in
  g a x
