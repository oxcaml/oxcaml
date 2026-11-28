(* The counters in Cfg, which must be those of the Cmm it is selected from. *)

let[@inline never] side x = x * 3

(* A self tail call becomes a jump: its counter is dropped (see
   [Fdo_dropped_counters]). [@loop never] keeps Simplify from turning it into a
   loop first. *)
let[@loop never] rec count_down n = if n > 0 then count_down (side n - 1) else n

(* Each outcome of a comparison has the counters of its branch. *)
let[@inline never] compare x y = if x < y then side 1 else side 2
