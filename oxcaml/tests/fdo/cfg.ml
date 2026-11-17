(* The counters in Cfg, which must be those of the Cmm it is selected from. *)

let[@inline never] side x = x * 3

(* A self tail call becomes a jump: its counter goes onto the edges into its
   block, here the [then] branch. [@loop never] keeps Simplify from turning it
   into a loop first. *)
let[@loop never] rec count_down n = if n > 0 then count_down (side n - 1) else n

(* Here they go onto the function entry, but miss the jump back (see
   [Cfg.add_orphaned_counters]). *)
let[@loop never] rec forever n = forever (side n)

(* Each outcome of a comparison has the counters of its branch. *)
let[@inline never] compare x y = if x < y then side 1 else side 2
