type t =
  { used : int;
    unused : int
  }

let[@inline never] make x = { used = x; unused = Sys.opaque_identity (x * 100) }

let[@inline never] read t = t.used

(* Never used by any module in the program, but not yet removed by the
   whole-program solve: the naive graph union keeps each unit's conservative
   boundary facts. *)
let[@inline never] unused_export t = t.unused

type p =
  { keep : int;
    drop : int
  }

(* A closure that stays local to this unit, capturing a value ([extra]) whose
   only use is the never-read [drop] field of a block that also stays local.
   The value slot is dead: it must be kept, with a poisoned value, so that the
   layout of the closure does not change. *)
let[@inline never] sum_with_dead_capture x =
  let extra = Sys.opaque_identity (x * 7) in
  let[@inline never] [@local never] make_p a b = { keep = a; drop = b } in
  let[@inline never] [@local never] g y = (make_p (x + y) extra).keep in
  g (Sys.opaque_identity 1)

(* A closure whose body is small enough to be inlined into other units, so
   their code projects this unit's value slots directly. Guards cross-unit
   agreement on closure layouts and slot offsets. *)
let[@inline never] make_adder x =
  let base = Sys.opaque_identity (x + 1) in
  let[@inline always] add y = base + y in
  add
