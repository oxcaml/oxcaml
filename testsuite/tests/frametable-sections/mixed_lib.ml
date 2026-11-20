exception Deep of int

let[@inline never] frame3 n = if n >= 0 then raise (Deep n) else n

let[@inline never] frame2 n = frame3 (n + 1) + 1

let[@inline never] frame1 n = frame2 (n + 1) + 1

let[@inline never] raise_deep n = ignore (frame1 n)

(* Allocates on the way down and runs a major GC at the bottom, so that the
   GC scans every frame of the recursion through its descriptor. *)
let[@inline never] rec deep_alloc n acc =
  if n = 0
  then (
    Gc.full_major ();
    List.length acc)
  else deep_alloc (n - 1) (string_of_int n :: acc) + 1

let[@inline never] call_back f = f () + 1
