(* TEST
   flambda2;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* The .mli ensures that only the [test_*] functions escape, so the reaper is
   able to change the calling conventions of the [add_*] and [read] functions
   below and unbox the boxed numbers flowing through them.  There should be no
   [%box_num] or [%unbox_num] operations left in the output. *)

let[@inline never] [@local never] add_int64 x y = Int64.add x y

let[@inline never] [@local never] add_int32 x y = Int32.add x y

let[@inline never] [@local never] add_nativeint x y = Nativeint.add x y

let test_int64 a b = Int64.to_int (add_int64 (Int64.of_int a) (Int64.of_int b))

let test_int32 a b = Int32.to_int (add_int32 (Int32.of_int a) (Int32.of_int b))

let test_nativeint a b =
  Nativeint.to_int (add_nativeint (Nativeint.of_int a) (Nativeint.of_int b))

(* A boxed number captured in a closure: the closure is unboxed, and the
   contents of its value slot along with it. *)
let test_value_slot n =
  let b = Int64.of_int n in
  let[@inline never] [@local never] read () = Int64.to_int b in
  read () + 1

(* Boxed numbers inside an unboxed block: the pair is unboxed into its
   components, whose boxes are unboxed in turn (and the unused second
   component is deleted entirely). *)
let[@inline never] [@local never] pair_first (p : int64 * int64) =
  Int64.to_int (fst p)

let test_pair a b = pair_first (Int64.of_int a, Int64.of_int b)
