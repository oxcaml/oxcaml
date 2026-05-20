(* TEST
   flambda2;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   { native with dump-raw, dump-simplify, dump-reaper; check-fexpr-dump; }
 *)

let f x y =
  (* [g] will use its value slot [y] if and only if [h] uses it. *)
  let g h = x + h y in
  let[@inline never] id z = z in
  let[@inline never] unused _ = 0 in
  (* Here, [g] is inlined, and will use its value slot [y]. Since we
     are inside the code of [u], simplify has to keep reading from the
     closure of [g]. *)
  let[@inline never][@local never] u () = (g[@inlined always]) id in
  (* In the only non-inlined call of [g], [h] will not use its argument,
     meaning that the value slot [y] is unused inside [g]. If we unbox
     the closure of [g], we must therefore take care not to include the
     value slot [y] in this call. *)
  (g[@inlined never]) unused + u ()