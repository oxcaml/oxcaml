(* TEST
   flambda2;
   flags += "-O3 -flambda2-reaper -X reaper-lambda-lifting=1 -reaper-local-fields -reaper-debug-flags=nostamps";
   native with dump-reaper;
   check-fexpr-dump;
 *)

(* The partial application of [f] in [mk] is done through [apply2], so the
   stub is created by simplify. Stubs are not resimplified when inlined, so
   the two closures [g1] and [g2] share the same code_id. As [f] does not use
   its first two arguments, the stub does not read the value slots of its
   closure, and [g1] and [g2] can be unboxed. However, the [my_closure]
   variable of the stub is not unboxed (it is unused), so we must be careful
   not to leave stray references to [g1] or [g2]. *)

let[@inline never] f (_x : int) (_y : int) z = z + 1

let[@inline always] apply2 h a b = h a b

let[@inline always] mk a b = apply2 f a b

let[@inline never] test a b c =
  let g1 = mk a b in
  let g2 = mk b a in
  (g1 [@inlined never]) c + (g2 [@inlined never]) c

let () =
  assert (test 1 2 3 = 8)