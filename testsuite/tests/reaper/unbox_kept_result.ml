(* TEST
   flambda2;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   native with dump-simplify, dump-reaper;
   check-fexpr-dump;
 *)

(* Here, we used to unbox the result of [make], which used to cause
   a bug in the rewrite of the arity wrapper, causing a miscompilation. *)

type _ w = P : (int * int) w | F : (int -> int) w

let[@inline never] make x = (x, x)

let[@inline never] test (type a) (w : a w) =
  let g : a = match w with P -> make 1 | F -> succ in
  match w with F -> g 3 | P -> fst (make 10)

let () = print_int (test (Sys.opaque_identity P))
