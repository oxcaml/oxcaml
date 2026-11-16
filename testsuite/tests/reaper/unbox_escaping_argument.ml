(* TEST
   flambda2;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   native with dump-simplify, dump-reaper;
   check-fexpr-dump;
 *)

(* The argument given to [f] used to be unboxed but was not rewritten
   correctly, causing a crash. *)

type _ w = P : (int * int) w | F : (int -> int) w

let[@inline never] f : type a. a w -> a -> int = fun w p ->
  match w with F -> p 3 | P -> 0

let[@inline never] g n = f P (n, n)

let () = print_int (g 5)
