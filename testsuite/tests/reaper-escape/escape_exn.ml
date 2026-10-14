(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* Exceptions: raising past an open region is fine (the handler restores
   the local stack pointer), but a value carried by the exception escapes. *)
exception E of (int * int)

let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] f x y =
  let p = (x, y) in
  if x < 0 then raise (Failure "neg") else sum_pair p

let[@inline never] g x y =
  let p = (x, y) in
  if x < 0 then raise (E p) else sum_pair p

let () =
  Printf.printf "%d\n" (f 1 2);
  (try ignore (g (-1) 2) with E (a, b) -> Printf.printf "E %d %d\n" a b)
