(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* Negative: the pair is passed to a C external (Hashtbl.hash). *)
let[@inline never] f x y =
  let p = (x, y) in
  Hashtbl.hash p

let () = Printf.printf "%d\n" (f 1 2 land 0xff)
