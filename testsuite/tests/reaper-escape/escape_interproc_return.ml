(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* The allocation happens in [mk] and is returned; only [f] knows it does
   not escape. Making it local needs [mk] to allocate in its caller's
   region (a calling-convention change), like an [exclave_]. *)
let[@inline never] mk x y = (x, y)

let[@inline never] f x y =
  let p = mk x y in
  fst p + snd p

let () = Printf.printf "%d\n" (f 3 4)
