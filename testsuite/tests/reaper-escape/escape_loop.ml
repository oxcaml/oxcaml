(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* Allocation inside a loop: a region per iteration (or around the loop
   body) is needed, not a region around the whole function. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] f n =
  let acc = ref 0 in
  for i = 1 to n do
    let p = (i, i + 1) in
    acc := !acc + sum_pair p
  done;
  !acc

let () = Printf.printf "%d\n" (f 10)
