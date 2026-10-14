(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* try/with inside and around loops with local allocations. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p
let[@inline never] boom (p : int * int) = if fst p mod 3 = 0 then raise Exit else snd p
let[@inline never] f n =
  let acc = ref 0 in
  for i = 1 to n do
    let p = (i, i) in
    (try acc := !acc + boom p with Exit -> acc := !acc + sum_pair p)
  done;
  (try
     for i = 1 to n do
       let p = (i, i) in
       acc := !acc + boom p
     done
   with Exit -> ());
  !acc
let () = Printf.printf "%d\n" (f 10)
