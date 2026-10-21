(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* try/with inside and around loops. The non-escaping reference [acc] is
   allocated locally by the type checker (before being turned into a mutable
   variable), so the front end keeps a region around the whole body of [f], in
   which the try-regions are nested; the analysis currently makes nothing local
   inside such a pre-existing region, so both pairs stay on the heap. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p
let[@inline never] boom (p : int * int) =
  if fst p mod 3 = 0 then raise Exit else snd p
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
let () = (Printf.printf [@inlined never]) "%d\n" (f 10)
