(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* A function returning at mode local called from a loop: the result lives in
   the per-iteration region. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p
module M : sig
  val f : int -> int
end = struct
  let[@inline never] mk i = (i, i * 2)
  let[@inline never] f n =
    let acc = ref 0 in
    for i = 1 to n do
      let p = mk i in
      acc := !acc + sum_pair p
    done;
    !acc
end
let () = Printf.printf "%d\n" (M.f 10)
