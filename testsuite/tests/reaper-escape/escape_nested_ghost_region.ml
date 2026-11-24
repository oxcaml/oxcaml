(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* The region of the loop receives the result of [mk], which returns at mode
   local, so it has a ghost region; the region of [f], which only holds [keep],
   has none. The ghost region of the loop must not be given the missing one as
   its parent. In [g] both regions receive such a result, and the ghost region
   of the loop has the one of [g] as its parent. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

module M : sig
  val f : int -> int

  val g : int -> int
end = struct
  let[@inline never] mk i = i, i * 2

  let[@inline never] f n =
    let keep = n, n + 1 in
    let acc = ref 0 in
    for i = 1 to n do
      let p = mk i in
      acc := !acc + sum_pair p
    done;
    !acc + sum_pair keep

  let[@inline never] g n =
    let keep = mk n in
    let acc = ref 0 in
    for i = 1 to n do
      let p = mk i in
      acc := !acc + sum_pair p
    done;
    !acc + sum_pair keep
end

let () = (Printf.printf [@inlined never]) "%d %d\n" (M.f 10) (M.g 10)
