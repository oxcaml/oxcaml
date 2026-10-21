(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* The pair allocated in the [try] body reaches the handler through the mutable
   variable [acc] (an extra parameter of the handler): the handler closes the
   try-region first, which frees everything allocated in the body, so the pair
   must stay on the heap (it would otherwise be overwritten by [q]). [acc] is
   annotated [global]: otherwise the type checker allocates the non-escaping
   reference locally, which leaves a region around the body of [f] in which the
   analysis does nothing (see [escape_loop_try]). *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

module M : sig
  val f : int -> int
end = struct
  let[@inline never] f x =
    let acc @ global = ref (0, 0) in
    let r =
      try
        acc := (x, x + 1);
        if x > 0 then raise Exit else !acc
      with Exit -> !acc
    in
    let q = x + 5, x + 6 in
    sum_pair q + sum_pair r
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 1)
