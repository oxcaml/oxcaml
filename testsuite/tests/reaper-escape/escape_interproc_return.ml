(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* The allocation happens in [mk] and is returned; only [f] knows it does
   not escape. [mk] is hidden by the signature, so all its call sites are
   known and it can be made to allocate its result in its caller's region
   (like an [exclave_]). [sum_pair] is exported, which prevents the pair from
   being unboxed instead. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

module M : sig
  val f : int -> int -> int
end = struct
  let[@inline never] mk x y = (x, y)

  let[@inline never] f x y =
    let p = mk x y in
    (sum_pair p [@nontail])
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 3 4)
