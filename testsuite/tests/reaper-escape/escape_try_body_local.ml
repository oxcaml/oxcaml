(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* Allocations in a [try] body that are not raised live in the enclosing region
   and may be local. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] boom x = if x > 100 then raise Exit else x

module M : sig
  val f : int -> int
end = struct
  let[@inline never] f x =
    let r = try let p = x, x + 1 in sum_pair p + boom x with Exit -> 0 in
    let q = x + 5, x + 6 in
    sum_pair q + r
end

let () = (Printf.printf [@inlined never]) "%d %d\n" (M.f 1) (M.f 200)
