(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* [f] has a region of its own from the type checker, for [l]; it is closed
   before the tail call to [after], whose argument is allocated while it is
   open. Once [after] is inlined, the pair is used after that region has been
   closed, so it must not be made local in the region of [f] (where it would
   be overwritten by [s]). *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] sum_local (local_ p : int * int) = fst p + snd p

let[@inline always] after (p : int * int) (n : int) =
  let r = n, n + 1 in
  let s = n + 2, n + 3 in
  sum_pair r + sum_pair s + sum_pair p

module M : sig
  val f : int -> int
end = struct
  let[@inline never] f x =
    let local_ l = x, x in
    let n = sum_local l in
    after (x + 1, x + 2) n
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 10)
