(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* A recursive function whose result is allocated: callers only read it. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p
module M : sig
  val f : int -> int
end = struct
  let rec build n =
    if n = 0 then (0, 0) else let p = build (n - 1) in (sum_pair p, n)
  [@@inline never]
  let[@inline never] f n =
    let p = build n in
    (sum_pair p [@nontail])
end
let () = (Printf.printf [@inlined never]) "%d\n" (M.f 10)
