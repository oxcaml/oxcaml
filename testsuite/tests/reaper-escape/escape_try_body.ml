(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* The pair allocated in the [try] body is the result of the [try] expression,
   reaching the continuation after it (defined outside the try-region) on the
   normal path only; the handler closes the try-region, which frees everything
   allocated in the body, before producing its own value. Since the try-region
   is never closed on the normal path, the pair may be local in the region of
   [f]. (See [escape_try_mutable] for a value reaching the handler.) *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

module M : sig
  val f : int -> int
end = struct
  let[@inline never] f x =
    let r =
      try
        let p = x, x + 1 in
        if x > 100 then raise Exit else p
      with Exit -> 0, 0
    in
    let q = x + 5, x + 6 in
    sum_pair q + sum_pair r
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.f 1)
