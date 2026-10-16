(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* The exception payload is allocated in the [try] body and caught by a handler
   of the same function: the handler closes the try-region first, which frees
   everything allocated in the body, so the payload must stay on the heap. *)
exception E of (int * int)

let[@inline never] sum_pair (p : int * int) = fst p + snd p

module M : sig
  val f : int -> int
end = struct
  let[@inline never] f x =
    let r = try if x > 0 then raise (E (x, x + 1)) else 0, 0 with E p -> p in
    let q = x + 5, x + 6 in
    sum_pair q + sum_pair r
end

let () = Printf.printf "%d\n" (M.f 1)
