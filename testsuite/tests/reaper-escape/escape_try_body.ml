(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* A heap allocation inside a [try] body is executed while the try-region is
   open, so it must not be made local in the function's region: it would be
   freed when the try-region is closed, and overwritten by [q]. *)
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

let () = Printf.printf "%d\n" (M.f 1)
