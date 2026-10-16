(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* The inner option is reachable from the outer one, which is returned by [mk]
   (and unboxed); the inner one then escapes from [f] through its result, so
   it must be allocated in the region of [g], not in one of [mk]. *)
let[@inline never] get o = match o with Some v -> v | None -> 0

let[@inline never] sum_pair (p : int * int) = fst p + snd p

module M : sig
  val g : int -> int
end = struct
  let[@inline never] mk x = Some (Some x)

  let[@inline never] f x = match mk x with Some y -> y | None -> None

  let[@inline never] g x =
    let y = f x in
    let p = x + 100, x + 200 in
    get y + sum_pair p
end

let () = Printf.printf "%d\n" (M.g 7)
