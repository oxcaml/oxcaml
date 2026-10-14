(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* The allocation is returned through two levels of calls before being read by
   an exported function (so it cannot be unboxed): both callees are hidden by
   the signature, so all their call sites are known and they should be made to
   return at mode local. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p
module M : sig
  val f : int -> int -> int
end = struct
  let[@inline never] mk x y = (x, y)
  let[@inline never] mk2 x y = let p = mk x y in p
  let[@inline never] f x y =
    let p = mk2 x y in
    (sum_pair p [@nontail])
end
let () = Printf.printf "%d\n" (M.f 3 4)
