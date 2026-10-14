(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* A two-argument closure partially applied through an unknown-arity call in a
   hidden higher-order function. In [g] the partial application is only
   called: [h] can be local. In [g2] the partial application is returned, so
   [h] must stay on the heap. ([papp] and [papp2] are distinct because the
   analysis is not context-sensitive: a single helper would merge the two
   results.) *)
module M : sig
  val g : int -> int -> int
  val g2 : int -> int -> int -> int
end = struct
  let[@inline never] papp (f : int -> int -> int) = f 1
  let[@inline never] papp2 (f : int -> int -> int) = f 1
  let[@inline never] g a b =
    let h y z = (y * a) + (b * z) in
    let p = papp h in
    (p 2 [@nontail])
  let[@inline never] g2 a b =
    let h y z = (y * a) + (b * z) in
    papp2 h
end
let () = Printf.printf "%d %d\n" (M.g 2 3) ((M.g2 2 3) 2)
