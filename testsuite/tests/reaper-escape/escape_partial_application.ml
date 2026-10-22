(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* A two-argument closure partially applied through an unknown-arity call in a
   hidden higher-order function. The partial application is allocated by the
   curry functions in the region current in [papp], so it is treated as an
   allocation of [papp] returned to its caller. In [g] the partial application
   is only called: [h] can be local, and [papp] is made to return at mode local
   so that the partial application lands in the region of [g]. In [g2] the
   partial application is returned, so it escapes, and [h] with it. ([papp] and
   [papp2] are distinct because the analysis is not context-sensitive: a single
   helper would merge the two results.) *)
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
let () = (Printf.printf [@inlined never]) "%d %d\n" (M.g 2 3) ((M.g2 2 3) 2)
