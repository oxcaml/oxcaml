(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* A dynamic partial application is allocated by the curry functions in the
   region current at the call site: here in [papp], whose own local allocation
   [tmp] is live across it. It is treated as an allocation of [papp] returned to
   its caller: [papp] is made to return at mode local, so that [tmp] and the
   partial application land in the region of [run], where [h] is local, and
   [big] (allocated in that region after the call) cannot overwrite them.
   [papp_exported] cannot change its calling convention, so the partial
   application it returns escapes, and [h2] stays on the heap. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] sum7 (t : int * int * int * int * int * int * int) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] papp_exported (f : int -> int -> int) m =
  let tmp = m, m + 1 in
  let k = f (sum_pair tmp) in
  let _ = sum_pair tmp in
  k

module M : sig
  val run : int -> int
end = struct
  let[@inline never] papp (f : int -> int -> int) m =
    let tmp = m, m + 1 in
    let k = f (sum_pair tmp) in
    let _ = sum_pair tmp in
    k

  let[@inline never] run n =
    let h a b = a + b + n in
    let k = papp h n in
    let h2 a b = (a * b) + n in
    let k2 = papp_exported h2 n in
    let big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
    k 2 + k2 3 + sum7 big
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.run 10)
