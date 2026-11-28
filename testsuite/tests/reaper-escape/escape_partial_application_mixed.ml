(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -experimental-reaper-escape -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* The curry functions allocate a partial application on the heap or locally
   according to the code of the closure applied. In [use], the unknown-arity
   application in [papp] partially applies either the toplevel function
   [static], whose partial applications are always on the heap, or the closure
   [h]. Were [h] made local, the partial application would be treated as local
   too, and [p], which it holds, with it: but when [static] is applied it is
   allocated on the heap, and would point at the local stack. So the partial
   application, [h] and [p] all stay on the heap. In [use_local] only a local
   closure is applied, and everything is local. *)
let[@inline never] sum7 (t : int * int * int * int * int * int * int) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] static a (b : int * int) c = a + fst b + snd b + c

module M : sig
  val use : int -> bool -> int

  val use_local : int -> int
end = struct
  let[@inline never] papp (f : int -> int * int -> int -> int) x p =
    (f x p [@nontail])

  let[@inline never] papp_local (f : int -> int * int -> int -> int) x p =
    (f x p [@nontail])

  let[@inline never] use n flag =
    let p = n, n + 1 in
    let h a (b : int * int) c = a + fst b + snd b + c + n in
    let f = if flag then h else static in
    let k = papp f n p in
    let big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
    let s = sum7 big in
    (k 5 [@nontail]) + s

  let[@inline never] use_local n =
    let p = n, n + 1 in
    let h a (b : int * int) c = a + fst b + snd b + c + n in
    let k = papp_local h n p in
    let big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
    let s = sum7 big in
    (k 5 [@nontail]) + s
end

let () =
  (Printf.printf [@inlined never]) "%d %d %d\n" (M.use 10 true)
    (M.use 10 false) (M.use_local 10)
