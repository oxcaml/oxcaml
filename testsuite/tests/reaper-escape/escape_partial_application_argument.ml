(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -experimental-reaper-escape -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* A dynamic partial application holds the arguments it was given: as for the
   type checker, it closes over them. In [mk] and [mk2] the partial application
   is returned to unknown code, so [p] must not be local, whether the function
   partially applied is a toplevel one ([h], whose partial applications are
   always on the heap) or a closure that is itself a candidate ([h2]). In
   [use], the partial application is only called while the region of [use] is
   open: [p], [h3] and the partial application itself are local, the latter
   being allocated in the region of [use] since [papp_local] returns at mode
   local. [clobber] reuses the memory of the regions that have been closed. *)
let[@inline never] sum7 (t : int * int * int * int * int * int * int) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] sum7_local
    (local_ (t : int * int * int * int * int * int * int)) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] clobber n =
  let local_ big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  let local_ big2 = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  (sum7_local big [@nontail]) + (sum7_local big2 [@nontail])

module M : sig
  val mk : int -> int -> int

  val mk2 : int -> int -> int

  val use : int -> int
end = struct
  let[@inline never] h a (b : int * int) c = a + fst b + snd b + c

  let[@inline never] papp (f : int -> int * int -> int -> int) x p =
    (f x p [@nontail])

  let[@inline never] papp_local (f : int -> int * int -> int -> int) x p =
    (f x p [@nontail])

  let[@inline never] mk n =
    let p = n, n + 1 in
    (papp h n p [@nontail])

  let[@inline never] mk2 n =
    let p = n, n + 1 in
    let h2 a (b : int * int) c = a + fst b + snd b + c + n in
    (papp h2 n p [@nontail])

  let[@inline never] use n =
    let p = n, n + 1 in
    let h3 a (b : int * int) c = a + fst b + snd b + c + n in
    let k = papp_local h3 n p in
    let big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
    let s = sum7 big in
    (k 5 [@nontail]) + s
end

let () =
  let g = M.mk 10 in
  let g2 = M.mk2 10 in
  let u = M.use 10 in
  let c = clobber 100 in
  (Printf.printf [@inlined never]) "%d %d %d %d\n" c (g 5) (g2 5) u
