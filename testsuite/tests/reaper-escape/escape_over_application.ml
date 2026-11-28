(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -experimental-reaper-escape -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* Over-applications through unknown-arity calls: [make] and [make2] take one
   argument and return a closure, which the generic application functions then
   apply to the remaining arguments. These functions are called through
   unknown-arity applications, so they cannot return at mode local, and the
   closures they return stay on the heap, with the blocks they capture.
   [clobber] is called between the creation and the use of each closure. *)
let[@inline never] sum7 (t : int * int * int * int * int * int * int) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] clobber n =
  let big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  (sum7 big [@nontail])

module M : sig
  val run : int -> int
end = struct
  let[@inline never] over2 (f : int -> int -> int) x =
    let r = f x 2 in
    r + clobber x

  let[@inline never] make a =
    let t = a, a + 1 in
    let _ = clobber a in
    fun b -> fst t + snd t + b

  let[@inline never] over3 (f : int -> int -> int -> int) x =
    let r = f x 2 3 in
    r + clobber x

  let[@inline never] make2 a =
    let t = a, a + 1 in
    fun b ->
      let u = b, fst t in
      let _ = clobber b in
      fun c -> fst u + snd u + snd t + c

  let[@inline never] run n =
    let x = over2 make n in
    let y = over3 make2 n in
    let g = make n in
    let _ = clobber n in
    let z = g 5 in
    let h = make2 n 7 in
    let _ = clobber n in
    x + y + z + h 9
end

let () = (Printf.printf [@inlined never]) "%d\n" (M.run 10)
