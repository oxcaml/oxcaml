(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -experimental-reaper-escape -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* A value leaving a region scope only escapes if it may be allocated in the
   region of that scope. [mk] returns at mode local a block containing another
   block that it allocates: both land in the region of the caller. In [leak],
   the inner block is then passed to a tail call, before which the region of
   [leak] is closed: it must not be local, although [leak] only receives it
   inside the outer block. In [keep] it is passed to a call that is not a tail
   call, and both blocks are local. [clobber] reuses the memory of the regions
   that have been closed. *)
let[@inline never] sum_pair (p : int * int) = fst p + snd p

let[@inline never] sum7 (t : int * int * int * int * int * int * int) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] clobber n =
  let big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  (sum7 big [@nontail])

let[@inline never] clobber_then_sum n (p : int * int) =
  let c = clobber n in
  c - c + sum_pair p

module M : sig
  val leak : int -> int

  val keep : int -> int
end = struct
  let[@inline never] mk x =
    let inner = x, x + 1 in
    inner, x + 2

  let[@inline never] mk2 x =
    let inner = x, x + 1 in
    inner, x + 2

  let[@inline never] leak x =
    let outer = mk x in
    let inner = fst outer in
    clobber_then_sum x inner

  let[@inline never] keep x =
    let outer = mk2 x in
    let inner = fst outer in
    (clobber_then_sum x inner [@nontail]) + snd outer
end

let () = (Printf.printf [@inlined never]) "%d %d\n" (M.leak 10) (M.keep 10)
