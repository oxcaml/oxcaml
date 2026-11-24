(* TEST
   flambda2;
   stack-allocation;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* After [mk] has been inlined, the closures of [use1] and [use2] share one
   code. The one of [use1] is only called in its region, but the one of [use2]
   is returned to unknown code, which may partially apply it: the result is a
   heap value for that code, so the curry functions must allocate it on the
   heap, hence the shared code cannot be given the metadata of a local closure,
   and the closure of [use1] stays on the heap. In [outer], the partial
   application made by [ext] is stored in a global while a region is open, and
   called after that region has been closed and its memory reused. *)
let[@inline never] sum7 (t : int * int * int * int * int * int * int) =
  let a, b, c, d, e, f, g = t in
  a + b + c + d + e + f + g

let[@inline never] sum_local (local_ p : int * int) = fst p + snd p

let[@inline never] clobber n =
  let local_ big = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  let local_ big2 = n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7 in
  (sum7 big [@nontail]) + (sum7 big2 [@nontail])

let[@inline] mk a = fun y z -> y + z + a

let[@inline never] apply_local (f : int -> int -> int) = (f 1 2 [@nontail])

let[@inline never] use1 a =
  let h = mk a in
  (apply_local h [@nontail])

let[@inline never] use2 a = mk a

let r : (int -> int) ref = ref (fun x -> x)

let[@inline never] ext () =
  let h = Sys.opaque_identity (use2 100) in
  h 5

let[@inline never] outer n =
  let local_ t = n, n in
  let p = ext () in
  r := p;
  (sum_local t [@nontail])

let () =
  let a = use1 3 in
  let b = outer 4 in
  let c = clobber 5 in
  (Printf.printf [@inlined never]) "%d %d %d %d\n" a b c (!r 7)
