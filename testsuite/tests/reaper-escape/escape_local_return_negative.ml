(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* Negative: [mk] is also called from a site that lets the result escape, so
   it must keep returning on the heap. *)
module M : sig
  val f : int -> int -> int
  val g : int -> int -> unit
  val cell : (int * int) ref
end = struct
  let[@inline never] mk x y = (x, y)
  let[@inline never] f x y =
    let p = mk x y in
    fst p + snd p
  let cell = ref (0, 0)
  let[@inline never] g x y = cell := mk x y
end
let () =
  M.g 5 6;
  let a, b = !M.cell in
  (Printf.printf [@inlined never]) "%d %d %d\n" (M.f 3 4) a b
