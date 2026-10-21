(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1 -reaper-debug-flags=nostamps";
   { native with dump-reaper; check-fexpr-dump; }
 *)

(* Mutable record: the stores into it currently make it escape (a known
   limitation of the analysis), so it stays on the heap. *)
type t = { mutable a : int; mutable b : int }

let[@inline never] bump r =
  r.a <- r.a + 1;
  r.b <- r.b + r.a

let[@inline never] f x =
  let r = { a = x; b = 0 } in
  bump r;
  bump r;
  r.a + r.b

let () = (Printf.printf [@inlined never]) "%d\n" (f 1)
