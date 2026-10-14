(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* Mutable record: stores into it must not make it escape. *)
type t = { mutable a : int; mutable b : int }

let[@inline never] bump r =
  r.a <- r.a + 1;
  r.b <- r.b + r.a

let[@inline never] f x =
  let r = { a = x; b = 0 } in
  bump r;
  bump r;
  r.a + r.b

let () = Printf.printf "%d\n" (f 1)
