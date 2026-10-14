(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* A record too large for the reaper's unboxing (> reaper_max_unbox_size),
   so only a locality decision can remove the heap allocation. *)
type t =
  { a : int; b : int; c : int; d : int; e : int; f : int;
    g : int; h : int; i : int; j : int; k : int; l : int }

let[@inline never] sum r =
  r.a + r.b + r.c + r.d + r.e + r.f + r.g + r.h + r.i + r.j + r.k + r.l

let[@inline never] f x =
  let r =
    { a = x; b = x + 1; c = x + 2; d = x + 3; e = x + 4; f = x + 5;
      g = x + 6; h = x + 7; i = x + 8; j = x + 9; k = x + 10; l = x + 11 }
  in
  (sum r [@nontail])

let () = Printf.printf "%d\n" (f 1)
