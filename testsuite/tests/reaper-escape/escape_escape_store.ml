(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* Negative: the pair is stored into a global (static) mutable cell. *)
let cell = ref (0, 0)

let[@inline never] f x y =
  let p = (x, y) in
  cell := p

let () =
  f 5 6;
  let a, b = !cell in
  Printf.printf "%d %d\n" a b
