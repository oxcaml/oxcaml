(* TEST
   flambda2;
   flags += "-flambda2-reaper -X reaper-escape=1";
   { native; check-program-output; }
 *)

(* Negative: [inner] is stored in [outer], which escapes, so [inner]
   must be heap-allocated too (heap blocks may not point at local ones). *)
let[@inline never] f x y z =
  let inner = (x, y) in
  let outer = (inner, z) in
  outer

let () =
  let (a, b), c = f 1 2 3 in
  Printf.printf "%d %d %d\n" a b c
