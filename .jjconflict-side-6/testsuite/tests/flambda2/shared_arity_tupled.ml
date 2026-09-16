(* TEST
 ocamlopt_flags = "-O3";
 flambda2;
 native;
*)
let f1 (x, (y1, y2)) = 
  let () = () in
  fun z -> x + y1 + y2 + z

let f2 (x, (y1, y2)) z = x + y1 + y2 + z

let[@inline never] test b x y1 y2 z = (if b then f1 else f2) (x, (y1, y2)) z

let n = test true 0 1 2 3