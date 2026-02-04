(* ocamlopt.opt -flambda2-reaper -reaper-local-fields -o indirect_params.ml && ./indirect_params *)

let f x = let () = () in fun y -> x + y
let[@inline never][@local never] h x y =
  let[@inline never][@local never] g f = f x y in
  g f

let () = assert (h 1 2 = 3)