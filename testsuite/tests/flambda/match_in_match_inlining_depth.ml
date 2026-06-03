(* TEST
   flambda;
   ocamlopt_flags = "-flambda2-inline-max-depth 1 -flambda2-expert-cont-lifting-budget 200 -flambda2-expert-cont-specialization-budget 20";
   native;
*)

let[@inline] incr x = x + 1

let[@inline] map f = function
  | None -> None
  | Some x -> Some (f x)

let[@inline] id f x = f x

let[@inline] _match = function
  | None -> 0
  | Some x -> x

let[@inline] test t =
  let f = id (id _match) in
  let y = map incr t in
  f y

