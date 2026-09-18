(* Dep_b: depends on Base, uses flambda_o3 optimization *)
[@@@ocaml.flambda_o3]

let () = Printf.printf "Dep_b: initializing\n%!"

(* Some computation that benefits from optimization *)
let rec sum_to n acc =
  if n <= 0 then acc
  else sum_to (n - 1) (acc + n)

let computed_value = sum_to 100 0

let dep_b_value = Base.base_value + computed_value

let () = Printf.printf "Dep_b: initialized with dep_b_value=%d (sum 1..100=%d)\n%!"
  dep_b_value computed_value
