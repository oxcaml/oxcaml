(* Diamond: depends on both Dep_a and Dep_b (diamond dependency on Base) *)
let () = Printf.printf "Diamond: initializing\n%!"

(* Use values from both dependencies *)
let combined = Dep_a.dep_a_value + Dep_b.dep_b_value

(* Use the closure from Dep_a *)
let with_seed = Dep_a.add_seed 42

let () = Printf.printf "Diamond: initialized with combined=%d, with_seed=%d\n%!"
  combined with_seed
