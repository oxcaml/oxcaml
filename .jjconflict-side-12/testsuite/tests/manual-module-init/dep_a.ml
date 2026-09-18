(* Dep_a: depends on Base, has a non-constant toplevel closure *)
let () = Printf.printf "Dep_a: initializing\n%!"

(* Initialize random with fixed seed for deterministic tests *)
let () = Random.init 42

(* Non-constant toplevel value - requires patching of module block *)
let random_seed = Random.int 1000

(* Closure that captures the random seed *)
let add_seed x = x + random_seed

let dep_a_value = Base.base_value + 10

let () = Printf.printf "Dep_a: initialized with dep_a_value=%d, random_seed=%d\n%!"
  dep_a_value random_seed
