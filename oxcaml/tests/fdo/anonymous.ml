(* Anonymous functions are named by their enclosing scope and their first tokens
   (parameters, then the body's identifiers, primitives and constants, without
   repetitions, at most five), not by their position among the scope's other
   anonymous functions, so inserting one does not rename the others:
   anonymous_more.ml is this file with an extra anonymous function at the start
   of each scope, compiled under the same module name; anonymous.output shows
   the counters of both. *)

let[@inline never] apply f x = f x

let[@inline never] twice f x = f (f x)

let a = apply (fun x -> x + 1) 1

let b = twice (fun y -> y * 2) 2

let[@inline never] inner n =
  let p = apply (fun x -> x - n) n in
  let q = twice (fun y -> y + n) p in
  let r = apply (fun y x -> x + y + n + y + p + q + a) p q in
  p + q + r

let () = Printf.printf "%d\n" (a + b + inner 3)
