(* Test closures and higher-order functions *)

(* Simple closure capturing a variable *)
let make_adder n x = x + n

(* Closure capturing multiple variables *)
let make_linear a b x = (a * x) + b

(* Higher-order function: map *)
let rec map f = function [] -> [] | x :: xs -> f x :: map f xs

(* Higher-order function: fold_left *)
let rec fold_left f acc = function
  | [] -> acc
  | x :: xs -> fold_left f (f acc x) xs

(* Higher-order function: filter *)
let rec filter p = function
  | [] -> []
  | x :: xs -> if p x then x :: filter p xs else filter p xs

(* Composition *)
let compose f g x = f (g x)

(* Partial application *)
let add x y = x + y

let mul x y = x * y

(* Curried vs uncurried *)
let curried_add x y z = x + y + z

let uncurried_add (x, y, z) = x + y + z

(* Function returning function *)
let twice f x = f (f x)

let thrice f x = f (f (f x))

(* Recursive closure *)
let make_counter () =
  let count = ref 0 in
  fun () ->
    incr count;
    !count

(* Entry point *)
let () =
  let add5 = make_adder 5 in
  let _ = add5 10 in
  let linear = make_linear 2 3 in
  let _ = linear 5 in
  let _ = map (fun x -> x * 2) [1; 2; 3] in
  let _ = fold_left ( + ) 0 [1; 2; 3; 4; 5] in
  let _ = filter (fun x -> x mod 2 = 0) [1; 2; 3; 4; 5; 6] in
  let double x = x * 2 in
  let square x = x * x in
  let _ = compose double square 3 in
  let add10 = add 10 in
  let _ = add10 5 in
  let _ = twice double 3 in
  let counter = make_counter () in
  let _ = counter () in
  let _ = counter () in
  ()
