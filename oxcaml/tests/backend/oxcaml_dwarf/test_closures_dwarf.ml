let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

(* Inline closure tests - anonymous functions passed as arguments *)
let[@inline never] [@local never] f_inline_closure_map (lst : int list)
    (f : int -> int) =
  match lst with [] -> [] | x :: xs -> f x :: List.map f xs

let _ = f_inline_closure_map [1; 2; 3] (fun x -> x * 2)

let _ = f_inline_closure_map [4; 5; 6] (fun y -> y + 10)

let[@inline never] [@local never] f_inline_closure_filter (lst : int list)
    (pred : int -> bool) =
  match lst with
  | [] -> []
  | x :: xs when pred x -> x :: List.filter pred xs
  | _ :: xs -> List.filter pred xs

let _ = f_inline_closure_filter [1; 2; 3; 4; 5] (fun x -> x > 2)

let _ = f_inline_closure_filter [10; 20; 30] (fun n -> n mod 10 = 0)

(* Named closure tests - named functions passed as arguments with captured
   variables *)
let[@inline never] [@local never] f_named_closure_with_capture (base : int)
    (lst : int list) (f : int -> int) =
  let _ = base in
  List.map f lst

let add_base base x = x + base

let multiply_by_factor factor y = y * factor

let _ = f_named_closure_with_capture 100 [1; 2; 3] (add_base 5)

let _ = f_named_closure_with_capture 200 [4; 5; 6] (multiply_by_factor 3)

(* Closure with captured environment *)
let[@inline never] [@local never] f_closure_with_env (outer : int)
    (inner : string) =
  let captured = outer + String.length inner in
  let make_adder increment =
    let closure x = x + captured + increment in
    closure
  in
  let adder = make_adder 10 in
  adder 42

let _ = f_closure_with_env 100 "hello"

let _ = f_closure_with_env 50 "world"

(* Higher-order function tests - functions that take and return functions *)
let[@inline never] [@local never] f_higher_order
    (transformer : (int -> int) -> int -> int) (base_func : int -> int)
    (value : int) =
  let new_func = transformer base_func in
  new_func value

let compose_with_double f =
  let closure x = f (x * 2) in
  closure

let compose_with_increment f =
  let closure y = f (y + 1) in
  closure

let square x = x * x

let negate x = -x

let _ = f_higher_order compose_with_double square 5

let _ = f_higher_order compose_with_increment negate 10

(* Partial application tests *)
let[@inline never] [@local never] f_partial_application (a : int) (b : int)
    (c : int) =
  a + b + c

let partial_func = f_partial_application 10 20

let _ = partial_func 30

let _ = partial_func 40

let[@inline never] [@local never] f_curried_function (x : int) (y : int)
    (z : int) (w : int) =
  (x * y) + (z * w)

let partially_applied = f_curried_function 2 3

let more_partial = partially_applied 4

let _ = more_partial 5

let _ = more_partial 6

(* Function composition with closures *)
let[@inline never] [@local never] f_compose (f : int -> int) (g : int -> int)
    (x : int) =
  f (g x)

let add_one n = n + 1

let double n = n * 2

let _ = f_compose double add_one 5

let _ = f_compose add_one double 7

(* Closure returning closure *)
let[@inline never] [@local never] f_closure_factory (multiplier : int) =
  let closure1 adder =
    let closure2 value = (value * multiplier) + adder in
    closure2
  in
  closure1

let factory_result = f_closure_factory 3

let nested_closure = factory_result 10

let _ = nested_closure 5

let _ = nested_closure 8
