(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

let const2 x y = 0
[%%expect{|
val const2 : 'a @ [< global] -> 'b @ 'n -> int @ 'm = <fun>
|}]

let fst2 x y = x
[%%expect{|
val fst2 : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] = <fun>
|}]

let three x y z = (x, z)
[%%expect{|
val three :
  'a @ [< 'n & global] ->
  'b @ [< global] -> 'c @ [< 'm & global] -> 'a * 'c @ [> 'm | 'n] = <fun>
|}]

let pair x y = (x, y)
[%%expect{|
val pair :
  'a @ [< 'n & global] -> 'b @ [< 'm & global] -> 'a * 'b @ [> 'm | 'n] =
  <fun>
|}]

let apply f x = f x
[%%expect{|
val apply :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< global] ->
  'a @ [< 'n] -> 'b @ [> 'm | dynamic] = <fun>
|}]

let compose f g x = f (g x)
[%%expect{|
val compose :
  ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< global] ->
  ('c @ [> 'o] -> 'a @ [< 'n & global]) @ [< global] ->
  'c @ [< 'o] -> 'b @ [> 'm | dynamic] = <fun>
|}]

let flip f x y = f y x
[%%expect{|
val flip :
  ('a @ [> 'o] -> 'b @ [> 'n] -> 'c @ [< 'm & global]) @ [< global] ->
  'b @ [< 'n & global] -> 'a @ [< 'o] -> 'c @ [> 'm | dynamic] = <fun>
|}]

let add a b = a + b
[%%expect{|
val add : int @ 'n -> int @ 'm -> int @ [> dynamic] = <fun>
|}, Principal{|
val add : int @ [< global] -> int @ 'm -> int @ [> dynamic] = <fun>
|}]

let once_closure (x @ once) = fun y -> (x, y)
[%%expect{|
val once_closure :
  'a @ [< 'n & global > once] ->
  'b @ [< 'm & global] -> 'a * 'b @ [> 'm | 'n | once] = <fun>
|}]

let portable_closure (x @ portable contended) y = (x, y)
[%%expect{|
val portable_closure :
  'a @ [< 'n & global portable > contended] ->
  'b @ [< 'm & global] -> 'a * 'b @ [> 'm | 'n | contended] = <fun>
|}]

type ('a, 'b) pair_record = { a : 'a; b : 'b }
[%%expect{|
type ('a, 'b) pair_record = { a : 'a; b : 'b; }
|}]

let mk_record a b = { a; b }
[%%expect{|
val mk_record :
  'a @ [< 'n & global] ->
  'b @ [< 'm & global] -> ('a, 'b) pair_record @ [> 'm | 'n] = <fun>
|}]

let local_closure x = exclave_ (fun y -> (x, y))
[%%expect{|
val local_closure :
  'a @ [< 'm & global] ->
  ('b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm]) @ [> close('m) | local] =
  <fun>
|}]

let use_and_return g x = ignore (g x); g
[%%expect{|
val use_and_return :
  ('a @ [> 'm] -> 'b @ [< global many read_write]) @ [< 'n mod aliased contended immutable & global many] ->
  'a @ [< 'm] ->
  ('a @ [> 'm] -> 'b @ [< global many read_write]) @ [> 'n | aliased] = <fun>
|}, Principal{|
val use_and_return :
  ('a @ [> 'm] -> 'b @ [< global many read_write]) @ [< 'n & global many] ->
  'a @ [< 'm] ->
  ('a @ [> 'm] -> 'b @ [< global many read_write]) @ [> 'n | aliased] = <fun>
|}]

let both_branches g x = if x then g else (fun y -> y)
[%%expect{|
val both_branches :
  ('a @ [< 'm] -> 'a @ [> 'm]) @ [< 'n & global] ->
  bool @ 'o -> ('a @ [< 'm] -> 'a @ [> 'm]) @ [> 'n | dynamic] = <fun>
|}]

type 'a cell = { mutable v : 'a }
[%%expect{|
type 'a cell = { mutable v : 'a; }
|}]

let store_and_call c g x = c.v <- g; c.v x
[%%expect{|
val store_and_call :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) cell @ [< past('mm0) & past('p) & global read_write] ->
  (('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< past('q) & past('o) & global many read_write] ->
   ('a @ [< 'n] -> 'b @ [> 'm | dynamic]) @ [> past('q) | past('mm0) mod many forkable unyielding | stateful]) @ [> past('o) | past('p) mod many forkable unyielding | stateful] =
  <fun>
|}, Principal{|
val store_and_call :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) cell @ [< past('mm0) & past('p) & global read_write] ->
  (('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< past('q) & past('o) & global many read_write] ->
   ('a @ [< 'n] -> 'b @ [> 'm | dynamic]) @ [> past('q) | past('mm0) | stateful]) @ [> past('o) | past('p) | stateful] =
  <fun>
|}]

let unique_fst (x @ unique) y = x
[%%expect{|
val unique_fst : 'a @ [< 'm & global unique] -> 'b @ 'n -> 'a @ [> 'm] =
  <fun>
|}]

let unique_closure (x @ unique) = fun y -> x
[%%expect{|
val unique_closure : 'a @ [< 'm & global unique] -> 'b @ 'n -> 'a @ [> 'm] =
  <fun>
|}]

let unique_cell (c @ unique) x = c.v <- x; c
[%%expect{|
val unique_cell :
  'a cell @ [< 'm & global unique write] ->
  'a @ [< global many read_write] ->
  'a cell @ [> 'm mod many forkable unyielding] = <fun>
|}, Principal{|
val unique_cell :
  'a cell @ [< 'm & global unique write] ->
  'a @ [< global many read_write] -> 'a cell @ [> 'm] = <fun>
|}]

let stack_args g = g (stack_ (1, 2)) (stack_ (3, 4)); ()
[%%expect{|
val stack_args :
  (int * int @ [> local] -> int * int @ [> local] -> 'a @ 'm) @ 'o ->
  unit @ 'n = <fun>
|}]
