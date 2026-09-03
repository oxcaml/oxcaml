(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

let const2 x y = 0
[%%expect{|
val const2 :
  'a @ [< past('m) & global] -> ('b @ 'o -> int @ 'n) @ [> past('m)] = <fun>
|}]

let fst2 x y = x
[%%expect{|
val fst2 : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]

let three x y z = (x, z)
[%%expect{|
val three :
  'a @ [< 'm & global] ->
  ('b @ [< past('n) & global] ->
   ('c @ [< 'o & global] -> 'a * 'c @ [> 'o | 'm]) @ [> close('m) | past('n)]) @ [> close('m)] =
  <fun>
|}]

let pair x y = (x, y)
[%%expect{|
val pair :
  'a @ [< 'm & global] ->
  ('b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm]) @ [> close('m)] = <fun>
|}]

let apply f x = f x
[%%expect{|
val apply :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< past('o) & global] ->
  ('a @ [< 'n] -> 'b @ [> 'm | dynamic]) @ [> past('o)] = <fun>
|}]

let compose f g x = f (g x)
[%%expect{|
val compose :
  ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< past('mm0) & past('o) & global] ->
  (('c @ [> 'p] -> 'a @ [< 'n & global]) @ [< past('q) & global] ->
   ('c @ [< 'p] -> 'b @ [> 'm | dynamic]) @ [> past('q) | past('mm0)]) @ [> past('o)] =
  <fun>
|}]

let flip f x y = f y x
[%%expect{|
val flip :
  ('a @ [< past('m) > 'q] ->
   ('b @ [> 'p] -> 'c @ [< 'o & global]) @ [> past('m) | past('n)]) @ [< past('mm1) & past('n) & past('mm0) & global] ->
  ('b @ [< 'p & global] ->
   ('a @ [< 'q] -> 'c @ [> 'o | dynamic]) @ [> close('p) | past('mm1)]) @ [> past('mm0)] =
  <fun>
|}]

let add a b = a + b
[%%expect{|
val add : int @ 'n -> (int @ 'm -> int @ [> dynamic]) @ [> stateful] = <fun>
|}, Principal{|
val add :
  int @ [< past('m) & global] ->
  (int @ 'n -> int @ [> dynamic]) @ [> past('m) | stateful] = <fun>
|}]

let once_closure (x @ once) = fun y -> (x, y)
[%%expect{|
val once_closure :
  'a @ [< 'm & global > once] ->
  ('b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm | once]) @ [> close('m) | once] =
  <fun>
|}]

let portable_closure (x @ portable contended) y = (x, y)
[%%expect{|
val portable_closure :
  'a @ [< 'm & global portable > contended] ->
  ('b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm | contended]) @ [> close('m)] =
  <fun>
|}]

type ('a, 'b) pair_record = { a : 'a; b : 'b }
[%%expect{|
type ('a, 'b) pair_record = { a : 'a; b : 'b; }
|}]

let mk_record a b = { a; b }
[%%expect{|
val mk_record :
  'a @ [< 'm & global] ->
  ('b @ [< 'n & global] -> ('a, 'b) pair_record @ [> 'n | 'm]) @ [> close('m)] =
  <fun>
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
  ('a @ [> 'm] -> 'b @ [< global many read_write]) @ [< 'o mod aliased contended immutable & past('n) & global many] ->
  ('a @ [< 'm] ->
   ('a @ [> 'm] -> 'b @ [< global many read_write]) @ [> 'o | aliased]) @ [> past('n) | stateful] =
  <fun>
|}, Principal{|
val use_and_return :
  ('a @ [> 'm] -> 'b @ [< global many read_write]) @ [< 'n & global many] ->
  ('a @ [< 'm] ->
   ('a @ [> 'm] -> 'b @ [< global many read_write]) @ [> 'n | aliased]) @ [> close('n) | stateful] =
  <fun>
|}]

let both_branches g x = if x then g else (fun y -> y)
[%%expect{|
val both_branches :
  ('a @ [< 'm] -> 'a @ [> 'm]) @ [< 'o & past('n) & global] ->
  (bool @ 'p -> ('a @ [< 'm] -> 'a @ [> 'm]) @ [> 'o | dynamic]) @ [> past('n)] =
  <fun>
|}, Principal{|
val both_branches :
  ('a @ [< 'm] -> 'a @ [> 'm]) @ [< 'n & global] ->
  (bool @ 'o -> ('a @ [< 'm] -> 'a @ [> 'm]) @ [> 'n | dynamic]) @ [> close('n)] =
  <fun>
|}]

type 'a cell = { mutable v : 'a }
[%%expect{|
type 'a cell = { mutable v : 'a; }
|}]

let store_and_call c g x = c.v <- g; c.v x
[%%expect{|
val store_and_call :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) cell @ [< past('p) & global read_write] ->
  (('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< past('o) & global many read_write] ->
   ('a @ [< 'n] -> 'b @ [> 'm | dynamic]) @ [> past('q) | past('mm0) mod many forkable unyielding | stateful]) @ [> past('o) | past('p) mod many forkable unyielding | stateful] =
  <fun>
|}, Principal{|
val store_and_call :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) cell @ [< past('p) & global read_write] ->
  (('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< past('o) & global many read_write] ->
   ('a @ [< 'n] -> 'b @ [> 'm | dynamic]) @ [> past('q) | past('mm0) | stateful]) @ [> past('o) | past('p) | stateful] =
  <fun>
|}]

let unique_fst (x @ unique) y = x
[%%expect{|
val unique_fst :
  'a @ [< 'm & global unique] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]

let unique_closure (x @ unique) = fun y -> x
[%%expect{|
val unique_closure :
  'a @ [< 'm & global unique] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]

let unique_cell (c @ unique) x = c.v <- x; c
[%%expect{|
val unique_cell :
  'a cell @ [< 'm & global unique write] ->
  ('a @ [< global many read_write] ->
   'a cell @ [> 'm mod many forkable unyielding]) @ [> close('m) mod many | writing] =
  <fun>
|}, Principal{|
val unique_cell :
  'a cell @ [< 'm & global unique write] ->
  ('a @ [< global many read_write] -> 'a cell @ [> 'm]) @ [> close('m) | writing] =
  <fun>
|}]

let stack_args g = g (stack_ (1, 2)) (stack_ (3, 4)); ()
[%%expect{|
val stack_args :
  (int * int @ [> local] -> int * int @ [> local] -> 'a @ 'm) @ 'o ->
  unit @ 'n = <fun>
|}]
