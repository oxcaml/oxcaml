(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

let const2 x y = 0
[%%expect{|
val const2 : 'a @ [< global] -> 'b @ 'm -> int @ 'n = <fun>
|}]

let fst2 x y = x
[%%expect{|
val fst2 : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] = <fun>
|}]

let three x y z = (x, z)
[%%expect{|
val three :
  'a @ [< 'm & global] ->
  'b @ [< global] -> 'c @ [< 'n & global] -> 'a * 'c @ [> 'n | 'm] = <fun>
|}]

let pair x y = (x, y)
[%%expect{|
val pair :
  'a @ [< 'm & global] -> 'b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm] =
  <fun>
|}]

let apply f x = f x
[%%expect{|
val apply :
  ('a @ [> 'm] -> 'b @ [< 'n & global]) @ [< global] ->
  'a @ [< 'm] -> 'b @ [> 'n | dynamic] = <fun>
|}]

let compose f g x = f (g x)
[%%expect{|
val compose :
  ('a @ [> 'm | dynamic] -> 'b @ [< 'n & global]) @ [< global] ->
  ('c @ [> 'o] -> 'a @ [< 'm & global]) @ [< global] ->
  'c @ [< 'o] -> 'b @ [> 'n | dynamic] = <fun>
|}]

let flip f x y = f y x
[%%expect{|
val flip :
  ('a @ [> 'm] -> 'b @ [> 'n] -> 'c @ [< 'o & global]) @ [< global] ->
  'b @ [< 'n & global] -> 'a @ [< 'm] -> 'c @ [> 'o | dynamic] = <fun>
|}]

let add a b = a + b
[%%expect{|
val add : int @ 'm -> int @ 'n -> int @ [> dynamic] = <fun>
|}, Principal{|
val add : int @ [< global] -> int @ 'm -> int @ [> dynamic] = <fun>
|}]

let once_closure (x @ once) = fun y -> (x, y)
[%%expect{|
val once_closure :
  'a @ [< 'm & global > once] ->
  'b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm | once] = <fun>
|}]

let portable_closure (x @ portable contended) y = (x, y)
[%%expect{|
val portable_closure :
  'a @ [< 'm & global portable > contended] ->
  'b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm | contended] = <fun>
|}]

type ('a, 'b) pair_record = { a : 'a; b : 'b }
[%%expect{|
type ('a, 'b) pair_record = { a : 'a; b : 'b; }
|}]

let mk_record a b = { a; b }
[%%expect{|
val mk_record :
  'a @ [< 'm & global] ->
  'b @ [< 'n & global] -> ('a, 'b) pair_record @ [> 'n | 'm] = <fun>
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
  ('a @ ([> 'm] as 'n) ->
   'b @ ([< global many uncontended forkable unyielding read_write] as 'o)) @ [< 'p mod aliased contended immutable & global many] ->
  'a @ [< 'm] -> ('a @ 'n -> 'b @ 'o) @ [> 'p | aliased] = <fun>
|}, Principal{|
val use_and_return :
  ('a @ ([> 'm] as 'n) ->
   'b @ ([< global many uncontended forkable unyielding read_write] as 'o)) @ [< 'p & global many] ->
  'a @ [< 'm] -> ('a @ 'n -> 'b @ 'o) @ [> 'p | aliased] = <fun>
|}]

let both_branches g x = if x then g else (fun y -> y)
[%%expect{|
val both_branches :
  ('a @ ([< 'm] as 'n) -> 'a @ ([> 'm] as 'o)) @ [< 'p & global] ->
  bool @ 'q -> ('a @ 'n -> 'a @ 'o) @ [> 'p | dynamic] = <fun>
|}]

type 'a cell = { mutable v : 'a }
[%%expect{|
type 'a cell = { mutable v : 'a; }
|}]

let store_and_call c g x = c.v <- g; c.v x
[%%expect{|
val store_and_call :
  ('a @ ([> 'm] as 'n) -> 'b @ ([< 'o & global] as 'p)) cell @ [< global uncontended read_write] ->
  ('a @ 'n -> 'b @ 'p) @ [< global many uncontended forkable unyielding read_write] ->
  'a @ [< 'm] -> 'b @ [> 'o | dynamic] = <fun>
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
  'a cell @ [< 'm & global unique corrupted write] ->
  'a @ [< global many uncontended forkable unyielding read_write] ->
  'a cell @ [> 'm mod many forkable unyielding] = <fun>
|}, Principal{|
val unique_cell :
  'a cell @ [< 'm & global unique corrupted write] ->
  'a @ [< global many uncontended forkable unyielding read_write] ->
  'a cell @ [> 'm] = <fun>
|}]

let stack_args g = g (stack_ (1, 2)) (stack_ (3, 4)); ()
[%%expect{|
val stack_args :
  (int * int @ [> local] -> int * int @ [> local] -> 'a @ 'm) @ 'n ->
  unit @ 'o = <fun>
|}]

(* CR-soon mode-polymorphism-printing: printing crashes because a curry mode is
   hidden in front of an arrow that is printed as an alias. Fix once aliased
   types are handled properly. *)
let aliased_return (o : < m : 't > as 'o) : 't = fun a b -> ignore (a, b); o
[%%expect{|
>> Fatal error: Out_type.name_of_modes: printing an elided curry mode
Uncaught exception: Misc.Fatal_error

|}]
