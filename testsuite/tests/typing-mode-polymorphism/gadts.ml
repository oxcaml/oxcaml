(* TEST
 flags += "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

type _ t = A : bool t

let id x = x

let refine (type output) (w : output t) =
  match w with
  | A -> (id : bool -> output)
[%%expect{|
type _ t = A : bool t
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
val refine :
  'output t @ 'm -> (bool -> 'output) @ [> nonportable stateful dynamic] =
  <fun>
|}]

type _ arr = F : (int -> int) arr

let apply (type a) (w : a arr) (g : a) =
  match w with
  | F -> g 3
[%%expect{|
type _ arr = F : (int -> int) arr
val apply : 'a arr @ 'o -> ('a @ 'n -> int @ [> dynamic]) @ 'm = <fun>
|}, Principal{|
type _ arr = F : (int -> int) arr
Line 5, characters 9-12:
5 |   | F -> g 3
             ^^^
Error: This expression has type "int" but an expression was expected of type "'a"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}]

let pin (w : 'x arr) (g : 'x) =
  match w with
  | F -> g
[%%expect{|
val pin :
  (int -> int) arr @ 'o ->
  ((int -> int) @ [< 'n . aliased contended immutable] ->
   (int -> int) @ [> 'n]) @ 'm =
  <fun>
|}, Principal{|
val pin :
  (int -> int) arr @ [< 'm @@ past & global] ->
  ((int -> int) @ [< 'n] -> (int -> int) @ [> 'n]) @ [> 'm] = <fun>
|}]

type _ dom = L : (string @ local -> int) dom | G : (string -> int) dom

let local_arg_ok (type a) (w : a dom) (g : a) (s : string @ local) =
  match w with
  | L -> g s
  | G -> 0
[%%expect{|
type _ dom = L : (string @ local -> int) dom | G : (string -> int) dom
val local_arg_ok :
  'a dom @ 'o ->
  ('a @ [< 'n @@ past & global] ->
   (string @ [> local unforkable yielding] -> int @ [> dynamic]) @ [> 'n]) @ 'm =
  <fun>
|}, Principal{|
type _ dom = L : (string @ local -> int) dom | G : (string -> int) dom
Line 5, characters 9-12:
5 |   | L -> g s
             ^^^
Error: This expression has type "int" but an expression was expected of type "'a"
       This instance of "int" is ambiguous:
       it would escape the scope of its equation
|}]

let local_arg_bad (type a) (w : a dom) (g : a) (s : string @ local) =
  match w with
  | L -> 0
  | G -> g s
[%%expect{|
Line 4, characters 11-12:
4 |   | G -> g s
               ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

type _ cross = Int : int cross | Str : string cross

let crosses (type a) (w : a cross) (x : a @ local) : a @ global =
  match w with
  | Int -> x
  | Str -> assert false
[%%expect{|
type _ cross = Int : int cross | Str : string cross
val crosses :
  'a cross @ 'n ->
  ('a @ [> local unforkable yielding] ->
   'a @ [< global forkable unyielding > dynamic]) @ 'm =
  <fun>
|}, Principal{|
type _ cross = Int : int cross | Str : string cross
val crosses :
  'a cross @ [< 'm @@ past & global] ->
  ('a @ [> local unforkable yielding] ->
   'a @ [< global forkable unyielding > dynamic]) @ [> 'm] =
  <fun>
|}]

let escapes (type a) (w : a cross) (x : a @ local) : a @ global =
  match w with
  | Int -> assert false
  | Str -> x
[%%expect{|
Line 4, characters 11-12:
4 |   | Str -> x
               ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

type packed = P : (int -> int) -> packed

let pack = P id

let unpack (P f) = f
[%%expect{|
type packed = P : (int -> int) -> packed
val pack : packed = P <fun>
val unpack :
  packed @ [< 'm . aliased contended immutable] -> (int -> int) @ [> 'm] =
  <fun>
|}]
