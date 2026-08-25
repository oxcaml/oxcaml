(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

(*
 * This file tests printing of poymorphic mode variables
*)


let id x = x
[%%expect{|
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

let foo x = 42
[%%expect{|
val foo : 'a @ 'n -> int @ 'm = <fun>
|}]

let foo x = id x
[%%expect{|
val foo : 'a @ [< 'm & global] -> 'a @ [> 'm | dynamic] = <fun>
|}]

let foo f x = f x
[%%expect{|
val foo :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< global] ->
  'a @ [< 'n] -> 'b @ [> 'm | dynamic] = <fun>
|}, Principal{|
val foo :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< global] ->
  'a @ [< 'n] -> 'b @ [> 'm | dynamic] = <fun>
|}]

let foo =
  let id x = x in
  fun x -> id x
[%%expect{|
val foo : 'a @ [< 'm & global] -> 'a @ [> 'm | dynamic] = <fun>
|}]

let foo a b = a + b
[%%expect{|
val foo : int @ 'n -> int @ 'm -> int @ [> dynamic] = <fun>
|}, Principal{|
val foo : int @ [< global] -> int @ 'm -> int @ [> dynamic] = <fun>
|}]


(* records *)

type ('a,'b) mytypemod = { x : 'a; y : 'b @@ portable }

let foo t = t.x
[%%expect{|
type ('a, 'b) mytypemod = { x : 'a; y : 'b @@ portable; }
val foo : ('a, 'b) mytypemod @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

let foo t = t.y
[%%expect{|
val foo : ('a, 'b) mytypemod @ [< 'm] -> 'b @ [> 'm mod portable] = <fun>
|}]

let foo x z = x.y
[%%expect{|
val foo :
  ('a, 'b) mytypemod @ [< 'm & global] -> 'c @ 'n -> 'b @ [> 'm mod portable] =
  <fun>
|}]


let x =
  let foo x = x in
  let _ @ contended = foo (ref 42 : _ @ contended ) in
  let _ @ uncontended = foo  (ref 41 : _ @ uncontended) in
  foo
[%%expect{|
val x : '_weak1 -> '_weak1 @ [> aliased stateful dynamic] = <fun>
|}]

type ('a,'b) mytype = { x : 'a; y : 'b }
[%%expect{|
type ('a, 'b) mytype = { x : 'a; y : 'b; }
|}]

let foo x y = { x; y }
[%%expect{|
val foo :
  'a @ [< 'n & global] ->
  'b @ [< 'm & global] -> ('a, 'b) mytype @ [> 'm | 'n] = <fun>
|}]

let foo x = fun y -> { x; y }
[%%expect{|
val foo :
  'a @ [< 'n & global] ->
  'b @ [< 'm & global] -> ('a, 'b) mytype @ [> 'm | 'n] = <fun>
|}]

let foo x = { x; y = 42 }
[%%expect{|
val foo : 'a @ [< 'm & global] -> ('a, int) mytype @ [> 'm] = <fun>
|}]

let foo r = { r with y = 42 }
[%%expect{|
val foo : ('a, 'b) mytype @ [< 'm & global] -> ('a, int) mytype @ [> 'm] =
  <fun>
|}]

type 'a myref = { mutable x : 'a }
[%%expect{|
type 'a myref = { mutable x : 'a; }
|}]

let create a = { x = a }
[%%expect{|
val create :
  'a @ [< 'm mod aliased dynamic & global many] ->
  'a myref @ [> 'm | stateful] = <fun>
|}]

let read r = r.x
[%%expect{|
val read :
  'a myref @ [< 'm & read] ->
  'a @ [> 'm mod global many forkable unyielding | aliased dynamic] = <fun>
|}]

let store r = fun a -> r.x <- a
[%%expect{|
val store :
  'a myref @ [< global write] -> 'a @ [< global many read_write] -> unit @ 'm =
  <fun>
|}]

(* products *)

let dupl x = (x, x)
[%%expect{|
val dupl : 'a @ [< 'm & global many] -> 'a * 'a @ [> 'm | aliased] = <fun>
|}]

let prod x y = (x, y)
[%%expect{|
val prod :
  'a @ [< 'n & global] -> 'b @ [< 'm & global] -> 'a * 'b @ [> 'm | 'n] =
  <fun>
|}]

let prod_eta x = fun y -> (x, y)
[%%expect{|
val prod_eta :
  'a @ [< 'n & global] -> 'b @ [< 'm & global] -> 'a * 'b @ [> 'm | 'n] =
  <fun>
|}]

let fst (a, _) = a
let snd (_, b) = b
[%%expect{|
val fst : 'a * 'b @ [< 'm] -> 'a @ [> 'm] = <fun>
val snd : 'a * 'b @ [< 'm] -> 'b @ [> 'm] = <fun>
|}]

let foo x = fun y ->
  let x' = fst (x,y) in
  let y' = snd (x,y) in
  (x', y')
[%%expect{|
val foo :
  'a @ [< 'n & global many] ->
  'b @ [< 'm & global many] -> 'a * 'b @ [> 'm | 'n | aliased dynamic] =
  <fun>
|}]

(* currying *)

let foo x y = x
[%%expect{|
val foo : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] = <fun>
|}]

let foo x y = y
[%%expect{|
val foo : 'a @ [< global] -> 'b @ [< 'm] -> 'b @ [> 'm] = <fun>
|}]

let id x = x
let foo x y = id x
[%%expect{|
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
val foo : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm | dynamic] = <fun>
|}]

let foo f = fun x -> fun y -> f x y
[%%expect{|
val foo :
  ('a @ [> 'o] -> 'b @ [> 'n] -> 'c @ [< 'm & global]) @ [< global] ->
  'a @ [< 'o & global] -> 'b @ [< 'n] -> 'c @ [> 'm | dynamic] = <fun>
|}]

let fst x = fun y -> x
[%%expect{|
val fst : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] = <fun>
|}]
let snd x = fun y -> y
[%%expect{|
val snd : 'a @ 'n -> 'b @ [< 'm] -> 'b @ [> 'm] = <fun>
|}]

let foo x y = ref x
[%%expect{|
val foo :
  'a @ [< global many read_write] ->
  'b @ 'm -> 'a ref @ [> aliased stateful dynamic] = <fun>
|}]

let foo (x @ aliased) y = ref x
[%%expect{|
val foo :
  'a @ [< global many read_write > aliased] ->
  'b @ 'm -> 'a ref @ [> aliased stateful dynamic] = <fun>
|}]

let foo (x @ contended) y = x
[%%expect{|
val foo :
  'a @ [< 'm & global > contended] -> 'b @ 'n -> 'a @ [> 'm | contended] =
  <fun>
|}]

let foo x y z = 42
[%%expect{|
val foo : 'a @ [< global] -> 'b @ [< global] -> 'c @ 'n -> int @ 'm = <fun>
|}]

let foo x y = (x, y)
[%%expect{|
val foo :
  'a @ [< 'n & global] -> 'b @ [< 'm & global] -> 'a * 'b @ [> 'm | 'n] =
  <fun>
|}]

let foo x y z = (y,z)
[%%expect{|
val foo :
  'a @ [< global] ->
  'b @ [< 'n & global] -> 'c @ [< 'm & global] -> 'b * 'c @ [> 'm | 'n] =
  <fun>
|}]

(* annotations *)

let legacy_id : 'a -> 'a = fun x -> x
[%%expect{|
val legacy_id : 'a -> 'a = <fun>
|}]

(* CR mode-poly-printing: apply "X mode implies Y mode" logic to bounds *)
let foo (x @ local) = x
[%%expect{|
val foo : 'a @ [< 'm > local] -> 'a @ [> 'm | local] = <fun>
|}]

let foo x = exclave_ x
[%%expect{|
val foo : 'a @ [< 'm] -> 'a @ [> 'm | local] = <fun>
|}]

let foo (x @ portable) = x
[%%expect{|
val foo : 'a @ [< 'm & portable] -> 'a @ [> 'm] = <fun>
|}]

let foo : (unit -> unit) @ portable = fun () -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo (y @ unique) (z @ portable) = z
[%%expect{|
val foo : 'a @ [< global unique] -> 'b @ [< 'm & portable] -> 'b @ [> 'm] =
  <fun>
|}]

let foo (x @ local) (y @ unique) (z @ portable) = exclave_ (x, y, z)
[%%expect{|
val foo :
  'a @ [< 'n > local] ->
  'b @ [< 'm & unique] ->
  ('c @ [< 'o & portable] -> 'a * 'b * 'c @ [> 'o | 'm | 'n | local]) @ [> close('m) | close('n) | local] =
  <fun>
|}]

(* if a type is annotated, mode crossing has an effect on the bounds of mode variable *)

type intref = { mutable v : int }

let foo (x : intref) (f : intref @ local -> int) = f x
[%%expect{|
type intref = { mutable v : int; }
val foo :
  intref @ [< global read_write] ->
  (intref @ local -> int) @ 'm -> int @ [> dynamic] = <fun>
|}]

let foo (f : int -> int) x y = f
[%%expect{|
val foo :
  (int -> int) @ [< 'm mod aliased contended immutable & global] ->
  'a @ [< global] -> 'b @ 'n -> (int -> int) @ [> 'm] = <fun>
|}, Principal{|
val foo :
  (int -> int) @ [< 'm mod aliased contended immutable & global] ->
  'a @ [< global] -> 'b @ 'n -> (int -> int) @ [> 'm] = <fun>
|}]

let foo (f : intref @ local -> int) (x : intref) (y : intref) = f x
[%%expect{|
val foo :
  (intref @ local -> int) @ [< global] ->
  intref @ [< global read_write] -> intref @ 'm -> int @ [> dynamic] = <fun>
|}, Principal{|
val foo :
  (intref @ local -> int) @ [< global] ->
  intref @ [< global read_write] -> intref @ 'm -> int @ [> dynamic] = <fun>
|}]

(* aliases of non-polymorphic functions *)

let map = List.map
[%%expect{|
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
|}]

let map f l = List.map f l
[%%expect{|
val map :
  ('a @ [> past('m) | aliased stateful dynamic] ->
   'b @ [< global many read_write]) @ [< past('n) & past('m) & global many] ->
  'a list @ [< global many read_write] ->
  'b list @ [> past('n) | aliased stateful dynamic] = <fun>
|}, Principal{|
val map :
  ('a @ [> past('m) | aliased stateful dynamic] ->
   'b @ [< global many read_write]) @ [< past('n) & past('m) & global many] ->
  'a list @ [< global many read_write] ->
  'b list @ [> past('n) | aliased stateful dynamic] = <fun>
|}]

let map_eta f = fun l -> List.map f l
[%%expect{|
val map_eta :
  ('a @ [> past('m) | aliased stateful dynamic] ->
   'b @ [< global many read_write]) @ [< past('n) & past('m) & global many] ->
  'a list @ [< global many read_write] ->
  'b list @ [> past('n) | aliased stateful dynamic] = <fun>
|}]

(* modules *)

 module Counter : sig
  type t

  val incr : t -> t

  val to_int : t -> int
end = struct
  type t = int

  let incr n = n + 1

  let to_int = fun n -> n
 end
 [%%expect{|
module Counter : sig type t val incr : t -> t val to_int : t -> int end
|}]

let incr n = Counter.incr n
[%%expect{|
val incr :
  Counter.t @ [< global many read_write] ->
  Counter.t @ [> aliased stateful dynamic] = <fun>
|}]

let incr = Counter.incr
[%%expect{|
val incr : Counter.t -> Counter.t = <fun>
|}]

let incr n = n + 1
[%%expect{|
val incr : int @ 'm -> int @ [> dynamic] = <fun>
|}]

let id x = x
[%%expect{|
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

module Foo : sig
  type t

  val id_portable : t @ portable -> t @ portable

  val id_nonportable : t -> t

  val bar : t @ portable -> t
end = struct
  type t = unit -> unit

  let id_portable = id

  let id_nonportable = id

  let bar = id
end
[%%expect{|
module Foo :
  sig
    type t
    val id_portable : t @ portable -> t @ portable
    val id_nonportable : t -> t
    val bar : t @ portable -> t
  end
|}]

module Foo : sig
  type t

  val illegal : t -> t @ portable
end = struct
  type t = unit -> unit

  let illegal = id
end
[%%expect{|
Lines 5-9, characters 6-3:
5 | ......struct
6 |   type t = unit -> unit
7 |
8 |   let illegal = id
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = unit -> unit
           val illegal : 'a @ [< 'm] -> 'a @ [> 'm]
         end
       is not included in
         sig type t val illegal : t -> t @ portable end
       Values do not match:
         val illegal : 'a @ [< 'm] -> 'a @ [> 'm]
       is not included in
         val illegal : t -> t @ portable
       The type
         "t @ [< 'm > stateful dynamic] -> t @ [> 'm | stateful dynamic]"
       is not compatible with the type "t -> t @ portable"
|}]

(* variant types *)

type 'a option' = None' | Some' of 'a

let wrap x = Some' x
[%%expect{|
type 'a option' = None' | Some' of 'a
val wrap : 'a @ [< 'm & global] -> 'a option' @ [> 'm] = <fun>
|}]

let unwrap_or default = function
  | None' -> default
  | Some' x -> x
[%%expect{|
val unwrap_or :
  'a @ [< 'n & global] -> 'a option' @ [< 'm] -> 'a @ [> 'm | 'n | dynamic] =
  <fun>
|}]

type ('a, 'b) either = Left of 'a | Right of 'b

let map_left f = function
  | Left x -> Left (f x)
  | Right y -> Right y
[%%expect{|
type ('a, 'b) either = Left of 'a | Right of 'b
val map_left :
  ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< global] ->
  ('a, 'c) either @ [< 'o & 'n & global] ->
  ('b, 'c) either @ [> 'o | 'm | dynamic] = <fun>
|}]

(* recursive functions *)

let rec length = function
  | [] -> 0
  | _ :: tl -> 1 + length tl
[%%expect{|
val length : 'a list @ [> dynamic] -> int @ [> dynamic] = <fun>
|}]

let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs
[%%expect{|
val map :
  ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< global many > aliased] ->
  'a list @ [< 'n > dynamic] -> 'b list @ [< global > 'm | dynamic] = <fun>
|}]

(* if/then/else *)

let choose b x y = if b then x else y
[%%expect{|
val choose :
  bool @ [< global] ->
  'a @ [< 'n & global] -> 'a @ [< 'm] -> 'a @ [> 'm | 'n | dynamic] = <fun>
|}]

(* nested closures *)

let nest x = fun () -> fun () -> fun () -> x
[%%expect{|
val nest :
  'a @ [< 'm & global] -> unit @ 'p -> unit @ 'o -> unit @ 'n -> 'a @ [> 'm] =
  <fun>
|}]

(* sequencing: using x then returning it *)

let use_and_return x = ignore x; x
[%%expect{|
val use_and_return :
  'a @ [< 'm & global many read_write] -> 'a @ [> 'm | aliased] = <fun>
|}]

(* multiple distinct mode variables *)

let swap (a, b) = (b, a)
[%%expect{|
val swap : 'a * 'b @ [< 'm & global] -> 'b * 'a @ [> 'm] = <fun>
|}]

let both_id x y = (x, y)
[%%expect{|
val both_id :
  'a @ [< 'n & global] -> 'b @ [< 'm & global] -> 'a * 'b @ [> 'm | 'n] =
  <fun>
|}]

(* let bindings preserving modes *)

let let_chain x =
  let a = x in
  let b = a in
  let c = b in
  c
[%%expect{|
val let_chain : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

(* mode polymorphism with option type *)

let map_option f = function
  | None -> None
  | Some x -> Some (f x)
[%%expect{|
val map_option :
  ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< global] ->
  'a option @ [< 'n] -> 'b option @ [> 'm | dynamic] = <fun>
|}]

(* Currying over three arguments *)

let triple x y z = (x, y, z)
[%%expect{|
val triple :
  'a @ [< 'o & global] ->
  'b @ [< 'n & global] ->
  'c @ [< 'm & global] -> 'a * 'b * 'c @ [> 'm | 'n | 'o] = <fun>
|}]

let flip f (x, y) = f (y, x)
[%%expect{|
val flip :
  ('a * 'b @ [> 'n] -> 'c @ [< 'm & global]) @ [< global] ->
  'b * 'a @ [< 'n & global] -> 'c @ [> 'm | dynamic] = <fun>
|}]

let flip f x y = f y x
[%%expect{|
val flip :
  ('a @ [> 'o] -> 'b @ [> 'n] -> 'c @ [< 'm & global]) @ [< global] ->
  'b @ [< 'n & global] -> 'a @ [< 'o] -> 'c @ [> 'm | dynamic] = <fun>
|}]


let flip f = fun x -> fun y -> f y x
[%%expect{|
val flip :
  ('a @ [> 'o] -> 'b @ [> 'n] -> 'c @ [< 'm & global]) @ [< global] ->
  'b @ [< 'n & global] -> 'a @ [< 'o] -> 'c @ [> 'm | dynamic] = <fun>
|}]
