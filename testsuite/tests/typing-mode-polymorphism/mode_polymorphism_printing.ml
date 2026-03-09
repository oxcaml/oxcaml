(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha";
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

(* CR ageorges: Is there a way to explain the following? id is instantiated, but foo is
  generalized with more bounds that necessary? *)
let foo x = id x
[%%expect{|
val foo : 'a @ [< 'm & global] -> 'a @ [> 'm] = <fun>
|}]

let foo f x = f x
[%%expect{|
val foo :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o @@ past] ->
  ('a @ [< 'n] -> 'b @ [> 'm]) @ [> 'o] = <fun>
|}, Principal{|
val foo :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o @@ past] ->
  ('a @ [< 'n] -> 'b @ [> 'm]) @ [> 'o] = <fun>
|}]

let foo =
  let id x = x in
  fun x -> id x
[%%expect{|
val foo : 'a @ [< 'm & global] -> 'a @ [> 'm] = <fun>
|}]

(* CR ageorges: make the printer aware of mode crossing/jkinds *)
let foo a b = a + b
[%%expect{|
val foo : int @ 'p -> (int @ 'o -> int @ 'n) @ 'm = <fun>
|}, Principal{|
val foo : int @ [< 'm @@ past] -> (int @ 'o -> int @ 'n) @ [> 'm] = <fun>
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
val foo : ('a, 'b) mytypemod @ [< 'm] -> 'b @ [> 'm @@ portable] = <fun>
|}]

let foo x z = x.y
[%%expect{|
val foo :
  ('a, 'b) mytypemod @ [< 'm] ->
  ('c @ 'n -> 'b @ [> 'm @@ portable]) @ [> close('m)] = <fun>
|}]


let x =
  let foo x = x in
  let _ @ contended = foo (ref 42 : _ @ contended ) in
  let _ @ uncontended = foo  (ref 41 : _ @ uncontended) in
  foo
[%%expect{|
val x : '_weak1 @ unique stateless -> '_weak1 @ 'm = <fun>
|}]

type ('a,'b) mytype = { x : 'a; y : 'b }
[%%expect{|
type ('a, 'b) mytype = { x : 'a; y : 'b; }
|}]

let foo x y = { x; y }
[%%expect{|
val foo :
  'a @ [< 'm & global] ->
  ('b @ [< 'n & global] -> ('a, 'b) mytype @ [> 'n | 'm]) @ [> close('m)] =
  <fun>
|}]

let foo x = fun y -> { x; y }
[%%expect{|
val foo :
  'a @ [< 'm & global] ->
  ('b @ [< 'n & global] -> ('a, 'b) mytype @ [> 'n | 'm]) @ [> close('m)] =
  <fun>
|}]

let foo x = { x; y = 42 }
[%%expect{|
val foo : 'a @ [< 'm & global] -> ('a, int) mytype @ [> 'm] = <fun>
|}]

let foo r = { r with y = 42 }
[%%expect{|
val foo : ('a, 'b) mytype @ [< global] -> ('a, int) mytype @ 'm = <fun>
|}]

type 'a myref = { mutable x : 'a }
[%%expect{|
type 'a myref = { mutable x : 'a; }
|}]

let create a = { x = a }
[%%expect{|
val create :
  'a @ [< global many > 'm] -> 'a myref @ [< 'm @@ past > nonportable] =
  <fun>
|}]

let read r = r.x
[%%expect{|
val read : 'a myref @ [< 'm & shared] -> 'a @ [> 'm @@ global many | aliased] =
  <fun>
|}]

let store r = fun a -> r.x <- a
[%%expect{|
val store :
  'a myref @ [< 'n @@ past & global uncontended] ->
  ('a @ [< global many uncontended > 'm] -> unit @ 'o) @ [< 'm @@ past > 'n | nonportable] =
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
  'a @ [< 'm & global] ->
  ('b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm]) @ [> close('m)] = <fun>
|}]

let prod_eta x = fun y -> (x, y)
[%%expect{|
val prod_eta :
  'a @ [< 'm & global] ->
  ('b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm]) @ [> close('m)] = <fun>
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
  'a @ [< 'm & global many] ->
  ('b @ [< 'n & global many] -> 'a * 'b @ [> 'n | 'm | aliased]) @ [> close('m) | nonportable] =
  <fun>
|}]

(* currying *)

let foo x y = x
[%%expect{|
val foo : 'a @ [< 'm] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] = <fun>
|}]

let foo x y = y
[%%expect{|
val foo : 'a @ [< 'm @@ past] -> ('b @ [< 'n] -> 'b @ [> 'n]) @ [> 'm] =
  <fun>
|}]

let foo x y = id x
[%%expect{|
val foo :
  'a @ [< 'm & global] ->
  ('b @ 'n -> 'a @ [> 'm]) @ [> close('m) | nonportable] = <fun>
|}]

let foo f = fun x -> fun y -> f x y
[%%expect{|
val foo :
  ('a @ [< 'm @@ past > 'q] ->
   ('b @ [> 'p] -> 'c @ [< 'o & global]) @ [> 'm | 'n]) @ [< 'mm1 @@ past & 'n @@ past & 'mm0 @@ past & global] ->
  ('a @ [< 'q & global] ->
   ('b @ [< 'p] -> 'c @ [> 'o]) @ [> close('q) | 'mm1]) @ [> 'mm0] =
  <fun>
|}]

let fst x = fun y -> x
[%%expect{|
val fst : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]
let snd x = fun y -> y
[%%expect{|
val snd : 'a @ 'o -> ('b @ [< 'n] -> 'b @ [> 'n]) @ 'm = <fun>
|}]

let foo x y = ref x
[%%expect{|
val foo :
  'a @ [< global many uncontended > 'm] ->
  ('b @ 'n -> 'a ref @ [> aliased nonportable]) @ [< 'm @@ past > nonportable] =
  <fun>
|}]

let foo (x @ aliased) y = ref x
[%%expect{|
val foo :
  'a @ [< global many uncontended > 'm | aliased] ->
  ('b @ 'n -> 'a ref @ [> aliased nonportable]) @ [< 'm @@ past > nonportable] =
  <fun>
|}]

let foo (x @ contended) y = x
[%%expect{|
val foo :
  'a @ [< 'm > contended] ->
  ('b @ 'n -> 'a @ [> 'm | contended]) @ [> close('m)] = <fun>
|}]

let foo x y z = 42
[%%expect{|
val foo :
  'a @ [< 'o @@ past & 'm @@ past] ->
  ('b @ [< 'n @@ past] -> ('c @ 'q -> int @ 'p) @ [> 'n | 'o]) @ [> 'm] =
  <fun>
|}]

let foo x y = (x, y)
[%%expect{|
val foo :
  'a @ [< 'm & global] ->
  ('b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm]) @ [> close('m)] = <fun>
|}]

let foo x y z = (y,z)
[%%expect{|
val foo :
  'a @ [< 'o @@ past & 'm @@ past] ->
  ('b @ [< 'n & global] ->
   ('c @ [< 'p & global] -> 'b * 'c @ [> 'p | 'n]) @ [> close('n) | 'o]) @ [> 'm] =
  <fun>
|}]

(* annotations *)

(* CR ageorges: if a mode variable is fully determined (its bounds are equal) consider
  printing it as a constant rather than variable *)
let legacy_id (x @ global many aliased nonportable uncontended forkable unyielding stateful read_write dynamic) = x
[%%expect{|
val legacy_id :
  'a @ [< global many uncontended > aliased nonportable] ->
  'a @ [> aliased nonportable] = <fun>
|}]

let foo (local_ x) = x
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

let foo (unique_ y) (z @ portable) = z
[%%expect{|
val foo :
  'a @ [< 'm @@ past & unique] ->
  ('b @ [< 'n & portable] -> 'b @ [> 'n]) @ [> 'm] = <fun>
|}]

let foo (local_ x) (unique_ y) (z @ portable) = exclave_ (x, y, z)
[%%expect{|
val foo :
  'a @ [< 'm > local] ->
  ('b @ [< 'n & unique] ->
   ('c @ [< 'o & portable] -> 'a * 'b * 'c @ [> 'o | 'n | 'm | local]) @ [> close('m) | close('n) | local]) @ [> close('m) | local] =
  <fun>
|}]

(* if a type is annotated, mode crossing has an effect on the bounds of mode variable *)

type intref = { mutable v : int }

let foo (x : intref) (f : intref @ local -> int) = f x
[%%expect{|
type intref = { mutable v : int; }
val foo :
  intref @ [< 'm @@ past & uncontended] ->
  ((intref @ local -> int) @ 'o -> int @ 'n) @ [> 'm | nonportable] = <fun>
|}]

(* CR ageorges: ideally we want to apply mode crossing reguardless of principality *)
let foo (f : int -> int) x y = f
[%%expect{|
val foo :
  (int -> int) @ [< 'p . aliased contended & 'o @@ past & 'm @@ past] ->
  ('a @ [< 'n @@ past] -> ('b @ 'q -> (int -> int) @ [> 'p]) @ [> 'n | 'o]) @ [> 'm] =
  <fun>
|}, Principal{|
val foo :
  (int -> int) @ [< 'm . aliased contended] ->
  ('a @ [< 'n @@ past] ->
   ('b @ 'o -> (int -> int) @ [> 'm]) @ [> close('m) @@ many portable | 'n]) @ [> close('m) @@ many portable] =
  <fun>
|}]

let foo (f : intref @ local -> int) (x : intref) (y : intref) = f x
[%%expect{|
val foo :
  (intref @ local -> int) @ [< 'o @@ past & 'm @@ past] ->
  (intref @ [< 'n @@ past & uncontended] ->
   (intref @ 'q -> int @ 'p) @ [> 'n @@ many portable | 'o | nonportable]) @ [> 'm] =
  <fun>
|}, Principal{|
val foo :
  (intref @ local -> int) @ [< 'o @@ past & 'm @@ past] ->
  (intref @ [< 'n @@ past & uncontended] ->
   (intref @ 'q -> int @ 'p) @ [> 'n | 'o | nonportable]) @ [> 'm] =
  <fun>
|}]

(* aliases of non-polymorphic functions *)

let map = List.map
[%%expect{|
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
|}]

let map f l = List.map f l
[%%expect{|
val map :
  ('a @ [< 'm @@ past > aliased nonportable] ->
   'b @ [< global many uncontended]) @ [< 'n @@ past & global many > 'o | 'm] ->
  ('a list @ [< global many uncontended] ->
   'b list @ [< 'o @@ past > aliased nonportable]) @ [> 'n] =
  <fun>
|}, Principal{|
val map :
  ('a @ [< 'm @@ past > aliased nonportable] ->
   'b @ [< global many uncontended]) @ [< 'n @@ past & global many > 'o | 'm] ->
  ('a list @ [< global many uncontended] ->
   'b list @ [< 'o @@ past > aliased nonportable]) @ [> 'n] =
  <fun>
|}]

let map_eta f = fun l -> List.map f l
[%%expect{|
val map_eta :
  ('a @ [< 'm @@ past > aliased nonportable] ->
   'b @ [< global many uncontended]) @ [< 'n @@ past & global many > 'o | 'm] ->
  ('a list @ [< global many uncontended] ->
   'b list @ [< 'o @@ past > aliased nonportable]) @ [> 'n] =
  <fun>
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
  Counter.t @ [< global many uncontended] ->
  Counter.t @ [> aliased nonportable] = <fun>
|}]

let incr = Counter.incr
[%%expect{|
val incr : Counter.t -> Counter.t = <fun>
|}]

let incr n = n + 1
[%%expect{|
val incr : int @ 'n -> int @ 'm = <fun>
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

(* CR ageorges: remove duplicates in [< 'm & 'm] and [> 'm | 'm] *)
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
         "t @ [< 'm & 'm & 'm & 'm > nonportable] ->
         t @ [> 'm | 'm | 'm | 'm | nonportable]"
       is not compatible with the type "t -> t @ portable"
|}]
