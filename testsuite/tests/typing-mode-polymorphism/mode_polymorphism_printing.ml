(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha";
 expect;
*)

(*
 * This file tests printing of poymorphic mode variables
*)


let id x = x
[%%expect{|
val id : 'a @ [< 'm & global] -> 'a @ [< global > 'm] = <fun>
|}]

let foo x = 42
[%%expect{|
val foo : 'a @ 'm -> int @ [< global] = <fun>
|}]

(* CR ageorges: Is there a way to explain the following? id is instantiated, but foo is
  generalized with more bounds that necessary? *)
let foo x = id x
[%%expect{|
val foo : 'a @ [< 'm & global] -> 'a @ [< global > 'm] = <fun>
|}]

let foo f x = f x
[%%expect{|
val foo :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o.future & global] ->
  ('a @ [< 'n] -> 'b @ [< global > 'm]) @ [< global > 'o.future] = <fun>
|}, Principal{|
val foo :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o.future & global] ->
  ('a @ [< 'n] -> 'b @ [< global > 'm]) @ [< global > 'o.future] = <fun>
|}]

let foo =
  let id x = x in
  fun x -> id x
[%%expect{|
val foo : 'a @ [< 'm & global] -> 'a @ [< global > 'm] = <fun>
|}]

(* CR ageorges: make the printer aware of mode crossing/jkinds *)
let foo a b = a + b
[%%expect{|
val foo : int @ 'n -> (int @ 'm -> int @ [< global]) @ [< global] = <fun>
|}, Principal{|
val foo :
  int @ [< 'm.future & global] ->
  (int @ 'n -> int @ [< global]) @ [< global > 'm.future] = <fun>
|}]


(* records *)

type ('a,'b) mytypemod = { x : 'a; y : 'b @@ portable }

let foo t = t.x
[%%expect{|
type ('a, 'b) mytypemod = { x : 'a; y : 'b @@ portable; }
val foo : ('a, 'b) mytypemod @ [< 'm & global] -> 'a @ [< global > 'm] =
  <fun>
|}]

let foo t = t.y
[%%expect{|
val foo :
  ('a, 'b) mytypemod @ [< 'm & global] -> 'b @ [< global > 'm mod portable] =
  <fun>
|}]

let x =
  let foo x = x in
  let _ @ contended = foo (ref 42 : _ @ contended ) in
  let _ @ uncontended = foo  (ref 41 : _ @ uncontended) in
  foo
[%%expect{|
val x : '_weak1 -> '_weak1 @ [< global > aliased nonportable] = <fun>
|}]

type ('a,'b) mytype = { x : 'a; y : 'b }
[%%expect{|
type ('a, 'b) mytype = { x : 'a; y : 'b; }
|}]

let foo x y = { x; y }
[%%expect{|
val foo :
  'a @ [< 'p & 'n.future & global] ->
  ('b @ [< 'o & global] -> ('a, 'b) mytype @ [< global > 'm | 'o | 'p]) @ [< global > close('m) | 'n.future] =
  <fun>
|}]

let foo x = fun y -> { x; y }
[%%expect{|
val foo :
  'a @ [< 'p & 'n.future & global] ->
  ('b @ [< 'o & global] -> ('a, 'b) mytype @ [< global > 'm | 'o | 'p]) @ [< global > close('m) | 'n.future] =
  <fun>
|}]

let foo x = { x; y = 42 }
[%%expect{|
val foo : 'a @ [< 'm & global] -> ('a, int) mytype @ [< global > 'm] = <fun>
|}]

let foo r = { r with y = 42 }
[%%expect{|
val foo :
  ('a, 'b) mytype @ [< global many uncontended] ->
  ('a, int) mytype @ [< global > aliased nonportable] = <fun>
|}]

type 'a myref = { mutable x : 'a }
[%%expect{|
type 'a myref = { mutable x : 'a; }
|}]

let create a = { x = a }
[%%expect{|
val create :
  'a @ [< global many > 'm.future] ->
  'a myref @ [< 'm.future & global > nonportable] = <fun>
|}]

let read r = r.x
[%%expect{|
val read :
  'a myref @ [< 'm & shared] ->
  'a @ [< global > 'm mod global many | aliased] = <fun>
|}]

let store r = fun a -> r.x <- a
[%%expect{|
val store :
  'a myref @ [< 'n.future & global uncontended] ->
  ('a @ [< global many uncontended > 'm.future] -> unit @ [< global]) @ [< 'm.future & global > 'n.future | nonportable] =
  <fun>
|}]

(* products *)

let dupl x = (x, x)
[%%expect{|
val dupl : 'a @ [< 'm & global many] -> 'a * 'a @ [< global > 'm | aliased] =
  <fun>
|}]

let prod x y = (x, y)
[%%expect{|
val prod :
  'a @ [< 'p & 'n.future & global] ->
  ('b @ [< 'o & global] -> 'a * 'b @ [< global > 'm | 'o | 'p]) @ [< global > close('m) | 'n.future] =
  <fun>
|}]

let prod_eta x = fun y -> (x, y)
[%%expect{|
val prod_eta :
  'a @ [< 'p & 'n.future & global] ->
  ('b @ [< 'o & global] -> 'a * 'b @ [< global > 'm | 'o | 'p]) @ [< global > close('m) | 'n.future] =
  <fun>
|}]

let fst (a, _) = a
let snd (_, b) = b
[%%expect{|
val fst : 'a * 'b @ [< 'm & global] -> 'a @ [< global > 'm] = <fun>
val snd : 'a * 'b @ [< 'm & global] -> 'b @ [< global > 'm] = <fun>
|}]

let foo x = fun y ->
  let x' = fst (x,y) in
  let y' = snd (x,y) in
  (x', y')
[%%expect{|
val foo :
  'a @ [< 'p & 'n.future & global many] ->
  ('b @ [< 'o & global many] -> 'a * 'b @ [< global > 'm | 'o | 'p | aliased]) @ [< global > close('m) | 'n.future | nonportable] =
  <fun>
|}]

(* currying *)

let foo x y = x
[%%expect{|
val foo :
  'a @ [< 'o & 'n.future & global] ->
  ('b @ 'p -> 'a @ [< global > 'm | 'o]) @ [< global > close('m) | 'n.future] =
  <fun>
|}]

let foo x y = y
[%%expect{|
val foo :
  'a @ [< 'm.future & global] ->
  ('b @ [< 'n & global] -> 'b @ [< global > 'n]) @ [< global > 'm.future] =
  <fun>
|}]

let foo f = fun x -> fun y -> f x y
[%%expect{|
val foo :
  ('a @ [< 'm.future > 'mm0 | 'n | 'mm1] ->
   ('b @ [> 'q] -> 'c @ [< 'p & global]) @ [> 'm.future | monadic_to_comonadic_min('n) | 'o.future]) @ [< 'mm4.future & 'o.future & 'mm2.future & global] ->
  ('a @ [< 'mm3.future & 'mm1 & global] ->
   ('b @ [< 'q] -> 'c @ [< global > 'p]) @ [< global > 'mm3.future | close('mm0) | 'mm4.future]) @ [< global > 'mm2.future] =
  <fun>
|}]

let fst x = fun y -> x
[%%expect{|
val fst :
  'a @ [< 'o & 'n.future & global] ->
  ('b @ 'p -> 'a @ [< global > 'm | 'o]) @ [< global > close('m) | 'n.future] =
  <fun>
|}]
let snd x = fun y -> y
[%%expect{|
val snd :
  'a @ 'n -> ('b @ [< 'm & global] -> 'b @ [< global > 'm]) @ [< global] =
  <fun>
|}]

let foo x y = ref x
[%%expect{|
val foo :
  'a @ [< global many uncontended > 'm.future] ->
  ('b @ 'n -> 'a ref @ [< global > aliased nonportable]) @ [< 'm.future & global > nonportable] =
  <fun>
|}]

let foo (x @ aliased) y = ref x
[%%expect{|
val foo :
  'a @ [< global many uncontended > 'm.future | aliased] ->
  ('b @ 'n -> 'a ref @ [< global > aliased nonportable]) @ [< 'm.future & global > nonportable] =
  <fun>
|}]

let foo (x @ contended) y = x
[%%expect{|
val foo :
  'a @ [< 'o & 'n.future & global > contended] ->
  ('b @ 'p -> 'a @ [< global > 'm | 'o | contended]) @ [< global > close('m) | 'n.future] =
  <fun>
|}]

let foo x y z = 42
[%%expect{|
val foo :
  'a @ [< 'm.future & global] ->
  ('b @ [< 'n.future & global] ->
   ('c @ 'o -> int @ [< global]) @ [< global > 'n.future]) @ [< global > 'm.future] =
  <fun>
|}]

let foo x y = (x, y)
[%%expect{|
val foo :
  'a @ [< 'p & 'n.future & global] ->
  ('b @ [< 'o & global] -> 'a * 'b @ [< global > 'm | 'o | 'p]) @ [< global > close('m) | 'n.future] =
  <fun>
|}]

let foo x y z = (y,z)
[%%expect{|
val foo :
  'a @ [< 'm.future & global] ->
  ('b @ [< 'q & 'o.future & global] ->
   ('c @ [< 'p & global] -> 'b * 'c @ [< global > 'n | 'p | 'q]) @ [< global > close('n) | 'o.future]) @ [< global > 'm.future] =
  <fun>
|}]

(* annotations *)

(* CR ageorges: if a mode variable is fully determined (its bounds are equal) consider
  printing it as a constant rather than variable *)
let legacy_id (x @ global many aliased nonportable uncontended forkable unyielding stateful read_write dynamic) = x
[%%expect{|
val legacy_id :
  'a @ [< global many uncontended > aliased nonportable] ->
  'a @ [< global > aliased nonportable] = <fun>
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
val foo : 'a @ [< 'm & global portable] -> 'a @ [< global > 'm] = <fun>
|}]

let foo : (unit -> unit) @ portable = fun () -> ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

let foo (unique_ y) (z @ portable) = z
[%%expect{|
val foo :
  'a @ [< 'm.future & global unique] ->
  ('b @ [< 'n & global portable] -> 'b @ [< global > 'n]) @ [< global > 'm.future] =
  <fun>
|}]

let foo (local_ x) (unique_ y) (z @ portable) = exclave_ (x, y, z)
[%%expect{|
val foo :
  'a @ [< 'mm1 & 'n.future > local] ->
  ('b @ [< 'mm0 & 'p.future & unique] ->
   ('c @ [< 'q & portable] ->
    'a * 'b * 'c @ [> 'o | 'm | 'q | 'mm0 | 'mm1 | local]) @ [> close('o) | 'p.future | local]) @ [> close('m) | 'n.future | local] =
  <fun>
|}]

(* if a type is annotated, mode crossing has an effect on the bounds of mode variable *)

type intref = { mutable v : int }

let foo (x : intref) (f : intref @ local -> int) = f x
[%%expect{|
type intref = { mutable v : int; }
val foo :
  intref @ [< 'm.future & global uncontended] ->
  ((intref @ local -> int) @ 'n -> int @ [< global]) @ [< global > 'm.future | nonportable] =
  <fun>
|}]

(* CR ageorges: ideally we want to apply mode crossing reguardless of principality *)
let foo (f : int -> int) x y = f
[%%expect{|
val foo :
  (int -> int) @ [< 'o mod aliased contended & 'm.future & global] ->
  ('a @ [< 'n.future & global] ->
   ('b @ 'p -> (int -> int) @ [< global > 'o]) @ [< global > 'n.future]) @ [< global > 'm.future] =
  <fun>
|}, Principal{|
val foo :
  (int -> int) @ [< 'p mod aliased contended & 'n.future & global] ->
  ('a @ [< 'o.future & global] ->
   ('b @ 'q -> (int -> int) @ [< global > 'm | 'p]) @ [< global > 'o.future]) @ [< global > close('m) mod many portable | 'n.future] =
  <fun>
|}]

let foo (f : intref @ local -> int) (x : intref) (y : intref) = f x
[%%expect{|
val foo :
  (intref @ local -> int) @ [< 'm.future & global] ->
  (intref @ [< 'n.future & global uncontended] ->
   (intref @ 'o -> int @ [< global]) @ [< global > 'n.future mod many portable | nonportable]) @ [< global > 'm.future] =
  <fun>
|}, Principal{|
val foo :
  (intref @ local -> int) @ [< 'm.future & global] ->
  (intref @ [< 'n.future & global uncontended] ->
   (intref @ 'o -> int @ [< global]) @ [< global > 'n.future | nonportable]) @ [< global > 'm.future] =
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
  ('a @ [< 'm.future > aliased nonportable] ->
   'b @ [< global many uncontended]) @ [< 'n.future & global many > 'o.future | 'm.future] ->
  ('a list @ [< global many uncontended] ->
   'b list @ [< 'o.future & global > aliased nonportable]) @ [< global > 'n.future] =
  <fun>
|}, Principal{|
val map :
  ('a @ [< 'm.future > aliased nonportable] ->
   'b @ [< global many uncontended]) @ [< 'n.future & global many > 'o.future | 'm.future] ->
  ('a list @ [< global many uncontended] ->
   'b list @ [< 'o.future & global > aliased nonportable]) @ [< global > 'n.future] =
  <fun>
|}]

let map_eta f = fun l -> List.map f l
[%%expect{|
val map_eta :
  ('a @ [< 'm.future > aliased nonportable] ->
   'b @ [< global many uncontended]) @ [< 'n.future & global many > 'o.future | 'm.future] ->
  ('a list @ [< global many uncontended] ->
   'b list @ [< 'o.future & global > aliased nonportable]) @ [< global > 'n.future] =
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
  Counter.t @ [< global > aliased nonportable] = <fun>
|}]

let incr = Counter.incr
[%%expect{|
val incr : Counter.t -> Counter.t = <fun>
|}]

let incr n = n + 1
[%%expect{|
val incr : int @ 'm -> int @ [< global] = <fun>
|}]

let id x = x
[%%expect{|
val id : 'a @ [< 'm & global] -> 'a @ [< global > 'm] = <fun>
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
           val illegal : 'a @ [< 'm & global] -> 'a @ [< global > 'm]
         end
       is not included in
         sig type t val illegal : t -> t @ portable end
       Values do not match:
         val illegal : 'a @ [< 'm & global] -> 'a @ [< global > 'm]
       is not included in
         val illegal : t -> t @ portable
       The type
         "t @ [< 'm & 'm & 'm & 'm & global > nonportable] ->
         t @ [< global > 'm | 'm | 'm | 'm | nonportable]"
       is not compatible with the type "t -> t @ portable"
|}]
