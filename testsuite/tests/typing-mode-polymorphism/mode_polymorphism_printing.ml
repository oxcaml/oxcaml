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
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< past('o) & global] ->
  ('a @ [< 'n] -> 'b @ [> 'm | dynamic]) @ [> past('o)] = <fun>
|}]

let foo =
  let id x = x in
  fun x -> id x
[%%expect{|
val foo : 'a @ [< 'm & global] -> 'a @ [> 'm | dynamic] = <fun>
|}]

let foo a b = a + b
[%%expect{|
val foo : int @ 'n -> (int @ 'm -> int @ [> dynamic]) @ [> stateful] = <fun>
|}, Principal{|
val foo :
  int @ [< past('m) & global] ->
  (int @ 'n -> int @ [> dynamic]) @ [> past('m) | stateful] = <fun>
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
  ('a, 'b) mytypemod @ [< 'm & global] ->
  ('c @ 'n -> 'b @ [> 'm mod portable]) @ [> close('m)] = <fun>
|}]


let x =
  let foo x = x in
  let _ @ contended = foo (ref 42 : _ @ contended ) in
  let _ @ uncontended = foo  (ref 41 : _ @ uncontended) in
  foo
[%%expect{|
val x : '_weak1 -> '_weak1 @ [> aliased nonportable stateful dynamic] = <fun>
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
  'a @ [< 'm mod aliased dynamic & global many forkable unyielding] ->
  'a myref @ [> 'm | nonportable stateful] = <fun>
|}]

let read r = r.x
[%%expect{|
val read :
  'a myref @ [< 'm & shared read] ->
  'a @ [> 'm mod global many forkable unyielding | aliased dynamic] = <fun>
|}]

let store r = fun a -> r.x <- a
[%%expect{|
val store :
  'a myref @ [< past('m) & global corrupted write] ->
  ('a @ [< global many uncontended forkable unyielding read_write] ->
   unit @ 'n) @ [> past('m) | corruptible writing] =
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
  'a @ [< 'n & global many] ->
  'b @ [< 'm & global many] -> 'a * 'b @ [> 'm | 'n | aliased dynamic] =
  <fun>
|}]

(* currying *)

let foo x y = x
[%%expect{|
val foo : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]

let foo x y = y
[%%expect{|
val foo :
  'a @ [< past('m) & global] -> ('b @ [< 'n] -> 'b @ [> 'n]) @ [> past('m)] =
  <fun>
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
  ('a @ [< past('m) > 'q] ->
   ('b @ [> 'p] -> 'c @ [< 'o & global]) @ [> past('m) | past('n)]) @ [< past('mm1) & past('n) & past('mm0) & global] ->
  ('a @ [< 'q & global] ->
   ('b @ [< 'p] -> 'c @ [> 'o | dynamic]) @ [> close('q) | past('mm1)]) @ [> past('mm0)] =
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
  'a @ [< global many uncontended forkable unyielding read_write] ->
  'b @ 'm -> 'a ref @ [> aliased nonportable stateful dynamic] = <fun>
|}]

let foo (x @ aliased) y = ref x
[%%expect{|
val foo :
  'a @ [< global many uncontended forkable unyielding read_write > aliased] ->
  'b @ 'm -> 'a ref @ [> aliased nonportable stateful dynamic] = <fun>
|}]

let foo (x @ contended) y = x
[%%expect{|
val foo :
  'a @ [< 'm & global > contended] ->
  ('b @ 'n -> 'a @ [> 'm | contended]) @ [> close('m)] = <fun>
|}]

let foo x y z = 42
[%%expect{|
val foo :
  'a @ [< past('o) & past('m) & global] ->
  ('b @ [< past('n) & global] ->
   ('c @ 'q -> int @ 'p) @ [> past('n) | past('o)]) @ [> past('m)] =
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
  'a @ [< past('o) & past('m) & global] ->
  ('b @ [< 'n & global] ->
   ('c @ [< 'p & global] -> 'b * 'c @ [> 'p | 'n]) @ [> close('n) | past('o)]) @ [> past('m)] =
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
val foo :
  'a @ [< past('m) & global unique] ->
  ('b @ [< 'n & portable] -> 'b @ [> 'n]) @ [> past('m)] = <fun>
|}]

let foo (x @ local) (y @ unique) (z @ portable) = exclave_ (x, y, z)
[%%expect{|
val foo :
  'a @ [< 'm > local] ->
  ('b @ [< 'n & unique] ->
   ('c @ [< 'o & portable] -> 'a * 'b * 'c @ [> 'o | 'n | 'm | local]) @ [> close('n) | close('m) | local]) @ [> close('m) | local] =
  <fun>
|}]

(* if a type is annotated, mode crossing has an effect on the bounds of mode variable *)

type intref = { mutable v : int }

let foo (x : intref) (f : intref @ local -> int) = f x
[%%expect{|
type intref = { mutable v : int; }
val foo :
  intref @ [< global uncontended read_write] ->
  (intref @ local -> int) @ 'm -> int @ [> dynamic] = <fun>
|}]

let foo (f : int -> int) x y = f
[%%expect{|
val foo :
  (int -> int) @ [< 'p mod aliased contended immutable & past('o) & past('m) & global] ->
  ('a @ [< past('n) & global] ->
   ('b @ 'q -> (int -> int) @ [> 'p]) @ [> past('n) | past('o)]) @ [> past('m)] =
  <fun>
|}, Principal{|
val foo :
  (int -> int) @ [< 'm mod aliased contended immutable & global] ->
  ('a @ [< past('n) & global] ->
   ('b @ 'o -> (int -> int) @ [> 'm]) @ [> close('m) mod many portable stateless | past('n)]) @ [> close('m) mod many portable stateless] =
  <fun>
|}]

let foo (f : intref @ local -> int) (x : intref) (y : intref) = f x
[%%expect{|
val foo :
  (intref @ local -> int) @ [< past('o) & past('m) & global] ->
  (intref @ [< past('n) & global uncontended read_write] ->
   (intref @ 'p -> int @ [> dynamic]) @ [> past('n) mod many portable forkable unyielding stateless | past('o) | nonportable stateful]) @ [> past('m)] =
  <fun>
|}, Principal{|
val foo :
  (intref @ local -> int) @ [< past('o) & past('m) & global] ->
  (intref @ [< past('n) & global uncontended read_write] ->
   (intref @ 'p -> int @ [> dynamic]) @ [> past('n) | past('o) | nonportable stateful]) @ [> past('m)] =
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
  ('a @ [> past('m) | aliased nonportable stateful dynamic] ->
   'b @ [< global many uncontended forkable unyielding read_write]) @ [< past('o) & past('m) & past('n) & global many forkable unyielding] ->
  ('a list @ [< global many uncontended forkable unyielding read_write] ->
   'b list @ [> past('o) | aliased nonportable stateful dynamic]) @ [> past('n) | stateful] =
  <fun>
|}]

let map_eta f = fun l -> List.map f l
[%%expect{|
val map_eta :
  ('a @ [> past('m) | aliased nonportable stateful dynamic] ->
   'b @ [< global many uncontended forkable unyielding read_write]) @ [< past('o) & past('m) & past('n) & global many forkable unyielding] ->
  ('a list @ [< global many uncontended forkable unyielding read_write] ->
   'b list @ [> past('o) | aliased nonportable stateful dynamic]) @ [> past('n) | stateful] =
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
  Counter.t @ [< global many uncontended forkable unyielding read_write] ->
  Counter.t @ [> aliased nonportable stateful dynamic] = <fun>
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
         "t @ [< 'm > nonportable stateful dynamic] ->
         t @ [> 'm | nonportable stateful dynamic]"
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
  'a @ [< 'm & global] ->
  ('a option' @ [< 'n] -> 'a @ [> 'n | 'm | dynamic]) @ [> close('m)] = <fun>
|}]

type ('a, 'b) either = Left of 'a | Right of 'b

let map_left f = function
  | Left x -> Left (f x)
  | Right y -> Right y
[%%expect{|
type ('a, 'b) either = Left of 'a | Right of 'b
val map_left :
  ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< past('o) & global] ->
  (('a, 'c) either @ [< 'p & 'n & global] ->
   ('b, 'c) either @ [> 'p | 'm | dynamic]) @ [> past('o)] =
  <fun>
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
  ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< global many] ->
  'a list @ [< 'n > dynamic] -> 'b list @ [< global > 'm | dynamic] = <fun>
|}, Principal{|
val map :
  ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< global many > aliased] ->
  'a list @ [< 'n > dynamic] -> 'b list @ [< global > 'm | dynamic] = <fun>
|}]

(* if/then/else *)

let choose b x y = if b then x else y
[%%expect{|
val choose :
  bool @ 'p ->
  ('a @ [< 'n & global] ->
   ('a @ [< 'o] -> 'a @ [> 'o | 'n | dynamic]) @ [> close('n)]) @ 'm =
  <fun>
|}, Principal{|
val choose :
  bool @ [< past('o) & past('m) & global] ->
  ('a @ [< 'n & global] ->
   ('a @ [< 'p] -> 'a @ [> 'p | 'n | dynamic]) @ [> close('n) | past('o)]) @ [> past('m)] =
  <fun>
|}]

(* nested closures *)

let nest x = fun () -> fun () -> fun () -> x
[%%expect{|
val nest :
  'a @ [< 'm & global] ->
  (unit @ 'p ->
   (unit @ 'o -> (unit @ 'n -> 'a @ [> 'm]) @ [> close('m)]) @ [> close('m)]) @ [> close('m)] =
  <fun>
|}]

(* sequencing: using x then returning it *)

let use_and_return x = ignore x; x
[%%expect{|
val use_and_return :
  'a @ [< 'm & global many uncontended forkable unyielding read_write] ->
  'a @ [> 'm | aliased] = <fun>
|}]

(* multiple distinct mode variables *)

let swap (a, b) = (b, a)
[%%expect{|
val swap : 'a * 'b @ [< 'm & global] -> 'b * 'a @ [> 'm] = <fun>
|}]

let both_id x y = (x, y)
[%%expect{|
val both_id :
  'a @ [< 'm & global] ->
  ('b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm]) @ [> close('m)] = <fun>
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
  ('a @ [> 'n | dynamic] -> 'b @ [< 'm & global]) @ [< past('o) & global] ->
  ('a option @ [< 'n] -> 'b option @ [> 'm | dynamic]) @ [> past('o)] = <fun>
|}]

(* Currying over three arguments *)

let triple x y z = (x, y, z)
[%%expect{|
val triple :
  'a @ [< 'm & global] ->
  ('b @ [< 'n & global] ->
   ('c @ [< 'o & global] -> 'a * 'b * 'c @ [> 'o | 'n | 'm]) @ [> close('n) | close('m)]) @ [> close('m)] =
  <fun>
|}]

let flip f (x, y) = f (y, x)
[%%expect{|
val flip :
  ('a * 'b @ [> 'n] -> 'c @ [< 'm & global]) @ [< past('o) & global] ->
  ('b * 'a @ [< 'n & global] -> 'c @ [> 'm | dynamic]) @ [> past('o)] = <fun>
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


let flip f = fun x -> fun y -> f y x
[%%expect{|
val flip :
  ('a @ [< past('m) > 'q] ->
   ('b @ [> 'p] -> 'c @ [< 'o & global]) @ [> past('m) | past('n)]) @ [< past('mm1) & past('n) & past('mm0) & global] ->
  ('b @ [< 'p & global] ->
   ('a @ [< 'q] -> 'c @ [> 'o | dynamic]) @ [> close('p) | past('mm1)]) @ [> past('mm0)] =
  <fun>
|}]

(* CR dkalinichenko: the second definition should print [stateful nonportable]. *)

let stateful x : _ @ stateful = x
let stateful_nonportable x : _ @ stateful nonportable = x
[%%expect{|
val stateful : 'a @ [< 'm] -> 'a @ [> 'm | stateful] = <fun>
val stateful_nonportable : 'a @ [< 'm] -> 'a @ [> 'm | nonportable stateful] =
  <fun>
|}]

(* CR dkalinichenko: the printing loses information that the argument [f] and the returned
   [f] must have equal modes (in addition to having the same constraints). *)

let call_and_return f x = f x, f
[%%expect{|
val call_and_return :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< past('p) & past('o) & global many] ->
  ('a @ [< 'n] ->
   'b * ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [> 'm | past('p) | aliased dynamic]) @ [> past('o)] =
  <fun>
|}, Principal{|
val call_and_return :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o & global many] ->
  ('a @ [< 'n] ->
   'b * ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [> 'm | 'o | aliased dynamic]) @ [> close('o)] =
  <fun>
|}]

(* CR dkalinichenko: the second definition should retain the [global] upper
   bound on its intermediate closure. *)

let opaque_return x y = Sys.opaque_identity x
let global_opaque_return x : _ @ global = fun y -> Sys.opaque_identity x
[%%expect{|
val opaque_return :
  'a @ [< global many uncontended forkable unyielding read_write] ->
  'b @ 'm -> 'a @ [> aliased nonportable stateful dynamic] = <fun>
val global_opaque_return :
  'a @ [< global many uncontended forkable unyielding read_write] ->
  'b @ 'm -> 'a @ [> aliased nonportable stateful dynamic] = <fun>
|}]

let _ : (string -> (unit -> string) @ local) ref = ref opaque_return
[%%expect{|
- : (string -> (unit -> string) @ local) ref = {contents = <fun>}
|}]

let _ : (string -> (unit -> string) @ local) ref = ref global_opaque_return
[%%expect{|
Line 1, characters 51-75:
1 | let _ : (string -> (unit -> string) @ local) ref = ref global_opaque_return
                                                       ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "('a -> 'b -> 'a) ref"
       but an expression was expected of type
         "(string -> (unit -> string) @ local) ref"
       Type "'a -> 'b -> 'a" is not compatible with type
         "string -> (unit -> string) @ local"
Hint: This function application is partial, maybe some arguments are missing.
|}]

(* CR dkalinichenko: with [-principal], [past('p)] on the final closure
   should also appear as an upper bound on [c]. *)

type 'a cell = { mutable v : 'a }

let store_and_read c x () =
  c.v <- x;
  let _ = c.v in
  ()
[%%expect{|
type 'a cell = { mutable v : 'a; }
val store_and_read :
  'a cell @ [< past('n) & global uncontended read_write] ->
  ('a @ [< past('m) & global many uncontended forkable unyielding read_write] ->
   (unit @ 'mm0 -> unit @ 'q) @ [> past('o) | past('p) mod many forkable unyielding | nonportable stateful]) @ [> past('m) | past('n) mod many forkable unyielding | nonportable stateful] =
  <fun>
|}, Principal{|
type 'a cell = { mutable v : 'a; }
val store_and_read :
  'a cell @ [< past('n) & global uncontended read_write] ->
  ('a @ [< past('m) & global many uncontended forkable unyielding read_write] ->
   (unit @ 'mm0 -> unit @ 'q) @ [> past('o) | past('p) | nonportable stateful]) @ [> past('m) | past('n) | nonportable stateful] =
  <fun>
|}]

let _ :
    (string cell @ once ->
     (string -> (unit -> unit) @ once) @ once) ref =
  ref store_and_read
[%%expect{|
- : (string cell @ once -> string -> unit -> unit) ref = {contents = <fun>}
|}]

(* With [-principal], the underlying constraint prevents a [many] final closure
   when [c] is [once]. The surrounding [ref] prevents subsumption. *)
let _ :
    (string cell @ once ->
     (string -> (unit -> unit) @ many) @ once) ref =
  ref store_and_read
[%%expect{|
- : (string cell @ once -> string -> (unit -> unit)) ref = {contents = <fun>}
|}, Principal{|
Line 4, characters 2-20:
4 |   ref store_and_read
      ^^^^^^^^^^^^^^^^^^
Error: This expression has type
         "(string cell @ once -> string -> unit -> unit) ref"
       but an expression was expected of type
         "(string cell @ once -> string -> (unit -> unit)) ref"
       Type "string -> (unit -> unit) @ once" is not compatible with type
         "string -> unit -> unit"
Hint: This function application is partial, maybe some arguments are missing.
|}]
