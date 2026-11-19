(* TEST
 flags = "-extension unique -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

let id x = x
[%%expect{|
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

let foo x = 42
[%%expect{|
val foo : 'a @ 'm -> int @ 'n = <fun>
|}]

let foo x = id x
[%%expect{|
val foo : 'a @ [< 'm & global] -> 'a @ [> 'm | dynamic] = <fun>
|}]

let foo f x = f x
[%%expect{|
val foo :
  ('a @ [> 'm] -> 'b @ [< 'n & global]) @ [< global] ->
  'a @ [< 'm] -> 'b @ [> 'n | dynamic] = <fun>
|}]

let foo =
  let id x = x in
  fun x -> id x
[%%expect{|
val foo : 'a @ [< 'm & global] -> 'a @ [> 'm | dynamic] = <fun>
|}]

let foo a b = a + b
[%%expect{|
val foo : int @ 'm -> int @ 'n -> int @ [> dynamic] = <fun>
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
  'b @ [< 'n & global] -> ('a, 'b) mytype @ [> 'n | 'm] = <fun>
|}]

let foo x = fun y -> { x; y }
[%%expect{|
val foo :
  'a @ [< 'm & global] ->
  'b @ [< 'n & global] -> ('a, 'b) mytype @ [> 'n | 'm] = <fun>
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
  'a myref @ [< global corrupted write] ->
  'a @ [< global many uncontended forkable unyielding read_write] ->
  unit @ 'm = <fun>
|}]

(* products *)

let dupl x = (x, x)
[%%expect{|
val dupl : 'a @ [< 'm & global many] -> 'a * 'a @ [> 'm | aliased] = <fun>
|}]

let prod x y = (x, y)
[%%expect{|
val prod :
  'a @ [< 'm & global] -> 'b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm] =
  <fun>
|}]

let prod_eta x = fun y -> (x, y)
[%%expect{|
val prod_eta :
  'a @ [< 'm & global] -> 'b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm] =
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
  'a @ [< 'm & global many] ->
  'b @ [< 'n & global many] -> 'a * 'b @ [> 'n | 'm | aliased dynamic] =
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
  ('a @ [> 'm] -> 'b @ [> 'n] -> 'c @ [< 'o & global]) @ [< global] ->
  'a @ [< 'm & global] -> 'b @ [< 'n] -> 'c @ [> 'o | dynamic] = <fun>
|}]

let fst x = fun y -> x
[%%expect{|
val fst : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] = <fun>
|}]
let snd x = fun y -> y
[%%expect{|
val snd : 'a @ 'm -> 'b @ [< 'n] -> 'b @ [> 'n] = <fun>
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
  'a @ [< 'm & global > contended] -> 'b @ 'n -> 'a @ [> 'm | contended] =
  <fun>
|}]

let foo x y z = 42
[%%expect{|
val foo : 'a @ [< global] -> 'b @ [< global] -> 'c @ 'm -> int @ 'n = <fun>
|}]

let foo x y = (x, y)
[%%expect{|
val foo :
  'a @ [< 'm & global] -> 'b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm] =
  <fun>
|}]

let foo x y z = (y,z)
[%%expect{|
val foo :
  'a @ [< global] ->
  'b @ [< 'm & global] -> 'c @ [< 'n & global] -> 'b * 'c @ [> 'n | 'm] =
  <fun>
|}]

(* annotations *)

let legacy_id : 'a -> 'a = fun x -> x
[%%expect{|
val legacy_id : 'a -> 'a = <fun>
|}]

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
  'a @ [< 'm > local] ->
  'b @ [< 'n & unique] ->
  'c @ [< 'o & portable] -> 'a * 'b * 'c @ [> 'o | 'n | 'm | local] = <fun>
|}]

(* middle mode annotations *)

let foo (x @ read) = ()
[%%expect{|
val foo : 'a @ [< read > read] -> unit @ 'm = <fun>
|}]

let foo x : _ @ read = x
[%%expect{|
val foo : 'a @ [< 'm & read] -> 'a @ [< read > 'm | read] = <fun>
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
  (int -> int) @ [< 'm mod aliased contended immutable & global] ->
  'a @ [< global] -> 'b @ 'n -> (int -> int) @ [> 'm] = <fun>
|}]

let foo (f : intref @ local -> int) (x : intref) (y : intref) = f x
[%%expect{|
val foo :
  (intref @ local -> int) @ [< global] ->
  intref @ [< global uncontended read_write] ->
  intref @ 'm -> int @ [> dynamic] = <fun>
|}]

(* aliases of non-polymorphic functions *)

let map = List.map
[%%expect{|
val map : ('a -> 'b) -> 'a list -> 'b list = <fun>
|}]

let map f l = List.map f l
[%%expect{|
val map :
  ('a @ [> aliased nonportable stateful dynamic] ->
   'b @ [< global many uncontended forkable unyielding read_write]) @ [< global many forkable unyielding] ->
  'a list @ [< global many uncontended forkable unyielding read_write] ->
  'b list @ [> aliased nonportable stateful dynamic] = <fun>
|}]

let map_eta f = fun l -> List.map f l
[%%expect{|
val map_eta :
  ('a @ [> aliased nonportable stateful dynamic] ->
   'b @ [< global many uncontended forkable unyielding read_write]) @ [< global many forkable unyielding] ->
  'a list @ [< global many uncontended forkable unyielding read_write] ->
  'b list @ [> aliased nonportable stateful dynamic] = <fun>
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
       The return mode was expected to be "portable" but is "nonportable"
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
  'a @ [< 'm & global] -> 'a option' @ [< 'n] -> 'a @ [> 'n | 'm | dynamic] =
  <fun>
|}]

type ('a, 'b) either = Left of 'a | Right of 'b

let map_left f = function
  | Left x -> Left (f x)
  | Right y -> Right y
[%%expect{|
type ('a, 'b) either = Left of 'a | Right of 'b
val map_left :
  ('a @ [> 'm | dynamic] -> 'b @ [< 'n & global]) @ [< global] ->
  ('a, 'c) either @ [< 'o & 'm & global] ->
  ('b, 'c) either @ [> 'o | 'n | dynamic] = <fun>
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
  ('a @ [> 'm | dynamic] -> 'b @ [< 'n & global]) @ [< global many] ->
  'a list @ [< 'm > dynamic] -> 'b list @ [< global > 'n | dynamic] = <fun>
|}, Principal{|
val map :
  ('a @ [> 'm | dynamic] -> 'b @ [< 'n & global]) @ [< global many > aliased] ->
  'a list @ [< 'm > dynamic] -> 'b list @ [< global > 'n | dynamic] = <fun>
|}]

(* if/then/else *)

let choose b x y = if b then x else y
[%%expect{|
val choose :
  bool @ 'm ->
  'a @ [< 'n & global] -> 'a @ [< 'o] -> 'a @ [> 'o | 'n | dynamic] = <fun>
|}, Principal{|
val choose :
  bool @ [< global] ->
  'a @ [< 'm & global] -> 'a @ [< 'n] -> 'a @ [> 'n | 'm | dynamic] = <fun>
|}]

(* nested closures *)

let nest x = fun () -> fun () -> fun () -> x
[%%expect{|
val nest :
  'a @ [< 'm & global] -> unit @ 'n -> unit @ 'o -> unit @ 'p -> 'a @ [> 'm] =
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
  'a @ [< 'm & global] -> 'b @ [< 'n & global] -> 'a * 'b @ [> 'n | 'm] =
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
  ('a @ [> 'm | dynamic] -> 'b @ [< 'n & global]) @ [< global] ->
  'a option @ [< 'm] -> 'b option @ [> 'n | dynamic] = <fun>
|}]

(* Currying over three arguments *)

let triple x y z = (x, y, z)
[%%expect{|
val triple :
  'a @ [< 'm & global] ->
  'b @ [< 'n & global] ->
  'c @ [< 'o & global] -> 'a * 'b * 'c @ [> 'o | 'n | 'm] = <fun>
|}]

let flip f (x, y) = f (y, x)
[%%expect{|
val flip :
  ('a * 'b @ [> 'm] -> 'c @ [< 'n & global]) @ [< global] ->
  'b * 'a @ [< 'm & global] -> 'c @ [> 'n | dynamic] = <fun>
|}]

let flip f x y = f y x
[%%expect{|
val flip :
  ('a @ [> 'm] -> 'b @ [> 'n] -> 'c @ [< 'o & global]) @ [< global] ->
  'b @ [< 'n & global] -> 'a @ [< 'm] -> 'c @ [> 'o | dynamic] = <fun>
|}]


let flip f = fun x -> fun y -> f y x
[%%expect{|
val flip :
  ('a @ [> 'm] -> 'b @ [> 'n] -> 'c @ [< 'o & global]) @ [< global] ->
  'b @ [< 'n & global] -> 'a @ [< 'm] -> 'c @ [> 'o | dynamic] = <fun>
|}]

let stateful x : _ @ stateful = x
let stateful_nonportable x : _ @ stateful nonportable = x
[%%expect{|
val stateful : 'a @ [< 'm] -> 'a @ [> 'm | stateful] = <fun>
val stateful_nonportable : 'a @ [< 'm] -> 'a @ [> 'm | nonportable stateful] =
  <fun>
|}]

let opaque_return x y = Sys.opaque_identity x
let global_opaque_return x : _ @ global = fun y -> Sys.opaque_identity x
[%%expect{|
val opaque_return :
  'a @ [< global many uncontended forkable unyielding read_write] ->
  'b @ 'm -> 'a @ [> aliased nonportable stateful dynamic] = <fun>
val global_opaque_return :
  'a @ [< global many uncontended forkable unyielding read_write] ->
  ('b @ 'm -> 'a @ [> aliased nonportable stateful dynamic]) @ [< global > nonportable stateful] =
  <fun>
|}]

let _ : (string -> (unit -> string) @ local) ref = ref opaque_return
[%%expect{|
- : (string -> (unit -> string) @ local) ref = {contents = <fun>}
|}]

let _ : (string -> (unit -> string) @ local) ref = ref global_opaque_return
[%%expect{|
- : (string -> (unit -> string) @ local) ref = {contents = <fun>}
|}, Principal{|
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

type 'a cell = { mutable v : 'a }

let store_and_read c x () =
  c.v <- x;
  let _ = c.v in
  ()
[%%expect{|
type 'a cell = { mutable v : 'a; }
val store_and_read :
  'a cell @ [< global uncontended read_write] ->
  'a @ [< global many uncontended forkable unyielding read_write] ->
  unit @ 'm -> unit @ 'n = <fun>
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

let k a b = a
let foo x = (k x).contents
[%%expect{|
val k : 'a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm] = <fun>
Line 2, characters 12-17:
2 | let foo x = (k x).contents
                ^^^^^
Error: This expression has type "'a -> 'b" which is not a record type.
|}]

let optional_arg ?(g = fun x y -> x) () = ()
[%%expect{|
val optional_arg :
  ?g:('a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm]) @ [< global] ->
  unit @ 'o -> unit @ 'p = <fun>
|}]

let shared_curry g = [g; (fun x y -> y)]
[%%expect{|
val shared_curry :
  ('a @ ([< past('m) & global] as 'n) ->
   ('b @ ([< 'p] as 'q) -> 'b @ ([> 'p] as 'mm0)) @ ([> past('m)] as 'o)) @ [< 'mm1 & global] ->
  ('a @ 'n -> ('b @ 'q -> 'b @ 'mm0) @ 'o) list @ [> 'mm1] = <fun>
|}]

let labelled_curry ~g () = ignore (g == (fun x y -> x))
[%%expect{|
val labelled_curry :
  g:('a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm]) @ [< global many uncontended read_write] ->
  unit @ 'o -> unit @ [> dynamic] = <fun>
|}]

let optional_curry ?g () = ignore (g == Some (fun x y -> x))
[%%expect{|
val optional_curry :
  ?g:('a @ [< 'm & global] -> 'b @ 'n -> 'a @ [> 'm]) @ [< global many uncontended read_write] ->
  unit @ 'o -> unit @ [> dynamic] = <fun>
|}]


let curry_edge_to_printed_curry f a b c =
  ignore (f == fun _ (_ : int ref) _ -> ()); f a b c
[%%expect{|
val curry_edge_to_printed_curry :
  ('a @ [< past('m) & global > 'n] ->
   (int ref @ [< global > 'p mod many portable forkable unyielding stateless] ->
    'b @ [> 'q] -> unit @ [< global]) @ [> past('m) | past('o)]) @ [< past('o) & global many uncontended read_write] ->
  'a @ [< 'n & global] ->
  int ref @ [< 'p & global] -> 'b @ [< 'q] -> unit @ [> dynamic] = <fun>
|}, Principal{|
val curry_edge_to_printed_curry :
  ('a @ [< past('m) & global > 'n] ->
   (int ref @ [< global > 'p] -> 'b @ [> 'q] -> unit @ [< 'mm0 & global]) @ [> past('m) | past('o)]) @ [< past('o) & global many uncontended read_write] ->
  'a @ [< 'n & global] ->
  int ref @ [< 'p & global] -> 'b @ [< 'q] -> unit @ [> 'mm0 | dynamic] =
  <fun>
|}]

let curry_acc_mismatch_crash f k a =
  ignore (f == (fun x y z -> z));
  let h = f a in
  k (fun () -> let _ = h in ());
  ()
[%%expect{|
val curry_acc_mismatch_crash :
  ('a @ [< past('m) & global > 'n] ->
   ('b @ [< global] -> 'c @ [< 'p] -> 'c @ [> 'p]) @ [< past('o) > past('m)]) @ [< global many uncontended read_write] ->
  ((unit @ 'q -> unit @ 'mm0) @ [> past('o)] -> 'd @ 'mm1) @ [< global] ->
  'a @ [< 'n & global] -> unit @ 'mm2 = <fun>
|}]

let curry_acc_mismatch_imprecise f k a =
  ignore (f == (fun x (y @ local) z -> z));
  let h = f a in
  k (fun () -> let _ = h in ()); ()
[%%expect{|
val curry_acc_mismatch_imprecise :
  ('a @ [< past('m) & global > 'n] ->
   ('b @ [> local] -> 'c @ [< 'p] -> 'c @ [> 'p]) @ [< past('o) > past('m)]) @ [< global many uncontended read_write] ->
  ((unit @ 'q -> unit @ 'mm0) @ [> past('o)] -> 'd @ 'mm1) @ [< global] ->
  'a @ [< 'n & global] -> unit @ 'mm2 = <fun>
|}]

let empty_heads (x @ local once unforkable yielding)
    (f : a:int -> b:int -> _ @ local once -> _) g =
  exclave_ (if true then f x else fun ~a:_ -> g)
[%%expect{|
val empty_heads :
  'a @ [< 'm & uncontended read_write > local once unforkable yielding] ->
  (a:int -> (b:int -> 'a @ local once -> 'b)) @ [< global many forkable unyielding] ->
  (b:int -> 'b @ ([> aliased nonportable stateful dynamic] as 'n)) @ [< 'o mod aliased contended immutable] ->
  (a:int ->
   (b:int -> 'b @ 'n) @ [> 'o | local once nonportable unforkable yielding stateful]) @ [> 'm | local once nonportable unforkable yielding stateful dynamic] =
  <fun>
|}, Principal{|
val empty_heads :
  'a @ [< 'm & uncontended read_write > local once unforkable yielding] ->
  (a:int -> (b:int -> 'a @ local once -> 'b)) @ [< global many forkable unyielding] ->
  (b:int -> 'b @ ([> aliased nonportable stateful dynamic] as 'n)) @ [< 'o] ->
  (a:int ->
   (b:int -> 'b @ 'n) @ [> 'o | local once nonportable unforkable yielding stateful]) @ [> 'm | local once nonportable unforkable yielding stateful dynamic] =
  <fun>
|}]

module Curry_inclusion : sig val g : ('a -> int) -> unit end = struct
  let g f = ignore (f == (fun (x @ local) y z -> z))
end
[%%expect{|
Lines 1-3, characters 63-3:
1 | ...............................................................struct
2 |   let g f = ignore (f == (fun (x @ local) y z -> z))
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val g :
             ('a @ [> local] -> 'b @ 'm -> 'c @ [< 'n] -> 'c @ [> 'n]) @ [< many uncontended read_write] ->
             unit @ [> dynamic]
         end
       is not included in
         sig val g : ('a -> int) -> unit end
       Values do not match:
         val g :
           ('a @ [> local] -> 'b @ 'm -> 'c @ [< 'n] -> 'c @ [> 'n]) @ [< many uncontended read_write] ->
           unit @ [> dynamic]
       is not included in
         val g : ('a -> int) -> unit
       The type
         "('a @ [> local] -> 'b @ 'm -> 'c @ [< 'n] -> 'c @ [> 'n]) @ [< many uncontended read_write] ->
         unit @ [> dynamic]"
       is not compatible with the type "('a -> int) -> unit"
       Type "'b @ 'm -> 'c @ [< 'n] -> 'c @ [> 'n]" is not compatible with type
         "int"
|}]

let call_and_return f x = f x, f
[%%expect{|
val call_and_return :
  ('a @ ([> 'm] as 'n) -> 'b @ ([< 'o & global] as 'p)) @ [< past('q) & global many] ->
  'a @ [< 'm] ->
  'b * ('a @ 'n -> 'b @ 'p) @ [> 'o | past('q) | aliased dynamic] = <fun>
|}, Principal{|
val call_and_return :
  ('a @ ([> 'm] as 'n) -> 'b @ ([< 'o & global] as 'p)) @ [< 'q & global many] ->
  'a @ [< 'm] -> 'b * ('a @ 'n -> 'b @ 'p) @ [> 'o | 'q | aliased dynamic] =
  <fun>
|}]
