(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* CR layouts-scannable: These tests should test out the built-ins once they
   get updated to be appropriately non_pointer *)

(* type declarations *)

type t : value non_pointer
[%%expect{|
type t
|}]

type t : immutable_data non_pointer
[%%expect{|
type t : immutable_data
|}]

type ('a : any non_pointer, 'b : any maybe_pointer, 'c : any) t;;
[%%expect{|
type ('a : any, 'b : any, 'c : any) t
|}]

type t : value non_pointer & value maybe_pointer & float64
[%%expect{|
type t : value & value & float64
|}]

(* Checking non_pointer annotations, based on [typing-layouts-or-null/separability.ml]
   and [typing-jkind-bounds/annots.ml]. *)
(* CR layouts-scannable: Once separability is a scannable axis, move these! *)

(* Annotation on type parameters: *)

type t_maybeptr : any maybe_pointer
type t_nonptr : any non_pointer
[%%expect{|
type t_maybeptr : any
type t_nonptr : any
|}]

type t_maybeptr_val : value maybe_pointer
type t_nonptr_val : value non_pointer
[%%expect{|
type t_maybeptr_val
type t_nonptr_val
|}]

type ('a : any maybe_pointer) accepts_maybeptr
type ('a : any non_pointer) accepts_nonptr
[%%expect{|
type ('a : any) accepts_maybeptr
type ('a : any) accepts_nonptr
|}]

type ('a : value maybe_pointer) accepts_maybeptr_val
type ('a : value non_pointer) accepts_nonptr_val
[%%expect{|
type 'a accepts_maybeptr_val
type 'a accepts_nonptr_val
|}]

type succeeds = t_maybeptr accepts_maybeptr
type succeeds = t_nonptr accepts_maybeptr
type succeeds = t_maybeptr_val accepts_maybeptr_val
type succeeds = t_nonptr_val accepts_maybeptr_val
[%%expect{|
type succeeds = t_maybeptr accepts_maybeptr
type succeeds = t_nonptr accepts_maybeptr
type succeeds = t_maybeptr_val accepts_maybeptr_val
type succeeds = t_nonptr_val accepts_maybeptr_val
|}]

type fails = t_maybeptr accepts_nonptr
[%%expect{|
type fails = t_maybeptr accepts_nonptr
|}]
type succeeds = t_nonptr accepts_nonptr
[%%expect{|
type succeeds = t_nonptr accepts_nonptr
|}]

type fails = t_maybeptr_val accepts_nonptr_val
[%%expect{|
type fails = t_maybeptr_val accepts_nonptr_val
|}]
type succeeds = t_nonptr_val accepts_nonptr_val
[%%expect{|
type succeeds = t_nonptr_val accepts_nonptr_val
|}]

type succeeds = t_nonptr_val accepts_nonptr
type fails = t_nonptr accepts_nonptr_val
[%%expect{|
type succeeds = t_nonptr_val accepts_nonptr
Line 2, characters 13-21:
2 | type fails = t_nonptr accepts_nonptr_val
                 ^^^^^^^^
Error: This type "t_nonptr" should be an instance of type "('a : value)"
       The layout of t_nonptr is any
         because of the definition of t_nonptr at line 2, characters 0-31.
       But the layout of t_nonptr must be a sublayout of value
         because of the definition of accepts_nonptr_val at line 2, characters 0-48.
|}]

(* when the layout is not value, the scannable axes should not be relevant *)
type succeeds = float# accepts_maybeptr
type succeeds = float# accepts_nonptr
[%%expect{|
type succeeds = float# accepts_maybeptr
type succeeds = float# accepts_nonptr
|}]

type succeeds = #(t_nonptr * t_maybeptr) accepts_maybeptr
type succeeds = #(t_nonptr * t_maybeptr) accepts_nonptr
type succeeds = #(t_nonptr_val * t_nonptr) accepts_nonptr
type succeeds = #(t_maybeptr_val * t_maybeptr) accepts_nonptr
[%%expect{|
type succeeds = #(t_nonptr * t_maybeptr) accepts_maybeptr
type succeeds = #(t_nonptr * t_maybeptr) accepts_nonptr
type succeeds = #(t_nonptr_val * t_nonptr) accepts_nonptr
type succeeds = #(t_maybeptr_val * t_maybeptr) accepts_nonptr
|}]

(* CR layouts-scannable: There are versions of these tests that use [immediate]
   instead of [value non_pointer]. Once [immediate] means that, these will be
   redundant and can probably be removed *)
type 'a t = 'a accepts_nonptr
type ('a : value) t = 'a accepts_nonptr
[%%expect{|
type 'a t = 'a accepts_nonptr
type 'a t = 'a accepts_nonptr
|}]

let f : ('a : value non_pointer) accepts_nonptr -> ('a : value) accepts_nonptr = fun x -> x
let f : ('a : value non_pointer). 'a accepts_nonptr -> 'a accepts_nonptr = fun x -> x
let f : ('a : value). 'a accepts_nonptr -> 'a accepts_nonptr = fun x -> x
[%%expect{|
val f : 'a accepts_nonptr -> 'a accepts_nonptr = <fun>
val f : 'a accepts_nonptr -> 'a accepts_nonptr = <fun>
val f : 'a accepts_nonptr -> 'a accepts_nonptr = <fun>
|}]

let f : (_ : value) accepts_nonptr -> unit = fun _ -> ()
let g : (_ : value non_pointer) accepts_nonptr -> unit = fun _ -> ()
[%%expect{|
val f : 'a accepts_nonptr -> unit = <fun>
val g : 'a accepts_nonptr -> unit = <fun>
|}]

let f : (_ : value non_pointer) -> (_ : value) = fun _ -> assert false
let g : (_ : value) -> (_ : value non_pointer) = fun _ -> assert false
[%%expect{|
val f : 'a -> 'b = <fun>
val g : 'a -> 'b = <fun>
|}]

(* CR layouts-separability: Once [immediate] means [scannable non_pointer],
   add tests for this behavior! *)

(* unboxed records *)

type fails : value non_pointer = #{ a : t_maybeptr_val }
[%%expect{|
type fails = #{ a : t_maybeptr_val; }
|}]
type succeeds : value non_pointer = #{ a : t_nonptr_val }
[%%expect{|
type succeeds = #{ a : t_nonptr_val; }
|}]

type succeeds : value non_pointer & value non_pointer = #{ a : t_nonptr_val; b : t_nonptr_val }
[%%expect{|
type succeeds = #{ a : t_nonptr_val; b : t_nonptr_val; }
|}]

(* Annotation on types in functions *)

let f (a : (_ : any non_pointer)) (b : (_ : any maybe_pointer)) =
  let _unify_them = [ a; b ] in
  ()
[%%expect{|
val f : 'a -> 'a -> unit = <fun>
|}]

let f x =
  let g (x : (_ : any non_pointer)) = () in
  g x
[%%expect{|
val f : 'a -> unit = <fun>
|}]

let f (type a : value maybe_pointer) (x : a) =
  let require_np (y : (_ : value non_pointer)) = () in
  require_np y
[%%expect{|
Line 3, characters 13-14:
3 |   require_np y
                 ^
Error: Unbound value "y"
|}]

let f (type a : float64 maybe_pointer) (x : a) =
  let g (x : (_ : float64 non_pointer)) = () in
  g x
[%%expect{|
val f : ('a : float64). 'a -> unit = <fun>
|}]

let f (type a : value non_pointer) (x : a) =
  (* here, y is value maybe_pointer *)
  let g y = () in
  g x
[%%expect{|
val f : 'a -> unit = <fun>
|}]

let f (t : (_ : value non_pointer & value)) =
  (* here, x is value maybe_pointer *)
  let g (type a : value non_pointer) (x : a) = () in
  let #(np, v) = t in
  g np
[%%expect{|
val f : #('a * 'b) -> unit = <fun>
|}]

let f (prod : (_ : value non_pointer & value)) =
  let should_promote_snd (type a : value non_pointer) (x : a) = () in
  let #(np, v) = prod in
  should_promote_snd v
[%%expect{|
val f : #('a * 'b) -> unit = <fun>
|}]

let f (type a1 : value non_pointer) (type a2 : value) (a1 : a1) (a2 : a2) =
  let make_a_product = #(a1, a2) in
  let #(np, v) = make_a_product in
  let g (type b : value non_pointer) (x : b) = () in
  g np
[%%expect{|
val f : 'a1 -> 'a2 -> unit = <fun>
|}]

let f (type a1 : value non_pointer) (type a2 : value) (a1 : a1) (a2 : a2) =
  let make_a_product = #(a1, a2) in
  let #(np, v) = make_a_product in
  let cant_promote_snd (type a : value non_pointer) (x : a) = () in
  cant_promote_snd v
[%%expect{|
val f : 'a1 -> 'a2 -> unit = <fun>
|}]

let f () =
  let pass_np (_ : (_ : any non_pointer)) = () in
  pass_np #1.0
[%%expect{|
val f : unit -> unit = <fun>
|}]

(* modules and module inclusion *)

module M (X : sig type t : any non_pointer end) : sig type t : any end = X
module M (X : sig type t : any end) : sig type t : any non_pointer end = X
[%%expect{|
module M : functor (X : sig type t : any end) -> sig type t : any end
module M : functor (X : sig type t : any end) -> sig type t : any end
|}]

module M1 : sig
  type ('a : value non_pointer) t : value
end = struct
  type ('a : value maybe_pointer) t = t_nonptr_val
end
[%%expect{|
module M1 : sig type 'a t end
|}]

module FailingTypeParam : sig
  type ('a : value) t : value
end = struct
  type ('a : value non_pointer) t = int
end
[%%expect{|
module FailingTypeParam : sig type 'a t end
|}]

module FailingTypeDecl : sig
  type ('a : value non_pointer) t : value non_pointer
end = struct
  type ('a : value) t = t_maybeptr_val
end
[%%expect{|
module FailingTypeDecl : sig type 'a t end
|}]

external[@layout_poly] id : ('a : any non_pointer). 'a -> 'a = "%identity"
let id' x = id x
[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
val id' : 'a -> 'a = <fun>
|}]
