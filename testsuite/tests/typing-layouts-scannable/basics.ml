(* TEST
 flags = "-extension layouts_alpha -w +184+185";
 expect;
*)

(* CR layouts-scannable: These tests should test out the built-ins once they
   get updated to be appropriately non_pointer *)

(* type declarations *)

type t : value non_pointer
[%%expect{|
type t : value non_pointer
|}]

type t : immutable_data non_pointer
[%%expect{|
type t : immutable_data non_pointer
|}]

type ('a : any non_pointer, 'b : any maybe_pointer, 'c : any) t;;
[%%expect{|
type ('a : any non_pointer, 'b : any, 'c : any) t
|}]

type t : value non_pointer & value maybe_pointer & float64
[%%expect{|
type t : value non_pointer & value & float64
|}]

(* Checking non_pointer annotations, based on [typing-layouts-or-null/separability.ml]
   and [typing-jkind-bounds/annots.ml]. *)
(* CR layouts-scannable: Once separability is a scannable axis, move these! *)

(* Annotation on type parameters: *)

type t_maybeptr : any maybe_pointer
type t_nonptr : any non_pointer
[%%expect{|
type t_maybeptr : any
type t_nonptr : any non_pointer
|}]

type ('a : any maybe_pointer) accepts_maybeptr
type ('a : any non_pointer) accepts_nonptr
[%%expect{|
type ('a : any) accepts_maybeptr
type ('a : any non_pointer) accepts_nonptr
|}]

type succeeds = t_maybeptr accepts_maybeptr
type succeeds = t_nonptr accepts_maybeptr
[%%expect{|
type succeeds = t_maybeptr accepts_maybeptr
type succeeds = t_nonptr accepts_maybeptr
|}]

type fails = t_maybeptr accepts_nonptr
[%%expect{|
Line 1, characters 13-23:
1 | type fails = t_maybeptr accepts_nonptr
                 ^^^^^^^^^^
Error: This type "t_maybeptr" should be an instance of type
         "('a : any non_pointer)"
       The layout of t_maybeptr is any
         because of the definition of t_maybeptr at line 1, characters 0-35.
       But the layout of t_maybeptr must be a sublayout of any non_pointer
         because of the definition of accepts_nonptr at line 2, characters 0-42.
|}]
type succeeds = t_nonptr accepts_nonptr
[%%expect{|
type succeeds = t_nonptr accepts_nonptr
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
[%%expect{|
type succeeds = #(t_nonptr * t_maybeptr) accepts_maybeptr
type succeeds = #(t_nonptr * t_maybeptr) accepts_nonptr
|}]

(* CR layouts-scannable: There are versions of these tests that use [immediate]
   instead of [value non_pointer]. Once [immediate] means that, these will be
   redundant and can probably be removed *)
type 'a t = 'a accepts_nonptr
type ('a : value) t = 'a accepts_nonptr
[%%expect{|
type ('a : value non_pointer) t = 'a accepts_nonptr
type ('a : value non_pointer) t = 'a accepts_nonptr
|}]

let f : ('a : value non_pointer) accepts_nonptr -> ('a : value) accepts_nonptr = fun x -> x
let f : ('a : value non_pointer). 'a accepts_nonptr -> 'a accepts_nonptr = fun x -> x
let f : ('a : value). 'a accepts_nonptr -> 'a accepts_nonptr = fun x -> x
[%%expect{|
val f : ('a : value non_pointer). 'a accepts_nonptr -> 'a accepts_nonptr =
  <fun>
val f : ('a : value non_pointer). 'a accepts_nonptr -> 'a accepts_nonptr =
  <fun>
Line 3, characters 8-60:
3 | let f : ('a : value). 'a accepts_nonptr -> 'a accepts_nonptr = fun x -> x
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The universal type variable 'a was declared to have kind value.
       But it was inferred to have kind value non_pointer
         because of the definition of accepts_nonptr at line 2, characters 0-42.
|}]

let f : (_ : value) accepts_nonptr -> unit = fun _ -> ()
let g : (_ : value non_pointer) accepts_nonptr -> unit = fun _ -> ()
[%%expect{|
val f : ('a : value non_pointer). 'a accepts_nonptr -> unit = <fun>
val g : ('a : value non_pointer). 'a accepts_nonptr -> unit = <fun>
|}]

let f : (_ : value non_pointer) -> (_ : value) = fun _ -> assert false
let g : (_ : value) -> (_ : value non_pointer) = fun _ -> assert false
[%%expect{|
val f : ('a : value non_pointer) 'b. 'a -> 'b = <fun>
val g : 'a ('b : value non_pointer). 'a -> 'b = <fun>
|}]

(* CR layouts-separability: as base jkinds become non_pointer, test these!
   including the interesting cases where annotations don't matter *)

(* unboxed records *)

type t_maybeptr_val : value maybe_pointer
type t_nonptr_val : value non_pointer
[%%expect{|
type t_maybeptr_val
type t_nonptr_val : value non_pointer
|}]

type fails : value non_pointer = #{ a : t_maybeptr_val }
[%%expect{|
Line 1, characters 0-56:
1 | type fails : value non_pointer = #{ a : t_maybeptr_val }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "fails" is value
         because it is an unboxed record.
       But the layout of type "fails" must be a sublayout of value non_pointer
         because of the annotation on the declaration of the type fails.
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
val f : ('a : value_or_null non_pointer). 'a -> 'a -> unit = <fun>
|}]

let f x =
  let g (x : (_ : any non_pointer)) = () in
  g x
[%%expect{|
val f : ('a : value_or_null non_pointer). 'a -> unit = <fun>
|}]

let f (type a : value maybe_pointer) (x : a) =
  let g (x : (_ : value non_pointer)) = () in
  g x
[%%expect{|
Line 3, characters 4-5:
3 |   g x
        ^
Error: This expression has type "a" but an expression was expected of type
         "('a : value non_pointer)"
       The layout of a is value
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a sublayout of value non_pointer
         because of the definition of g at line 2, characters 8-42.
|}]

let f (type a : value non_pointer) (x : a) =
  (* here, x is value maybe_pointer *)
  let g x = () in
  g x
[%%expect{|
val f : ('a : value non_pointer). 'a -> unit = <fun>
|}]

let f (t : (_ : value non_pointer & value)) =
  (* here, x is value maybe_pointer *)
  let g (type a : value non_pointer) (x : a) = () in
  let #(x, y) = t in
  g x
[%%expect{|
val f : ('a : value non_pointer) 'b. #('a * 'b) -> unit = <fun>
|}]

let f (t : (_ : value non_pointer & value)) =
  (* here, x is value maybe_pointer *)
  let g (type a : value non_pointer) (x : a) = () in
  let #(x, y) = t in
  g y
[%%expect{|
val f : ('a : value non_pointer) ('b : value non_pointer). #('a * 'b) -> unit =
  <fun>
|}]

(* modules and module inclusion *)

module M (X : sig type t : any non_pointer end) : sig type t : any end = X
module M (X : sig type t : any end) : sig type t : any non_pointer end = X
[%%expect{|
module M :
  functor (X : sig type t : any non_pointer end) -> sig type t : any end
Line 2, characters 73-74:
2 | module M (X : sig type t : any end) : sig type t : any non_pointer end = X
                                                                             ^
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t end
       is not included in
         sig type t : any non_pointer end
       Type declarations do not match:
         type t = X.t
       is not included in
         type t : any non_pointer
       The layout of the first is any
         because of the definition of t at line 2, characters 18-30.
       But the layout of the first must be a sublayout of any non_pointer
         because of the definition of t at line 2, characters 42-66.
|}]

module M1 : sig
  type ('a : value non_pointer) t : value
end = struct
  type ('a : value maybe_pointer) t = t_nonptr_val
end
[%%expect{|
module M1 : sig type ('a : value non_pointer) t end
|}]

module MContravariantFailing : sig
  type ('a : value) t : value
end = struct
  type ('a : value non_pointer) t = int
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value non_pointer) t = int
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type ('a : value non_pointer) t = int end
       is not included in
         sig type 'a t end
       Type declarations do not match:
         type ('a : value non_pointer) t = int
       is not included in
         type 'a t
       The problem is in the kinds of a parameter:
       The layout of 'a is value
         because of the definition of t at line 2, characters 2-29.
       But the layout of 'a must be a sublayout of value non_pointer
         because of the definition of t at line 4, characters 2-39.
|}]

module MCovariantFailing : sig
  type ('a : value non_pointer) t : value non_pointer
end = struct
  type ('a : value) t = t_maybeptr_val
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type ('a : value) t = t_maybeptr_val
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = t_maybeptr_val end
       is not included in
         sig type ('a : value non_pointer) t : value non_pointer end
       Type declarations do not match:
         type 'a t = t_maybeptr_val
       is not included in
         type ('a : value non_pointer) t : value non_pointer
       The layout of the first is value
         because of the definition of t_maybeptr_val at line 1, characters 0-41.
       But the layout of the first must be a sublayout of value non_pointer
         because of the definition of t at line 2, characters 2-53.
|}]

external[@layout_poly] id : ('a : any non_pointer). 'a -> 'a = "%identity"
let id' x = id x
[%%expect{|
external id : ('a : any non_pointer). 'a -> 'a = "%identity" [@@layout_poly]
val id' : ('a : value_or_null non_pointer). 'a -> 'a = <fun>
|}]
