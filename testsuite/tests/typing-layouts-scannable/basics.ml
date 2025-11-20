(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* type declarations *)

type t : value non_pointer
[%%expect{|
type t : value non_pointer
|}]

type t : immutable_data non_pointer
[%%expect{|
type t : immutable_data non_pointer
|}]

type ('a : any non_pointer, 'b : any maybe_separable, 'c : any) t;;
[%%expect{|
type ('a : any non_pointer, 'b : any, 'c : any) t
|}]

type t : value non_pointer & value maybe_separable & float64
[%%expect{|
type t : value non_pointer & value maybe_separable & float64
|}]

(* Checking non_pointer annotations, based on [typing-layouts-or-null/separability.ml]
   and [typing-jkind-bounds/annots.ml]. *)
(* CR zeisbach: Once separability is a scannable axis, move these! *)

(* Annotation on type parameters: *)

type t_maybeptr : any maybe_separable
type t_nonptr : any non_pointer
[%%expect{|
type t_maybeptr : any
type t_nonptr : any non_pointer
|}]

type t_maybeptr_val : value maybe_separable
type t_nonptr_val : value non_pointer
[%%expect{|
type t_maybeptr_val : value maybe_separable
type t_nonptr_val : value non_pointer
|}]

type ('a : any maybe_separable) accepts_maybeptr
type ('a : any non_pointer) accepts_nonptr
[%%expect{|
type ('a : any) accepts_maybeptr
type ('a : any non_pointer) accepts_nonptr
|}]

type ('a : value maybe_separable) accepts_maybeptr_val
type ('a : value non_pointer) accepts_nonptr_val
[%%expect{|
type ('a : value maybe_separable) accepts_maybeptr_val
type ('a : value non_pointer) accepts_nonptr_val
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
Line 1, characters 13-23:
1 | type fails = t_maybeptr accepts_nonptr
                 ^^^^^^^^^^
Error: This type "t_maybeptr" should be an instance of type
         "('a : any non_pointer)"
       The layout of t_maybeptr is any
         because of the definition of t_maybeptr at line 1, characters 0-37.
       But the layout of t_maybeptr must be a sublayout of any non_pointer
         because of the definition of accepts_nonptr at line 2, characters 0-42.
|}]
type succeeds = t_nonptr accepts_nonptr
[%%expect{|
type succeeds = t_nonptr accepts_nonptr
|}]

type fails = t_maybeptr_val accepts_nonptr_val
[%%expect{|
Line 1, characters 13-27:
1 | type fails = t_maybeptr_val accepts_nonptr_val
                 ^^^^^^^^^^^^^^
Error: This type "t_maybeptr_val" should be an instance of type
         "('a : value non_pointer)"
       The layout of t_maybeptr_val is scannable
         because of the definition of t_maybeptr_val at line 1, characters 0-43.
       But the layout of t_maybeptr_val must be a sublayout of
           scannable non_pointer
         because of the definition of accepts_nonptr_val at line 2, characters 0-48.
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
Error: This type "t_nonptr" should be an instance of type
         "('a : value non_pointer)"
       The layout of t_nonptr is any non_pointer
         because of the definition of t_nonptr at line 2, characters 0-31.
       But the layout of t_nonptr must be a sublayout of
           scannable non_pointer
         because of the definition of accepts_nonptr_val at line 2, characters 0-48.
|}]

(* when the layout is not value, the scannable axes should not be relevant *)
type succeeds = float# accepts_maybeptr
type succeeds = float# accepts_nonptr
[%%expect{|
type succeeds = float# accepts_maybeptr
type succeeds = float# accepts_nonptr
|}]

(* [int] is in fact [non_pointer], since it is an [immediate] *)
type succeeds = int accepts_nonptr
type succeeds = int accepts_nonptr_val
[%%expect{|
type succeeds = int accepts_nonptr
type succeeds = int accepts_nonptr_val
|}]

type fails = string accepts_nonptr_val
[%%expect{|
Line 1, characters 13-19:
1 | type fails = string accepts_nonptr_val
                 ^^^^^^
Error: This type "string" should be an instance of type
         "('a : value non_pointer)"
       The layout of string is scannable non_float
         because it is the primitive type string.
       But the layout of string must be a sublayout of scannable non_pointer
         because of the definition of accepts_nonptr_val at line 2, characters 0-48.
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

type succeeds = t_nonptr array
type succeeds = t_nonptr_val array
type fails = t_maybeptr_val array
[%%expect{|
type succeeds = t_nonptr array
type succeeds = t_nonptr_val array
Line 3, characters 13-27:
3 | type fails = t_maybeptr_val array
                 ^^^^^^^^^^^^^^
Error: This type "t_maybeptr_val" should be an instance of type
         "('a : any separable)"
       The layout of t_maybeptr_val is scannable
         because of the definition of t_maybeptr_val at line 1, characters 0-43.
       But the layout of t_maybeptr_val must be a sublayout of any separable
         because it's the type argument to the array type.
|}]

(* CR zeisbach: There are versions of these tests that use [immediate]
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

(* unboxed records *)

type t_maybeptr_val : value maybe_separable
type t_nonptr_val : value non_pointer
[%%expect{|
type t_maybeptr_val : value maybe_separable
type t_nonptr_val : value non_pointer
|}]

type fails : value non_pointer = #{ a : t_maybeptr_val }
[%%expect{|
Line 1, characters 0-56:
1 | type fails : value non_pointer = #{ a : t_maybeptr_val }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "fails" is scannable
         because it is an unboxed record.
       But the layout of type "fails" must be a sublayout of
           scannable non_pointer
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

let f (a : (_ : any non_pointer)) (b : (_ : any maybe_separable)) =
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

let f (type a : value maybe_separable) (x : a) =
  let require_np (y : (_ : value non_pointer)) = () in
  require_np y
[%%expect{|
Line 3, characters 13-14:
3 |   require_np y
                 ^
Error: Unbound value "y"
|}]

let f (type a : float64 maybe_separable) (x : a) =
  let g (x : (_ : float64 non_pointer)) = () in
  g x
[%%expect{|
val f : ('a : float64). 'a -> unit = <fun>
|}]

let f (type a : value non_pointer) (x : a) =
  (* here, y is value maybe_separable *)
  let g y = () in
  g x
[%%expect{|
val f : ('a : value non_pointer). 'a -> unit = <fun>
|}]

let f (t : (_ : value non_pointer & value)) =
  (* here, x is value maybe_separable *)
  let g (type a : value non_pointer) (x : a) = () in
  let #(np, v) = t in
  g np
[%%expect{|
val f : ('a : value non_pointer) 'b. #('a * 'b) -> unit = <fun>
|}]

let f (prod : (_ : value non_pointer & value)) =
  let should_promote_snd (type a : value non_pointer) (x : a) = () in
  let #(np, v) = prod in
  should_promote_snd v
[%%expect{|
val f : ('a : value non_pointer) ('b : value non_pointer). #('a * 'b) -> unit =
  <fun>
|}]

let f (type a1 : value non_pointer) (type a2 : value) (a1 : a1) (a2 : a2) =
  let make_a_product = #(a1, a2) in
  let #(np, v) = make_a_product in
  let g (type b : value non_pointer) (x : b) = () in
  g np
[%%expect{|
val f : ('a1 : value non_pointer) 'a2. 'a1 -> 'a2 -> unit = <fun>
|}]

let f (type a1 : value non_pointer) (type a2 : value) (a1 : a1) (a2 : a2) =
  let make_a_product = #(a1, a2) in
  let #(np, v) = make_a_product in
  let cant_promote_snd (type a : value non_pointer) (x : a) = () in
  cant_promote_snd v
[%%expect{|
Line 5, characters 19-20:
5 |   cant_promote_snd v
                       ^
Error: This expression has type "a2" but an expression was expected of type
         "('a : value non_pointer)"
       The layout of a2 is scannable separable
         because of the annotation on the abstract type declaration for a2.
       But the layout of a2 must be a sublayout of scannable non_pointer
         because of the definition of cant_promote_snd at line 4, characters 23-64.
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
  type ('a : value maybe_separable) t = t_nonptr_val
end
[%%expect{|
module M1 : sig type ('a : value non_pointer) t end
|}]

module FailingTypeParam : sig
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
       The layout of 'a is scannable separable
         because of the definition of t at line 2, characters 2-29.
       But the layout of 'a must be a sublayout of scannable non_pointer
         because of the definition of t at line 4, characters 2-39.
|}]

module FailingTypeDecl : sig
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
       The layout of the first is scannable
         because of the definition of t_maybeptr_val at line 1, characters 0-43.
       But the layout of the first must be a sublayout of
           scannable non_pointer
         because of the definition of t at line 2, characters 2-53.
|}]

external[@layout_poly] id : ('a : any non_pointer). 'a -> 'a = "%identity"
let id' x = id x
[%%expect{|
external id : ('a : any non_pointer). 'a -> 'a = "%identity" [@@layout_poly]
val id' : ('a : value_or_null non_pointer). 'a -> 'a = <fun>
|}]

(* legacy syntax for specifying scannable axes via mod bounds *)

module M : sig
  type t : value non_float
end = struct
  type t : value mod non_float
end
[%%expect{|
module M : sig type t : value non_float end
|}]

(* CR zeisbach: this test used to test with the old mod bounds, since the
   annotation would only _lower_ things. *)

module M : sig
  type t : value non_float
end = struct
  type t : immediate mod separable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : immediate mod separable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : immediate separable end
       is not included in
         sig type t : value non_float end
       Type declarations do not match:
         type t : immediate separable
       is not included in
         type t : value non_float
       The layout of the first is scannable separable
         because of the definition of t at line 4, characters 2-34.
       But the layout of the first must be a sublayout of scannable non_float
         because of the definition of t at line 2, characters 2-26.
|}]

(* CR zeisbach: mod syntax actually applys on individual components of a
   product, which might be another change from the existing behavior??
   but when i tested it, it looked like the existing behavior did this,
   at least for non-modal axes. but it also seemed to throw some away. *)

module M : sig
  type t : value non_float & value non_float
end = struct
  type t : (value mod non_float) & (value mod non_float)
end
[%%expect{|
module M : sig type t : value non_float & value non_float end
|}]

module M : sig
  type t : (value & value) mod non_float
end = struct
  type t : (value mod non_float) & value
end
[%%expect{|
module M : sig type t : value & value end
|}]

module M : sig
  type t : value non_float
end = struct
  type t : value mod separable mod non_float
end
[%%expect{|
module M : sig type t : value non_float end
|}]
