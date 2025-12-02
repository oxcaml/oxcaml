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
   and [typing-jkind-bounds/annots.ml]. Since separability is now a scannable axis,
   these different files are actually testing very similar code paths. *)

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
       The layout of t_maybeptr_val is value maybe_separable
         because of the definition of t_maybeptr_val at line 1, characters 0-43.
       But the layout of t_maybeptr_val must be a sublayout of immediate
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
       But the layout of t_nonptr must be a value layout
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
       The layout of string is value non_float
         because it is the primitive type string.
       But the layout of string must be a sublayout of immediate
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
       The layout of t_maybeptr_val is value maybe_separable
         because of the definition of t_maybeptr_val at line 1, characters 0-43.
       But the layout of t_maybeptr_val must be a sublayout of any separable
         because it's the type argument to the array type.
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
Error: The layout of type "fails" is value maybe_separable
         because it is an unboxed record.
       But the layout of type "fails" must be a sublayout of immediate
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
       The layout of a2 is value
         because of the annotation on the abstract type declaration for a2.
       But the layout of a2 must be a sublayout of immediate
         because of the definition of cant_promote_snd at line 4, characters 23-64.
|}]

let f () =
  let pass_np (_ : (_ : any non_pointer)) = () in
  pass_np #1.0
[%%expect{|
val f : unit -> unit = <fun>
|}]

(* or_null interaction: *)

let f (type a : immediate) (x : a) =
  let g (type b : immediate_or_null) (y : b) = () in
  g (This x : a or_null)
[%%expect{|
val f : ('a : immediate). 'a -> unit = <fun>
|}]

let f (x : 'a or_null) =
  let g (type b : immediate) (y : b) = () in
  match x with
  | This y -> g y
  | Null -> ()
[%%expect{|
val f : ('a : immediate). 'a or_null -> unit = <fun>
|}]

let outer (type a : value non_float) (nf : a or_null) =
  let f (x : 'a or_null) =
    let g (type b : value non_pointer) (y : b) = () in
    match x with
    | This y -> g y
    | Null -> ()
  in
  f nf
[%%expect{|
Line 8, characters 4-6:
8 |   f nf
        ^^
Error: This expression has type "a or_null"
       but an expression was expected of type "'a or_null"
       The layout of a is value non_float
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be a sublayout of immediate
         because of the definition of g at line 3, characters 10-51.
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
       The layout of 'a is value
         because of the definition of t at line 2, characters 2-29.
       But the layout of 'a must be a sublayout of immediate
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
       The layout of the first is value maybe_separable
         because of the definition of t_maybeptr_val at line 1, characters 0-43.
       But the layout of the first must be a sublayout of immediate
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

(* This test demonstrates a change in behavior now that the mod bound syntax
   gets parsed into scannable axes: before, mod bounds could only make non-modal
   axes lower (so [mod separable] would be a no-op, since [immediate] already
   meant [non_float]). But these now (currently, at least) override the
   existing scannable axes! *)

(* CR layouts-scannable: as we deprecate the old syntax, this small collection
   of tests should be removed. *)
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
       The layout of the first is value
         because of the definition of t at line 4, characters 2-34.
       But the layout of the first must be a sublayout of value non_float
         because of the definition of t at line 2, characters 2-26.
|}]

(* CR zeisbach: mod syntax actually applys on individual components of a
   product, which might be another change from the existing behavior??
   but when i tested it, it looked like the existing behavior did this,
   at least for non-modal axes. but it also seemed to throw some away.
   Oh btw this should go away once I rebase properly *)

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
