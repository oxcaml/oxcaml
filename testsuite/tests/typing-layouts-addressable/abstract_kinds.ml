(* TEST
 flags = "-extension layouts_alpha";
 {
   expect;
 }{
   flags = "-extension layouts_alpha -no-ikinds";
   expect;
 }
*)

(* Tests for the [addressable] kind operator applied to abstract kinds. The
   operator cannot be pushed into an unexpanded kind path, so it is recorded
   on the kind constructor and applied when the path is expanded or
   substituted. *)

kind_ k

type t : k addressable
[%%expect{|
kind_ k
type t : k addressable
|}]

(* The operator is idempotent, so a second application is redundant *)
type t2 : k addressable addressable
[%%expect{|
Line 1, characters 24-35:
1 | type t2 : k addressable addressable
                            ^^^^^^^^^^^
Warning 183 [redundant-kind-modifier]: This kind modifier, or a stronger one,
  is already implied by the kind "k addressable".

type t2 : k addressable
|}]

(* [k addressable <= any addressable] for any abstract [k]... *)
type ('a : any addressable) req

type ok = t req
[%%expect{|
type ('a : any addressable) req
type ok = t req
|}]

(* ...but a plain [k] is of unknown addressability *)
type tk : k

type bad = tk req
[%%expect{|
type tk : k
Line 3, characters 11-13:
3 | type bad = tk req
               ^^
Error: This type "tk" should be an instance of type "('a : any addressable)"
       The kind of tk is k
         because of the definition of tk at line 1, characters 0-11.
       But the kind of tk must be a subkind of any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

(* The intersection of [k addressable] and [any addressable] is [k addressable]
*)
type ('a : k addressable) refined = 'a req
[%%expect{|
type ('a : k addressable) refined = 'a req
|}]

(* Scannable axes and addressability commute in intersections: the annotation
   contributes addressability, and the bound a scannable axis... *)
type ('a : any non_null) req_nn

type ('a : k addressable) refined_nn = 'a req_nn
[%%expect{|
type ('a : any non_null) req_nn
Line 3, characters 6-24:
3 | type ('a : k addressable) refined_nn = 'a req_nn
          ^^^^^^^^^^^^^^^^^^
Warning 181 [imprecise-kind-annotation]: The type variable `'a'
  was annotated with kind `k addressable'
  but was inferred to have kind `k non_null addressable'.

type ('a : k non_null addressable) refined_nn = 'a req_nn
|}]

(* ...or the annotation contributes addressability and one axis, and the
   bound another axis *)
type ('a : any non_pointer) req_np

type ('a : k addressable non_null) refined_np = 'a req_np
[%%expect{|
type ('a : any non_pointer) req_np
Line 3, characters 6-33:
3 | type ('a : k addressable non_null) refined_np = 'a req_np
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 181 [imprecise-kind-annotation]: The type variable `'a'
  was annotated with kind `k non_null addressable'
  but was inferred to have kind `k non_pointer non_null addressable'.

type ('a : k non_pointer non_null addressable) refined_np = 'a req_np
|}]

type ('a : k non_null) bad_refined = 'a req
[%%expect{|
Line 1, characters 37-39:
1 | type ('a : k non_null) bad_refined = 'a req
                                         ^^
Error: This type "('a : k non_null)" should be an instance of type
         "('b : any addressable)"
       The layout of 'a is the abstract kind k
         because of the annotation on 'a in the declaration of the type
                                      bad_refined.
       But the layout of 'a must overlap with any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

(* The same commutation for subkinding *)
module M : sig
  type t : any non_null addressable
end = struct
  type t : k addressable non_null
end
[%%expect{|
module M : sig type t : any non_null addressable end
|}]

(* ...whereas [k addressable]'s nullability is unknown *)
module M : sig
  type t : any non_null addressable
end = struct
  type t : k addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : k addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : k addressable end
       is not included in
         sig type t : any non_null addressable end
       Type declarations do not match:
         type t : k addressable
       is not included in
         type t : any non_null addressable
       The kind of the first is k addressable
         because of the definition of t at line 4, characters 2-24.
       But the kind of the first must be a subkind of
           any non_null addressable
         because of the definition of t at line 2, characters 2-35.
|}]

type ('a : k) not_refined = 'a req
[%%expect{|
Line 1, characters 28-30:
1 | type ('a : k) not_refined = 'a req
                                ^^
Error: This type "('a : k)" should be an instance of type
         "('b : any addressable)"
       The layout of 'a is the abstract kind k
         because of the annotation on 'a in the declaration of the type
                                      not_refined.
       But the layout of 'a must overlap with any addressable
         because of the definition of req at line 1, characters 0-31.
|}]

(* [k addressable] and [k] are incomparable for an abstract [k] *)
module M : sig
  type t : k addressable
end = struct
  type t : k
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : k
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : k end
       is not included in
         sig type t : k addressable end
       Type declarations do not match:
         type t : k
       is not included in
         type t : k addressable
       The kind of the first is k
         because of the definition of t at line 4, characters 2-12.
       But the kind of the first must be a subkind of k addressable
         because of the definition of t at line 2, characters 2-24.
|}]

module M : sig
  type t : k
end = struct
  type t : k addressable
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : k addressable
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : k addressable end
       is not included in
         sig type t : k end
       Type declarations do not match:
         type t : k addressable
       is not included in
         type t : k
       The kind of the first is k addressable
         because of the definition of t at line 4, characters 2-24.
       But the kind of the first must be a subkind of k
         because of the definition of t at line 2, characters 2-12.
|}]

module M : sig
  type t : k addressable
end = struct
  type t : k addressable
end
[%%expect{|
module M : sig type t : k addressable end
|}]

(* Kind aliases of made-addressable abstract kinds *)
kind_ ka = k addressable

type t' : ka

module M : sig
  type t : k addressable
end = struct
  type t : ka
end
[%%expect{|
kind_ ka = k addressable
type t' : k addressable
module M : sig type t : k addressable end
|}]

(* The pending operator is applied when the kind is substituted. [bits64] is
   addressable, so [k addressable] becomes just [bits64]... *)
module type S = sig
  kind_ k

  type t : k addressable

  val mk : unit -> t
end

module M64 : S with kind_ k = bits64 = struct
  kind_ k = bits64

  type t = int64_u

  let mk () = #0L
end

let use () = M64.mk ()
[%%expect{|
module type S = sig kind_ k type t : k addressable val mk : unit -> t end
module M64 : sig kind_ k = bits64 type t : bits64 val mk : unit -> t end
val use : unit -> M64.t = <fun>
|}]

(* ...whereas [bits8] is not, so [k addressable] becomes [bits8 addressable],
   which [int8#]'s kind [bits8] does not satisfy *)
module Bad : S with kind_ k = bits8 = struct
  kind_ k = bits8

  type t = int8#

  let mk () = assert false
end
[%%expect{|
Lines 1-7, characters 38-3:
1 | ......................................struct
2 |   kind_ k = bits8
3 |
4 |   type t = int8#
5 |
6 |   let mk () = assert false
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = bits8 type t = int8# val mk : unit -> 'a end
       is not included in
         sig
           kind_ k = bits8
           type t : bits8 addressable
           val mk : unit -> t
         end
       Type declarations do not match:
         type t = int8#
       is not included in
         type t : bits8 addressable
       The layout of the first is bits8
         because it is the unboxed version of the primitive type int8.
       But the layout of the first must be a sublayout of bits8 addressable
         because of the definition of t at line 4, characters 2-24.
|}]

module type T = sig
  kind_ k

  type t : k addressable
end

module Ok8 : T with kind_ k = bits8 = struct
  kind_ k = bits8

  type t : bits8 addressable
end
[%%expect{|
module type T = sig kind_ k type t : k addressable end
module Ok8 : sig kind_ k = bits8 type t : bits8 addressable end
|}]

(* The pending operator is also applied when the kind alias is expanded *)
kind_ kv = value

type t : kv addressable [@@warning "-183"]

let f (x : t) (y : t) = x == y
[%%expect{|
kind_ kv = value
type t
val f : t -> t -> bool = <fun>
|}]

kind_ k8 = bits8

type t8 : k8 addressable

let f (x : t8) = x
[%%expect{|
kind_ k8 = bits8
type t8 : bits8 addressable
val f : t8 -> t8 = <fun>
|}]

(* Functor application *)

module type Sk = sig
  type t : k
end

module type Ska = sig
  type t : k addressable
end

module F (_ : Sk) : Ska = struct
  type t : k addressable
end

module M0 = struct
  type t : k
end

module M1 = F (M0)
[%%expect{|
module type Sk = sig type t : k end
module type Ska = sig type t : k addressable end
module F : Sk -> Ska
module M0 : sig type t : k end
module M1 : sig type t = F(M0).t end
|}]

(* F cannot be applied twice: its result's kind [k addressable] is not a
   subkind of the argument's [k]... *)
module Bad = F (M1)
[%%expect{|
Line 1, characters 13-19:
1 | module Bad = F (M1)
                 ^^^^^^
Error: Modules do not match: sig type t = F(M0).t end is not included in
       Sk
     Type declarations do not match:
       type t = F(M0).t
     is not included in
       type t : k
     The kind of the first is k addressable
       because of the definition of t at line 6, characters 2-24.
     But the kind of the first must be a subkind of k
       because of the definition of t at line 2, characters 2-12.
|}]

(* A functor that wraps its argument's kind in [addressable] can be
   iterated *)
module Wrap (M : sig
    kind_ k

    type t : k
  end) =
struct
  kind_ k = M.k addressable

  type t : k
end

module M0 = struct
  kind_ k

  type t : k
end

module M1 = Wrap (M0)
module M2 = Wrap (M1)
[%%expect{|
module Wrap :
  functor (M : sig kind_ k type t : k end) ->
    sig kind_ k = M.k addressable type t : M.k addressable end
module M0 : sig kind_ k type t : k end
module M1 : sig kind_ k = M0.k addressable type t = Wrap(M0).t end
module M2 : sig kind_ k = M1.k addressable type t = Wrap(M1).t end
|}]

(* Record wrappers make their contents addressable. *)
type record8 = { x : int8# }
type unboxed_record8 = #{ x : int8# }
type ('a : bits8 addressable) addressable8
type ('a : bits8) plain8
[%%expect{|
type record8 = { x : int8#; }
type unboxed_record8 = #{ x : int8#; }
type ('a : bits8 addressable) addressable8
type ('a : bits8) plain8
|}]

type good_record8 = record8# addressable8
[%%expect{|
type good_record8 = record8# addressable8
|}]

type good_unboxed_record8 = unboxed_record8 addressable8
[%%expect{|
type good_unboxed_record8 = unboxed_record8 addressable8
|}]

type bad_record8 = record8# plain8
[%%expect{|
Line 1, characters 19-27:
1 | type bad_record8 = record8# plain8
                       ^^^^^^^^
Error: This type "record8#" should be an instance of type "('a : bits8)"
       The layout of record8# is bits8 addressable
         because it is an unboxed record.
       But the layout of record8# must be a sublayout of bits8
         because of the definition of plain8 at line 4, characters 0-24.
|}]

type bad_unboxed_record8 = unboxed_record8 plain8
[%%expect{|
Line 1, characters 27-42:
1 | type bad_unboxed_record8 = unboxed_record8 plain8
                               ^^^^^^^^^^^^^^^
Error: This type "unboxed_record8" should be an instance of type "('a : bits8)"
       The layout of unboxed_record8 is bits8 addressable
         because of the definition of unboxed_record8 at line 2, characters 0-37.
       But the layout of unboxed_record8 must be a sublayout of bits8
         because of the definition of plain8 at line 4, characters 0-24.
|}]

module Record8 : sig
  type t : bits8 addressable box
end = struct
  type t = { x : int8# }
end
[%%expect{|
module Record8 : sig type t : bits8 addressable box end
|}]

module Unboxed_record8 : sig
  type t : bits8 addressable
end = struct
  type t = #{ x : int8# }
end
[%%expect{|
module Unboxed_record8 : sig type t : bits8 addressable end
|}]

(* Making a field addressable does not change the type of its projection. *)
let project_record8 (r : record8) : int8# = r.x
let project_unboxed_record8 (r : unboxed_record8) : int8# = r.#x
[%%expect{|
val project_record8 : record8 -> int8# = <fun>
val project_unboxed_record8 : unboxed_record8 -> int8# = <fun>
|}]

type nested_record8 = #{ x : unboxed_record8 }
type good_nested_record8 = nested_record8 addressable8
[%%expect{|
type nested_record8 = #{ x : unboxed_record8; }
type good_nested_record8 = nested_record8 addressable8
|}]

(* An already-addressable field retains its kind. *)
type record64 = #{ x : int64_u }
type ('a : bits64) plain64
type good_record64 = record64 plain64
[%%expect{|
type record64 = #{ x : int64_u; }
type ('a : bits64) plain64
type good_record64 = record64 plain64
|}]

(* Kind checking must see through a record's type parameter. *)
type ('a : bits8) param_record8 = #{ x : 'a }
type good_param_record8 = int8# param_record8 addressable8
[%%expect{|
type ('a : bits8) param_record8 = #{ x : 'a; }
type good_param_record8 = int8# param_record8 addressable8
|}]

module Param_record8 : sig
  type ('a : bits8) t : bits8 addressable
end = struct
  type ('a : bits8) t = #{ x : 'a }
end
[%%expect{|
module Param_record8 : sig type ('a : bits8) t : bits8 addressable end
|}]

type unboxed_float_record = #{ x : float# }
type ('a : float64 addressable) addressable_float
type good_float_record = unboxed_float_record addressable_float
[%%expect{|
type unboxed_float_record = #{ x : float#; }
type ('a : float64 addressable) addressable_float
type good_float_record = unboxed_float_record addressable_float
|}]

module Float_record : sig
  type t : float64 addressable box
end = struct
  type t = { x : float# }
end
[%%expect{|
module Float_record : sig type t : float64 addressable box end
|}]

type unboxed_void_record = #{ x : unit# }
type ('a : void addressable) addressable_void
type good_void_record = unboxed_void_record addressable_void
[%%expect{|
type unboxed_void_record = #{ x : unit#; }
type ('a : void addressable) addressable_void
type good_void_record = unboxed_void_record addressable_void
|}]

type ('a : any) any_record = #{ x : 'a }
type good_any_record = int8# any_record addressable8
[%%expect{|
type ('a : any) any_record = #{ x : 'a; }
type good_any_record = int8# any_record addressable8
|}]

module Any_record : sig
  type ('a : any) t : any addressable
end = struct
  type ('a : any) t = #{ x : 'a }
end
[%%expect{|
module Any_record : sig type ('a : any) t : any addressable end
|}]

kind_ record_kind
type abstract_field : record_kind
type abstract_record = #{ x : abstract_field }
type ('a : record_kind addressable) addressable_abstract
type good_abstract_record = abstract_record addressable_abstract
[%%expect{|
kind_ record_kind
type abstract_field : record_kind
type abstract_record = #{ x : abstract_field; }
type ('a : record_kind addressable) addressable_abstract
type good_abstract_record = abstract_record addressable_abstract
|}]

(* Addressing an already-addressable field preserves its mode bounds. *)
type ('a : any) idempotent_record = #{ x : 'a }
type ('a : bits8 addressable mod portable) portable_addressable8
type ('a : bits8 addressable mod portable) idempotent_record_ok =
  'a idempotent_record portable_addressable8
[%%expect{|
type ('a : any) idempotent_record = #{ x : 'a; }
type ('a : bits8 addressable mod portable) portable_addressable8
type ('a : bits8 addressable mod portable) idempotent_record_ok =
    'a idempotent_record portable_addressable8
|}]
