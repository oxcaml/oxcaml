(* TEST
 include stdlib_stable;
 flags = "-extension layouts_alpha";
 expect;
*)

open Stdlib_stable

type ('a : any) t = { fst : 'a; mutable snd : 'a }
[%%expect{|
type ('a : any) t = { fst : 'a; mutable snd : 'a; }
|}]

let fst t = t.fst
[%%expect{|
val fst : 'a t -> 'a = <fun>
|}]

let fst (type a : any) (t : a t) = t.fst
[%%expect{|
Line 1, characters 35-40:
1 | let fst (type a : any) (t : a t) = t.fst
                                       ^^^^^
Error: Fields being projected must be representable.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because it's the type of a field being projected.
|}]

let fst (t : int t) = t.fst
[%%expect{|
val fst : int t -> int = <fun>
|}]

let fst (type a : value) (t : a t) = t.fst
[%%expect{|
val fst : 'a t -> 'a = <fun>
|}]

let fst (t : int64# t) = t.fst
[%%expect{|
val fst : int64# t -> int64# = <fun>
|}]

let fst (t : int64# t) =
  match t with { fst; _ } -> fst
[%%expect{|
val fst : int64# t -> int64# = <fun>
|}]

let fst (type a : bits64) (t : a t) = t.fst
[%%expect{|
val fst : ('a : bits64). 'a t -> 'a = <fun>
|}]

let fst (type a : bits64) (t : a t) =
  match t with { fst; _ } -> fst
[%%expect{|
val fst : ('a : bits64). 'a t -> 'a = <fun>
|}]

let set_snd t a = t.snd <- a
[%%expect {|
val set_snd : 'a t -> 'a -> unit = <fun>
|}]

let set_snd t (a : int64#) = t.snd <- a
[%%expect {|
val set_snd : int64# t -> int64# -> unit = <fun>
|}]

let set_snd (type a : bits64) (t : a t) a = t.snd <- a
[%%expect {|
val set_snd : ('a : bits64). 'a t -> 'a -> unit = <fun>
|}]

let make fst snd = { fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let make (fst : int) snd = { fst; snd }
[%%expect{|
val make : int -> int -> int t = <fun>
|}]

let make (fst : int) (snd : int) = { fst; snd }
[%%expect{|
val make : int -> int -> int t = <fun>
|}]

let make fst snd : int t = { fst; snd }
[%%expect{|
val make : int -> int -> int t = <fun>
|}]

let make (type a : value) (fst : a) (snd : a) = { fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let make (type a : value) fst snd : a t = { fst; snd }
[%%expect{|
val make : 'a -> 'a -> 'a t = <fun>
|}]

let make (fst : int64#) snd = { fst; snd }
[%%expect{|
val make : int64# -> int64# -> int64# t = <fun>
|}]

let make (fst : int64#) (snd : int64#) = { fst; snd }
[%%expect{|
val make : int64# -> int64# -> int64# t = <fun>
|}]

let make (type a : bits64) (fst : a) (snd : a) = { fst; snd }
[%%expect{|
val make : ('a : bits64). 'a -> 'a -> 'a t = <fun>
|}]

external box_int64 : int64# -> int64 = "%box_int64"
[%%expect {|
external box_int64 : int64# -> int64 = "%box_int64"
|}]

(* Test that typing and genprintval work when the actual type has kind value *)
let test_block_with_values = { fst = 1; snd = 2 } |> Sys.opaque_identity
[%%expect {|
val test_block_with_values : int t = {fst = 1; snd = 2}
|}]

let make_test_block () = { fst = #1L; snd = #2L } |> Sys.opaque_identity
[%%expect {|
val make_test_block : unit -> int64# t = <fun>
|}]

let test_block = make_test_block ()
[%%expect {|
val test_block : int64# t = {fst = <abstr>; snd = <abstr>}
|}]

let test_direct =
  (* Check that projecting from a block with an [any] accounts for the layouts
     of all previous fields in the block *)
  test_block.snd |> box_int64
[%%expect{|
val test_direct : int64 = 2L
|}]

let test_via_index =
  (* Check that the same works when accessing through a block index *)
  let idx = ((.snd) : (('a : bits64) t, 'a) idx_mut) |> Sys.opaque_identity in
  Idx_mut.get test_block idx |> box_int64
[%%expect {|
val test_via_index : int64 = 2L
|}]

let test_set_direct =
  let t = make_test_block () in
  t.snd <- #42L;
  (t |> Sys.opaque_identity).snd |> box_int64
[%%expect {|
val test_set_direct : int64 = 42L
|}]

let test_set_via_index =
  let t = make_test_block () in
  let idx = ((.snd) : (('a : bits64) t, 'a) idx_mut) |> Sys.opaque_identity in
  Idx_mut.set t idx #42L;
  Idx_mut.get t idx |> box_int64
[%%expect {|
val test_set_via_index : int64 = 42L
|}]

let test_unboxed_pair_block : #(int64# * int64#) t =
  { fst = #(#1L, #2L); snd = #(#3L, #4L) }
|> Sys.opaque_identity
[%%expect {|
val test_unboxed_pair_block : #(int64# * int64#) t =
  {fst = #(<abstr>, <abstr>); snd = #(<abstr>, <abstr>)}
|}]

let test_unboxed_pair_direct =
  let #(_fst, snd) = test_unboxed_pair_block.snd in snd |> box_int64
[%%expect{|
val test_unboxed_pair_direct : int64 = 4L
|}]

let test_nested_block : int64# t# t =
  { fst = #{ fst = #1L; snd = #2L }; snd = #{ fst = #3L; snd = #4L } }
|> Sys.opaque_identity
[%%expect {|
val test_nested_block : int64# t# t =
  {fst = #{fst = <unknown>; snd = <unknown>};
   snd = #{fst = <unknown>; snd = <unknown>}}
|}]

let test_nested_direct =
  test_nested_block.snd.#snd |> box_int64
[%%expect{|
val test_nested_direct : int64 = 4L
|}]

let test_nested_via_index =
  let idx =
    ((.snd.#snd) : (int64# t# t, int64#) idx_mut)
    |> Sys.opaque_identity
  in
  Idx_mut.get test_nested_block idx |> box_int64
[%%expect{|
val test_nested_via_index : int64 = 4L
|}]

(* Test that a record with [any] is never a flat float block *)

module Secret_float : sig
  type 'a pair := 'a t
  type t
  val to_float : t -> float
  val mk_pair : unit -> t pair
end = struct
  type t = float
  let to_float x = x
  let mk_pair () = { fst = 42.0; snd = 99.0 }
end
[%%expect {|
module Secret_float :
  sig type t val to_float : t -> float val mk_pair : unit -> t/1 t/2 end
|}]

let test_secret_float =
  let t = Secret_float.mk_pair () in
  let f =
    (* If the record is flat, this will segfault *)
    Secret_float.to_float t.fst
  in
  f
[%%expect {|
val test_secret_float : float = 42.
|}]

(* [@@unboxed] matches *)

type a : any
type t = { t : a } [@@unboxed]
let f { t } = t
[%%expect{|
type a : any
type t = { t : a; } [@@unboxed]
Line 3, characters 6-11:
3 | let f { t } = t
          ^^^^^
Error: This expression has type "('a : '_representable_layout_1)"
       but an expression was expected of type "t"
       The layout of t is any
         because of the definition of a at line 1, characters 0-12.
       But the layout of t must be representable
         because it's the record type used in a projection.
|}]

type ('a : any) t = { t : 'a } [@@unboxed]
let f { t } = t
[%%expect{|
type ('a : any) t = { t : 'a; } [@@unboxed]
val f : 'a t -> 'a = <fun>
|}]

(* Sort variables are not initialized well *)

(* Projecting a label other than the [any] *)

type ('a : any) t = { i : int ; a : 'a }
let foo t = (.a)
[%%expect{|
type ('a : any) t = { i : int; a : 'a; }
Line 2, characters 14-15:
2 | let foo t = (.a)
                  ^
Error: Cannot access record with unrepresentable field.
       The record has type 'a t, whose field a is not representable.
|}]

(* Matching *)

type ('a : any) t = { a : 'a }
let foo { a } = ()
[%%expect{|
type ('a : any) t = { a : 'a; }
Line 2, characters 8-13:
2 | let foo { a } = ()
            ^^^^^
Error: Cannot access record with unrepresentable field.
       The record has type 'a t, whose field a is not representable.
|}]

(* Block indices *)

(* CR-soon rtjoa: This should typecheck and default to value *)
type ('a : any) r = { t : 'a }
let f = (.t)
[%%expect{|
type ('a : any) r = { t : 'a; }
Line 2, characters 10-11:
2 | let f = (.t)
              ^
Error: Cannot access record with unrepresentable field.
       The record has type 'a r, whose field t is not representable.
|}]

(* Abstract kinds *)

kind_ k
type a : k
type t = { a : a }
let f { a } = ()
[%%expect{|
kind_ k
type a : k
type t = { a : a; }
Line 4, characters 6-11:
4 | let f { a } = ()
          ^^^^^
Error: Cannot access record with unrepresentable field.
       The record has type t, whose field a is not representable.
|}]

(* CR-soon rtjoa: The below two programs should work *)
module Substitution_mismatch : sig
  type a : any
  type t = { a : a }
end with type a := float# = struct
  type t = { a : float# }
end
[%%expect{|
Lines 4-6, characters 28-3:
4 | ............................struct
5 |   type t = { a : float# }
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { a : float#; } end
       is not included in
         sig type t = { a : float#; } end
       Type declarations do not match:
         type t = { a : float#; }
       is not included in
         type t = { a : float#; }
       Their internal representations differ:
       the first declaration has a fixed representation while the other varies.
|}]

module Substitution_mismatch_with_abstract_kinds : sig
  kind_ k
  type a : k
  type t = { a : a }
end with kind_ k := float64 and type a := float# = struct
  type t = { a : float# }
end
let f c { M.a = a } = if c then a else #0.
[%%expect{|
Lines 5-7, characters 51-3:
5 | ...................................................struct
6 |   type t = { a : float# }
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = { a : float#; } end
       is not included in
         sig type t = { a : float#; } end
       Type declarations do not match:
         type t = { a : float#; }
       is not included in
         type t = { a : float#; }
       Their internal representations differ:
       the first declaration has a fixed representation while the other varies.
|}]

module M : sig
  kind_ k
  type t : k
  type a = A of t
end with kind_ k := void and type t := unit# = struct
  type a = A of unit#
end
[%%expect{|
Lines 5-7, characters 47-3:
5 | ...............................................struct
6 |   type a = A of unit#
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type a = A of unit# end
       is not included in
         sig type a = A of unit# end
       Type declarations do not match:
         type a = A of unit#
       is not included in
         type a = A of unit#
       Constructors do not match:
         "A of unit#"
       is not the same as:
         "A of unit#"
       The first has a fixed representation and the second doesn't.
       Hint: Is there a type that has a representable layout in the first
         but has layout any in the second?
|}]

(* [@@flatten_floats] *)

type ('a : any) t = { a : 'a }
[@@flatten_floats]
[%%expect{|
Lines 1-2, characters 0-18:
1 | type ('a : any) t = { a : 'a }
2 | [@@flatten_floats]
Error: The "[@@flatten_floats]" attribute is only allowed on records with one or more
       non-atomic "float" fields, one or more "float#" fields, and all other fields
       void.
|}]

type ('a : any) t = { a : 'a; f : float }
[@@flatten_floats]
[%%expect{|
Lines 1-2, characters 0-18:
1 | type ('a : any) t = { a : 'a; f : float }
2 | [@@flatten_floats]
Error: The "[@@flatten_floats]" attribute is only allowed on records with one or more
       non-atomic "float" fields, one or more "float#" fields, and all other fields
       void.
|}]

type ('a : any) t = { a : 'a; f : float# }
[@@flatten_floats]
[%%expect{|
Lines 1-2, characters 0-18:
1 | type ('a : any) t = { a : 'a; f : float# }
2 | [@@flatten_floats]
Error: The "[@@flatten_floats]" attribute is only allowed on records with one or more
       non-atomic "float" fields, one or more "float#" fields, and all other fields
       void.
|}]
