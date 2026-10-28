(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 {
   expect;
 }
*)

(* NOTE: When adding tests to this file, also update
   [typing-layouts-products/basics_implicit_unboxed_records.ml] *)

open Stdlib_upstream_compatible

(**************************************************************************)
(* Basic examples: construction, functional updates, projection, matching *)

(* We can change the type of an unboxed record with a functional update. *)

type ('a : value & value) t = #{ x : 'a ; y : string }
let f : #(int * string) t -> #(string * int) t =
  fun (#{ x = #(i, s); y } as r) -> #{ r with x = #(s, i) }
[%%expect{|
type ('a : value & value) t = #{ x : 'a; y : string; }
val f : #(int * string) t -> #(string * int) t = <fun>
|}]

(* Patterns, as-patterns, partial patterns *)

type t = #{ i: int; j : int }
let add (#{ i; _} as r) = i + r.#j
[%%expect{|
type t = #{ i : int; j : int; }
val add : t -> int = <fun>
|}]

let bad_match (x : t) =
  match x with
  | _ -> .
[%%expect{|
Line 3, characters 4-5:
3 |   | _ -> .
        ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "#{ _ }"
|}]

(* Unboxed records are not subject to the mixed-block restriction *)

type t = #{ f : float# ; i : int }
[%%expect{|
type t = #{ f : float#; i : int; }
|}]

let mk_t () =
  #{ f = #3.14; i = 0 }
[%%expect{|
val mk_t : unit -> t = <fun>
|}]

let take_t #{ f; i } =
  #{ f; i }
[%%expect{|
val take_t : t -> t = <fun>
|}]

let combine_ts #{ f = _f1; i = i1 } #{ f = f2; i = _i2 } =
   #{ f = f2 ; i = i1 }
[%%expect{|
val combine_ts : t -> t -> t = <fun>
|}]

(* Top-level products *)

let unboxed_product = #{ f = #3.14; i = 0 }
[%%expect{|
val unboxed_product : t = #{f = <abstr>; i = 0}
|}]

;;
#{ f = #3.14; i = 0};;
[%%expect{|
- : t = #{f = <abstr>; i = 0}
|}]

type m_record = #{ i1 : int }
module M = struct
  let x = #{ i1 = 1 }
end
[%%expect{|
type m_record = #{ i1 : int; }
module M : sig val x : m_record end
|}]

type wrap_int = #{ i : int }
type wrap_wrap_int = #{ wi : wrap_int}
let w5 = #{ i = 5 }
let ww5 = #{ wi = #{ i = 5 }}
[%%expect{|
type wrap_int = #{ i : int; }
type wrap_wrap_int = #{ wi : wrap_int; }
val w5 : wrap_int = #{i = 5}
val ww5 : wrap_wrap_int = #{wi = #{i = 5}}
|}]

type t = #{ s : string }
let s = #{ s = "hi" }
[%%expect{|
type t = #{ s : string; }
val s : t = #{s = "hi"}
|}]

(* Fields whose declared type leaves the sort unknown (here, an [('a : any)]
   parameter) are printed according to the instantiated type. These used to
   print as [<unknown>]. *)

type ('a : any) with_any = #{ id : int; v : 'a }
let wa_int = #{ id = 1; v = 2 }
let wa_str = #{ id = 2; v = "s" }
let wa_float = #{ id = 3; v = #2.5 }
[%%expect{|
type ('a : any) with_any = #{ id : int; v : 'a; }
val wa_int : int with_any = #{id = 1; v = 2}
val wa_str : string with_any = #{id = 2; v = "s"}
val wa_float : float# with_any = #{id = 3; v = <abstr>}
|}]

type ('a : any) only_any = #{ v : 'a }
let oa = #{ v = 42 }
[%%expect{|
type ('a : any) only_any = #{ v : 'a; }
val oa : int only_any = #{v = 42}
|}]

;;
#{ i1 = 1 };;
[%%expect{|
- : m_record = #{i1 = 1}
|}]

(* Accessing inner products *)

type t = #{ is: #(int * int) }

let add t =
  let #(x, y) = t.#is in
  x + y
[%%expect{|
type t = #{ is : #(int * int); }
val add : t -> int = <fun>
|}]

(* An unboxed record is not an allocation, but a regular record is *)

type ('a, 'b) ab = { left : 'a ; right : 'b }
type ('a, 'b) ab_u = #{ left : 'a ; right : 'b }

let f_unboxed_record (local_ left) (local_ right) =
  let t = #{ left; right } in
  let #{ left = left'; _ } = t in
  left'
[%%expect{|
type ('a, 'b) ab = { left : 'a; right : 'b; }
type ('a, 'b) ab_u = #{ left : 'a; right : 'b; }
val f_unboxed_record : 'a @ local -> 'b @ local -> 'a @ local = <fun>
|}]

let f_boxed_record (local_ left) (local_ right) =
  let t = { left; right } in
  let { left = left'; _ } = t in
  left'
[%%expect{|
Line 4, characters 2-7:
4 |   left'
      ^^^^^
Error: This value is "local"
         because it is the field "left" of the record at line 3, characters 6-25
         which is "local"
         because it is allocated at line 2, characters 10-25 containing data
         which is "local" to the parent region
         because it is a record whose field "left" is the expression at line 2, characters 12-16
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

(* Fields can be declared mutable, which affects variance, but can't be
   mutated *)

type accepted = #{ mutable i : int }
[%%expect{|
type accepted = #{ mutable i : int; }
|}]

type atomic_bad = #{ mutable a : int [@atomic] }
[%%expect{|
Line 1, characters 21-46:
1 | type atomic_bad = #{ mutable a : int [@atomic] }
                         ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed record labels cannot be atomic
|}]

module M : sig
  type t = #{ mutable i : int }
end = struct
  type t = #{ i : int }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ i : int }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ i : int; } end
       is not included in
         sig type t = #{ mutable i : int; } end
       Type declarations do not match:
         type t = #{ i : int; }
       is not included in
         type t = #{ mutable i : int; }
       Fields do not match:
         "i : int;"
       is not the same as:
         "mutable i : int;"
       The second is mutable and the first is not.
|}]

(* Unboxed records with mutable fields are invariant *)
type +'a covariant_bad = #{ mutable x : 'a; y : int }
[%%expect{|
Line 1, characters 0-53:
1 | type +'a covariant_bad = #{ mutable x : 'a; y : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be covariant,
       but it is injective invariant.
|}]

type -'a contravariant_bad = #{ mutable x : 'a; y : int }
[%%expect{|
Line 1, characters 0-57:
1 | type -'a contravariant_bad = #{ mutable x : 'a; y : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this definition, expected parameter variances are not satisfied.
       The 1st type parameter was expected to be contravariant,
       but it is injective invariant.
|}]

type !'a injective_invariant = #{ mutable x : 'a; y : int }
[%%expect{|
type 'a injective_invariant = #{ mutable x : 'a; y : int; }
|}]

(* As for boxed records, only parameters in mutable fields are invariant *)
type +'a mut_isn't_param = #{ mutable x : int; y : 'a }
[%%expect{|
type 'a mut_isn't_param = #{ mutable x : int; y : 'a; }
|}]

type +'a param_isn't_mut = #{ x : 'a; y : int }
[%%expect{|
type 'a param_isn't_mut = #{ x : 'a; y : int; }
|}]

(* Unboxed "mutable" records can't be mutated in place yet, but for forward
   compatibility, reading their [mutable] fields still requires [shared]. *)
type doesn't_cross_contention = #{ mutable r : int ref; k : int }
let project (x : doesn't_cross_contention @ contended) =
  match x with #{ r = _; k } -> k
[%%expect{|
type doesn't_cross_contention = #{ mutable r : int ref; k : int; }
Line 3, characters 15-28:
3 |   match x with #{ r = _; k } -> k
                   ^^^^^^^^^^^^^
Error: This value is "contended"
       but is expected to be "shared" or "uncontended"
         because its mutable field "r" is being read.
|}]

let update (x : doesn't_cross_contention @ contended) = #{ x with k = 1 }
[%%expect{|
Line 1, characters 59-60:
1 | let update (x : doesn't_cross_contention @ contended) = #{ x with k = 1 }
                                                               ^
Error: This value is "contended"
       but is expected to be "shared" or "uncontended"
         because its mutable field "r" is being read.
|}]

(* [mutable] fields of unboxed records can't be mutated yet, but for forward
   compatibility, they still prevent generalization. *)
type 'a mut_poly = #{ mutable l : 'a list; k : int }
type ('a : value & value) container = { contents : 'a }
let not_generalized : _ mut_poly = #{ l = []; k = 0 }
let also_not_generalized : _ mut_poly container =
  { contents = #{ l = []; k = 0 } }
[%%expect{|
type 'a mut_poly = #{ mutable l : 'a list; k : int; }
type ('a : value & value) container = { contents : 'a; }
val not_generalized : '_weak1 mut_poly = #{l = []; k = 0}
val also_not_generalized : '_weak2 mut_poly container =
  {contents = #{l = []; k = 0}}
|}]

(* Single-field unboxed records are looked through when computing kinds, but a
   mutable label must still prevent crossing contention and visibility *)
type mut1 = #{ mutable x : int }
type 'a mut1_poly = #{ mutable x : 'a }
[%%expect{|
type mut1 = #{ mutable x : int; }
type 'a mut1_poly = #{ mutable x : 'a; }
|}]

let f (x : mut1) : (_ : immediate mod contended) = x
[%%expect{|
Line 1, characters 51-52:
1 | let f (x : mut1) : (_ : immediate mod contended) = x
                                                       ^
Error: The value "x" has type "mut1" but an expression was expected of type
         "('a : immediate)"
       The kind of mut1 is
           mutable_data non_pointer mod global dynamic unforkable yielding
         because of the definition of mut1 at line 1, characters 0-32.
       But the kind of mut1 must be a subkind of immediate
         because of the annotation on the wildcard _ at line 1, characters 19-48.
|}]

let f (x : int mut1_poly) : (_ : value mod contended) = x
[%%expect{|
Line 1, characters 56-57:
1 | let f (x : int mut1_poly) : (_ : value mod contended) = x
                                                            ^
Error: The value "x" has type "int mut1_poly"
       but an expression was expected of type "('a : value mod contended)"
       The kind of int mut1_poly is
           value mod global many stateless dynamic external_
         because of the definition of mut1_poly at line 2, characters 0-39.
       But the kind of int mut1_poly must be a subkind of value mod contended
         because of the annotation on the wildcard _ at line 1, characters 28-53.
|}, Principal{|
Line 1, characters 56-57:
1 | let f (x : int mut1_poly) : (_ : value mod contended) = x
                                                            ^
Error: The value "x" has type "int mut1_poly"
       but an expression was expected of type "('a : value mod contended)"
       The kind of int mut1_poly is
           value
             mod global many stateless dynamic external_
             with int @@ global many dynamic
         because of the definition of mut1_poly at line 2, characters 0-39.
       But the kind of int mut1_poly must be a subkind of value mod contended
         because of the annotation on the wildcard _ at line 1, characters 28-53.

       The first mode-crosses less than the second along:
         contention: mod uncontended ≰ mod contended
|}]

let f (x : int mut1_poly) : (_ : value mod portable) = x
[%%expect{|
val f : int mut1_poly -> int mut1_poly = <fun>
|}]

module M : sig type t : value mod contended end = struct
  type t = #{ mutable x : int }
end
[%%expect{|
Lines 1-3, characters 50-3:
1 | ..................................................struct
2 |   type t = #{ mutable x : int }
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ mutable x : int; } end
       is not included in
         sig type t : value mod contended end
       Type declarations do not match:
         type t = #{ mutable x : int; }
       is not included in
         type t : value mod contended
       The kind of the first is
           mutable_data non_pointer mod global dynamic unforkable yielding
         because it is the expansion of a type abbreviation.
       But the kind of the first must be a subkind of value mod contended
         because of the definition of t at line 1, characters 15-43.
|}]

module M : sig type t : value mod contended end = struct
  type t = int mut1_poly
end
[%%expect{|
Lines 1-3, characters 50-3:
1 | ..................................................struct
2 |   type t = int mut1_poly
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = int mut1_poly end
       is not included in
         sig type t : value mod contended end
       Type declarations do not match:
         type t = int mut1_poly
       is not included in
         type t : value mod contended
       The kind of the first is
           mutable_data non_pointer mod global dynamic unforkable yielding
         because it is the primitive type int.
       But the kind of the first must be a subkind of value mod contended
         because of the definition of t at line 1, characters 15-43.
|}]

type nested = #{ n : int mut1_poly }
let f (x : nested) : (_ : value mod contended) = x
[%%expect{|
type nested = #{ n : int mut1_poly; }
Line 2, characters 49-50:
2 | let f (x : nested) : (_ : value mod contended) = x
                                                     ^
Error: The value "x" has type "nested" but an expression was expected of type
         "('a : value mod contended)"
       The kind of nested is
           value mod global many stateless dynamic external_
         because of the definition of mut1_poly at line 2, characters 0-39.
       But the kind of nested must be a subkind of value mod contended
         because of the annotation on the wildcard _ at line 2, characters 21-46.
|}]

let f (x : #(int mut1_poly * int))
  : (_ : value mod contended & value mod contended) = x
[%%expect{|
Line 2, characters 54-55:
2 |   : (_ : value mod contended & value mod contended) = x
                                                          ^
Error: The value "x" has type "#(int mut1_poly * int)"
       but an expression was expected of type
         "('a : value mod contended & value mod contended)"
       The kind of #(int mut1_poly * int) is
           mutable_data non_pointer mod global unforkable yielding
           & mutable_data non_pointer mod global unforkable yielding
         because it is an unboxed tuple.
       But the kind of #(int mut1_poly * int) must be a subkind of
           value mod contended & value mod contended
         because of the annotation on the wildcard _ at line 2, characters 4-51.
|}, Principal{|
Line 2, characters 54-55:
2 |   : (_ : value mod contended & value mod contended) = x
                                                          ^
Error: The value "x" has type "#(int mut1_poly * int)"
       but an expression was expected of type
         "('a : value mod contended & value mod contended)"
       The kind of #(int mut1_poly * int) is
           immediate mod dynamic with int with int mut1_poly
           & immediate mod dynamic with int with int mut1_poly
         because it is an unboxed tuple.
       But the kind of #(int mut1_poly * int) must be a subkind of
           value mod contended & value mod contended
         because of the annotation on the wildcard _ at line 2, characters 4-51.
|}]

type ('a : any) mut1_any = #{ mutable x : 'a }
let f (x : float# mut1_any) : (_ : float64) = x
[%%expect{|
type ('a : any) mut1_any = #{ mutable x : 'a; }
val f : float# mut1_any -> float# mut1_any = <fun>
|}]

let f (x : float# mut1_any) : (_ : float64 mod contended) = x
[%%expect{|
Line 1, characters 60-61:
1 | let f (x : float# mut1_any) : (_ : float64 mod contended) = x
                                                                ^
Error: The value "x" has type "float# mut1_any"
       but an expression was expected of type "('a : float64 mod contended)"
       The layout of float# mut1_any is any
         because of the definition of mut1_any at line 1, characters 0-46.
       But the layout of float# mut1_any must be a sublayout of float64
         because of the annotation on the wildcard _ at line 1, characters 30-57.
|}]

(* A [contended] modality above a mutable label hides its mutability *)
type masked = #{ m : int mut1_poly @@ contended }
let f (x : masked) : (_ : value mod contended) = x
[%%expect{|
type masked = #{ m : int mut1_poly @@ contended; }
val f : masked -> masked = <fun>
|}]

module M : sig type t : value mod contended end = struct type t = masked end
[%%expect{|
module M : sig type t : value mod contended end
|}]

(* ...but one below it does not *)
type contended_int = #{ c : int @@ contended }
type unmasked = #{ mutable u : contended_int }
let f (x : unmasked) : (_ : value mod contended) = x
[%%expect{|
type contended_int = #{ c : int @@ contended; }
type unmasked = #{ mutable u : contended_int; }
Line 3, characters 51-52:
3 | let f (x : unmasked) : (_ : value mod contended) = x
                                                       ^
Error: The value "x" has type "unmasked" but an expression was expected of type
         "('a : value mod contended)"
       The kind of unmasked is
           mutable_data non_pointer mod global dynamic unforkable yielding
         because of the definition of unmasked at line 2, characters 0-46.
       But the kind of unmasked must be a subkind of value mod contended
         because of the annotation on the wildcard _ at line 3, characters 23-48.
|}]

module M : sig type t : value mod contended end = struct type t = unmasked end
[%%expect{|
Line 1, characters 50-78:
1 | module M : sig type t : value mod contended end = struct type t = unmasked end
                                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         sig type t = unmasked end
       is not included in
         sig type t : value mod contended end
       Type declarations do not match:
         type t = unmasked
       is not included in
         type t : value mod contended
       The kind of the first is
           mutable_data non_pointer mod global dynamic unforkable yielding
         because it is the primitive type int.
       But the kind of the first must be a subkind of value mod contended
         because of the definition of t at line 1, characters 15-43.
|}]

(* A [shared] modality above a mutable label hides its mutability only partly:
   [shared_masked] crosses contention from shared, but not from contended *)
type shared_masked = #{ sm : int mut1_poly @@ shared }
let use_uncontended (_ : 'a @ uncontended) = ()
let f (x : shared_masked @ shared) = use_uncontended x
[%%expect{|
type shared_masked = #{ sm : int mut1_poly @@ shared; }
val use_uncontended : 'a -> unit = <fun>
val f : shared_masked @ shared -> unit = <fun>
|}]

let f (x : shared_masked @ contended) = use_uncontended x
[%%expect{|
Line 1, characters 56-57:
1 | let f (x : shared_masked @ contended) = use_uncontended x
                                                            ^
Error: This value is "corrupted" because it crosses with something
         which is "contended".
       However, the highlighted expression is expected to be "uncontended".
|}]

module M : sig type t : value mod shared end = struct
  type t = shared_masked
end
[%%expect{|
module M : sig type t : value mod shared end
|}]

module M : sig type t : value mod contended end = struct
  type t = shared_masked
end
[%%expect{|
Lines 1-3, characters 50-3:
1 | ..................................................struct
2 |   type t = shared_masked
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = shared_masked end
       is not included in
         sig type t : value mod contended end
       Type declarations do not match:
         type t = shared_masked
       is not included in
         type t : value mod contended
       The kind of the first is
           mutable_data non_pointer
             mod global shared dynamic unforkable yielding
         because it is the primitive type int.
       But the kind of the first must be a subkind of value mod contended
         because of the definition of t at line 1, characters 15-43.
|}]

(* Only the outermost mutable label matters: [a] isn't below the [contended]
   modality, so it still stops [two_mutable] from crossing contention *)
type two_mutable = #{ mutable a : int mut1_poly @@ contended }
module M : sig type t : value mod contended end = struct
  type t = two_mutable
end
[%%expect{|
type two_mutable = #{ mutable a : int mut1_poly @@ contended; }
Lines 2-4, characters 50-3:
2 | ..................................................struct
3 |   type t = two_mutable
4 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = two_mutable end
       is not included in
         sig type t : value mod contended end
       Type declarations do not match:
         type t = two_mutable
       is not included in
         type t : value mod contended
       The kind of the first is
           mutable_data non_pointer mod global dynamic unforkable yielding
         because it is the primitive type int.
       But the kind of the first must be a subkind of value mod contended
         because of the definition of t at line 2, characters 15-43.
|}]

(* Signature inclusion checks the kind of a declared single-label unboxed record
   directly, rather than through an abbreviation. A modality on a mutable label
   only applies to its contents, so it does not hide the label's
   mutability... *)
module M : sig type t : value mod contended end = struct
  type t = #{ mutable x : int @@ contended }
end
[%%expect{|
Lines 1-3, characters 50-3:
1 | ..................................................struct
2 |   type t = #{ mutable x : int @@ contended }
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ mutable x : int @@ contended; } end
       is not included in
         sig type t : value mod contended end
       Type declarations do not match:
         type t = #{ mutable x : int @@ contended; }
       is not included in
         type t : value mod contended
       The kind of the first is
           mutable_data non_pointer mod global dynamic unforkable yielding
         because it is the expansion of a type abbreviation.
       But the kind of the first must be a subkind of value mod contended
         because of the definition of t at line 1, characters 15-43.
|}]

(* ...nor does a modality inside the field... *)
module M : sig type t : value mod contended end = struct
  type t = #{ mutable u : contended_int }
end
[%%expect{|
Lines 1-3, characters 50-3:
1 | ..................................................struct
2 |   type t = #{ mutable u : contended_int }
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ mutable u : contended_int; } end
       is not included in
         sig type t : value mod contended end
       Type declarations do not match:
         type t = #{ mutable u : contended_int; }
       is not included in
         type t : value mod contended
       The kind of the first is
           mutable_data non_pointer mod global dynamic unforkable yielding
         because it is the expansion of a type abbreviation.
       But the kind of the first must be a subkind of value mod contended
         because of the definition of t at line 1, characters 15-43.
|}]

(* ...but a modality above an inner mutable label does *)
module M : sig type t : value mod contended end = struct
  type t = #{ m : int mut1_poly @@ contended }
end
[%%expect{|
module M : sig type t : value mod contended end
|}]

(* A mutable label restricts visibility as well as contention *)
module M : sig type t : value mod immutable end = struct
  type t = #{ mutable x : int }
end
[%%expect{|
Lines 1-3, characters 50-3:
1 | ..................................................struct
2 |   type t = #{ mutable x : int }
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ mutable x : int; } end
       is not included in
         sig type t : value mod immutable end
       Type declarations do not match:
         type t = #{ mutable x : int; }
       is not included in
         type t : value mod immutable
       The kind of the first is
           mutable_data non_pointer mod global dynamic unforkable yielding
         because it is the expansion of a type abbreviation.
       But the kind of the first must be a subkind of value mod immutable
         because of the definition of t at line 1, characters 15-43.
|}]

(* ...but nothing else *)
module M : sig type t : value mod global portable many end = struct
  type t = #{ mutable x : int }
end
[%%expect{|
module M : sig type t : value mod global many portable end
|}]

(* Looking through still refines the layout *)
let f (x : int mut1_poly) : (_ : value non_pointer) = x
[%%expect{|
val f : int mut1_poly -> int mut1_poly = <fun>
|}]

type ('a : value non_pointer) require_non_pointer
type t = int mut1_poly require_non_pointer
[%%expect{|
type ('a : value non_pointer) require_non_pointer
type t = int mut1_poly require_non_pointer
|}]

(* Including for the kind of a type abbreviation *)
module M : sig type t : value non_pointer end = struct
  type t = int mut1_poly
end
[%%expect{|
module M : sig type t : value non_pointer end
|}]

(*********************************)
(* Parameterized unboxed records *)

(* Checks of constrain_type_jkind *)

type 'a r = #{ i: 'a }
type int_r : immediate = int r
[%%expect{|
type 'a r = #{ i : 'a; }
type int_r = int r
|}]

type ('a : float64) t = #{ i: 'a }
type floatu_t : float64 = float# t
[%%expect{|
type ('a : float64) t = #{ i : 'a; }
type floatu_t = float# t
|}]

type 'a t = #{ i : 'a ; j : 'a }
type int_t : immediate & immediate = int t
[%%expect{|
type 'a t = #{ i : 'a; j : 'a; }
type int_t = int t
|}]

type ('a : float64) t = #{ i : 'a ; j : 'a }
type floatu_t : float64 & float64 = float# t
[%%expect{|
type ('a : float64) t = #{ i : 'a; j : 'a; }
type floatu_t = float# t
|}]

type 'a t = 'a list
type s = #{ lbl : s t }
[%%expect{|
type 'a t = 'a list
type s = #{ lbl : s t; }
|}]

type ('a : float64) t = #{ x : string; y : 'a }
[%%expect{|
type ('a : float64) t = #{ x : string; y : 'a; }
|}];;

type ('a : float64, 'b : immediate) t = #{ x : string; y : 'a; z : 'b }
[%%expect{|
type ('a : float64, 'b : immediate) t = #{ x : string; y : 'a; z : 'b; }
|}];;

type ('a : value & float64 & value) t1
type ('a : value) t2
[%%expect{|
type ('a : value & float64 & value) t1
type 'a t2
|}]

type s = r t1
and r = #{ x : int; y : float#; z : s t2 }
[%%expect{|
type s = r t1
and r = #{ x : int; y : float#; z : s t2; }
|}]

type s = r_bad t1
and r_bad = #{ y : float#; z : s t2 }
[%%expect{|
Line 2, characters 0-37:
2 | and r_bad = #{ y : float#; z : s t2 }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of r_bad is any & any
         because it is an unboxed record.
       But the layout of r_bad must be a sublayout of value & float64 & value
         because of the definition of t1 at line 1, characters 0-38.
|}]

(* CR layouts-scannable: The annotation on ['a] is temporarily necessary while
   inference for mutual recursive types + scannable axes is broken. See tests
   in [typing-layouts-scannable/mutual_recursion.ml] for more examples. *)
type 'a t = #{ a : 'a ; a' : 'a } constraint ('a : immediate & float64) = r
and r = #{ i : int ; f : float# }
[%%expect{|
type 'a t = #{ a : 'a; a' : 'a; } constraint 'a = r
and r = #{ i : int; f : float#; }
|}]

type 'a t = #{ a : 'a ; a' : 'a } constraint 'a = r
and r = #{ i : int }
[%%expect{|
type 'a t = #{ a : 'a; a' : 'a; } constraint 'a = r
and r = #{ i : int; }
|}]

(*******************)
(* Types with [as] *)

let f (x : < m: 'a. ([< `Foo of int & float] as 'a) -> unit>)
         : < m: 'a. ([< `Foo of int & float] as 'a) -> unit> = x;;

type t = #{ x : 'a. ([< `Foo of int & float ] as 'a) -> unit };;
let f t = #{ x = t.#x };;
[%%expect{|
val f :
  < m : 'a. ([< `Foo of int & float ] as 'a) -> unit > ->
  < m : 'b. ([< `Foo of int & float ] as 'b) -> unit > = <fun>
type t = #{ x : 'a. ([< `Foo of int & float ] as 'a) -> unit; }
val f : t -> t = <fun>
|}]

module Bad : sig
  type t = #{ i : int ; a: (<x:'a> as 'a) }
end = struct
  type t = #{ i : int ; a: (<x:'a * 'a> as 'a) }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ i : int ; a: (<x:'a * 'a> as 'a) }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ i : int; a : < x : 'a * 'a > as 'a; } end
       is not included in
         sig type t = #{ i : int; a : < x : 'a > as 'a; } end
       Type declarations do not match:
         type t = #{ i : int; a : < x : 'a * 'a > as 'a; }
       is not included in
         type t = #{ i : int; a : < x : 'a > as 'a; }
       Fields do not match:
         "a : < x : 'a * 'a > as 'a;"
       is not the same as:
         "a : < x : 'a > as 'a;"
       The type "< x : 'a * 'a > as 'a" is not equal to the type
         "< x : 'b > as 'b"
       The method "x" has type "< x : 'c > * < x : 'c > as 'c",
       but the expected method type was "< x : 'b > as 'b"
|}]

(**********************)
(* Signature checking *)

(* Must expose non-value kind *)
module M : sig
  type t
end = struct
  type t = #{ s: string; r: string }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ s: string; r: string }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ s : string; r : string; } end
       is not included in
         sig type t end
       Type declarations do not match:
         type t = #{ s : string; r : string; }
       is not included in
         type t
       The layout of the first is value non_float & value non_float
         because of the definition of t at line 4, characters 2-36.
       But the layout of the first must be a value layout
         because of the definition of t at line 2, characters 2-8.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

module M : sig
  type t = #{ f : float# ; s : string }
end = struct
  type t = #{ f : float# ; s : string }
end
[%%expect{|
module M : sig type t = #{ f : float#; s : string; } end
|}]

module M2 : sig
  type t : float64 & value
end = struct
  include M
end
[%%expect{|
module M2 : sig type t : float64 & value end
|}]

module M : sig
  type t : float64 & value
end = struct
  type t = #{ i : float# ; s : string }
end
[%%expect{|
module M : sig type t : float64 & value end
|}]

module M : sig
  type t : float64 & value
end = struct
  type t = #{ i : float# ; s : string }
end
[%%expect{|
module M : sig type t : float64 & value end
|}]

module M : sig
  type t
end = struct
  type t = #{ s : string }
end
[%%expect{|
module M : sig type t end
|}]

(*************************************)
(* Types that mode cross externality *)

type ('a : value mod external_) t = #{ x : float#; y : 'a }
type ('a : immediate) t = #{ x : float#; y : 'a }
[%%expect {|
type ('a : value mod external_) t = #{ x : float#; y : 'a; }
type ('a : immediate) t = #{ x : float#; y : 'a; }
|}]

type u : value mod external_
type t = #{ x : float#; y : u }
[%%expect {|
type u : value mod external_
type t = #{ x : float#; y : u; }
|}]

type u : immediate
type t = #{ x : float#; y : u }
[%%expect {|
type u : immediate
type t = #{ x : float#; y : u; }
|}]

(********************)
(* Recursive groups *)

type ('a : float64) t_float64_id = 'a
type ('a : immediate) t_immediate_id = 'a
[%%expect{|
type ('a : float64) t_float64_id = 'a
type ('a : immediate) t_immediate_id = 'a
|}];;

type 'a t_float = 'a t_float64_id
and 'a t_imm = 'a t_immediate_id
and ('a, 'b, 'ptr) t =
  #{ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
[%%expect{|
Line 4, characters 28-38:
4 |   #{ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
                                ^^^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a is float64
           because of the definition of t_float64_id at line 1, characters 0-37.
         But the layout of 'a must be a value layout
           because it instantiates an unannotated type parameter of t,
           chosen to have layout value.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}];;

type 'a t_float = 'a t_float64_id
and 'a t_imm = 'a t_immediate_id
and ('a : float64, 'b : immediate, 'ptr) t =
  #{ptr : 'ptr; x : 'a; y : 'a t_float; z : 'b; w : 'b t_imm}
[%%expect{|
type ('a : float64) t_float = 'a t_float64_id
and ('a : immediate) t_imm = 'a t_immediate_id
and ('a : float64, 'b : immediate, 'ptr) t = #{
  ptr : 'ptr;
  x : 'a;
  y : 'a t_float;
  z : 'b;
  w : 'b t_imm;
}
|}];;

(* We don't yet have syntax for setting an unboxed record field.
   However, the below, using a boxed set field, will never work. *)

type r = #{ i : int }
let f = #{ i = 1 }
[%%expect{|
type r = #{ i : int; }
val f : r = #{i = 1}
|}]

let () = f.i <- 2
[%%expect{|
Line 1, characters 9-10:
1 | let () = f.i <- 2
             ^
Error: This expression has type "r",
       which is an unboxed record rather than a boxed one.
|}]

(********************************)
(* Private unboxed record types *)

module M : sig
  type t = private #{ x : int; y : bool }
end = struct
  type t = #{ x : int; y : bool }
end;;
[%%expect{|
module M : sig type t = private #{ x : int; y : bool; } end
|}]

module M : sig
  type t = #{ x : int; y : bool }
end = struct
  type t = private #{ x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private #{ x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private #{ x : int; y : bool; } end
       is not included in
         sig type t = #{ x : int; y : bool; } end
       Type declarations do not match:
         type t = private #{ x : int; y : bool; }
       is not included in
         type t = #{ x : int; y : bool; }
       A private unboxed record constructor would be revealed.
|}];;


module M : sig
  type t = #{ x : int }
end = struct
  type t = private #{ x : int; y : bool }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private #{ x : int; y : bool }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private #{ x : int; y : bool; } end
       is not included in
         sig type t = #{ x : int; } end
       Type declarations do not match:
         type t = private #{ x : int; y : bool; }
       is not included in
         type t = #{ x : int; }
       A private unboxed record constructor would be revealed.
|}];;

module M : sig
  type t = #{ x : int; y : bool }
end = struct
  type t = private #{ x : int }
end;;
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = private #{ x : int }
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = private #{ x : int; } end
       is not included in
         sig type t = #{ x : int; y : bool; } end
       Type declarations do not match:
         type t = private #{ x : int; }
       is not included in
         type t = #{ x : int; y : bool; }
       A private unboxed record constructor would be revealed.
|}];;

(*****************************************************)
(* Special-cased errors for boxed/unboxed mismatches *)

type t_u = #{ u : float# }
type t = { b : float# }
[%%expect{|
type t_u = #{ u : float#; }
type t = { b : float#; }
|}]

let f () : t_u = { b = #1.0 }
[%%expect{|
Line 1, characters 17-29:
1 | let f () : t_u = { b = #1.0 }
                     ^^^^^^^^^^^^
Error: This boxed record expression should be unboxed instead,
       the expected type is "t_u"
|}]

let f () : t = #{ u = #2.0 }
[%%expect{|
Line 1, characters 15-28:
1 | let f () : t = #{ u = #2.0 }
                   ^^^^^^^^^^^^^
Error: This unboxed record expression should be boxed instead,
       the expected type is "t"
|}]

let ({ b } : t_u) = assert false
[%%expect{|
Line 1, characters 5-10:
1 | let ({ b } : t_u) = assert false
         ^^^^^
Error: This boxed record pattern should be unboxed instead,
       the expected type is "t_u"
|}]

let (#{ u } : t) = assert false
[%%expect{|
Line 1, characters 5-11:
1 | let (#{ u } : t) = assert false
         ^^^^^^
Error: This unboxed record pattern should be boxed instead,
       the expected type is "t"
|}]

let bad_get (t_u : t_u) = t_u.u
[%%expect{|
Line 1, characters 26-29:
1 | let bad_get (t_u : t_u) = t_u.u
                              ^^^
Error: This expression has type "t_u",
       which is an unboxed record rather than a boxed one.
|}]

let bad_get (t : t) = t.#b
[%%expect{|
Line 1, characters 22-23:
1 | let bad_get (t : t) = t.#b
                          ^
Error: This expression has type "t",
       which is a boxed record rather than an unboxed one.
|}]

let _ = { u = #5.0 }
[%%expect{|
Line 1, characters 10-11:
1 | let _ = { u = #5.0 }
              ^
Error: Unbound record field "u"
Hint: There is an unboxed record field with this name.
|}]

let bad_get t_u = t_u.u
[%%expect{|
Line 1, characters 22-23:
1 | let bad_get t_u = t_u.u
                          ^
Error: Unbound record field "u"
Hint: There is an unboxed record field with this name.
To project an unboxed record field, use ".#u" instead of ".u".
|}]

(*****************************************************************************)
(* Initial expressions for functionally updated records are always evaluated *)

type t = #{ x : string }

let [@warning "-23"] update_t t =
  let updated = ref false in
  let _ = #{ (updated := true; t) with x = "" } in
  assert !updated

let _ = update_t #{ x = "x" }
[%%expect{|
type t = #{ x : string; }
val update_t : t -> unit = <fun>
- : unit = ()
|}]

type t = #{ x : string ; y : float# ; z : unit}

let [@warning "-23"] update_t t =
  let counter = ref 0 in
  let _ = #{ (incr counter; t) with x = ""; y = #0.0 ; z = ()} in
  assert (!counter = 1);
  let _ = #{ (incr counter; t) with y = #0.0 } in
  assert (!counter = 2)

let _ = update_t #{ x = "x" ; y = #1.0 ; z = ()}
[%%expect{|
type t = #{ x : string; y : float#; z : unit; }
val update_t : t -> unit = <fun>
- : unit = ()
|}]

(************************************************************)
(* Basic tests for construction/projection representability *)

type ('a : any) t = #{ x : int; y : 'a }
[%%expect{|
type ('a : any) t = #{ x : int; y : 'a; }
|}]

(* CR layouts v7.2: once we allow record declarations with unknown kind (right
   now, ['a] in the decl above is defaulted to value), then this should give an
   error saying that records being projected from must be representable.

   Update: The record declaration is now allowed, but it's actually difficult to
   get that exact error in this situation, since one variable or another needs
   to be given an unrepresentable type. *)
let f : ('a : any). 'a t -> 'a = fun t -> t.#y
[%%expect{|
Line 1, characters 33-46:
1 | let f : ('a : any). 'a t -> 'a = fun t -> t.#y
                                     ^^^^^^^^^^^^^
Error: This definition has type "'b t -> 'b" which is less general than
         "('a : any). 'a t -> 'a"
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because we must know concretely how to pass a function argument.
|}]

(* CR layouts v7.2: once we allow record declarations with unknown kind
   (right now, ['a] in the decl above is defaulted to value), then this should
   give an error saying that records used in functional updates must be
   representable.

   Update: Similarly, the type is now allowed but the function argument is what
   gets flagged as an error.
*)
let f : ('a : any). 'a -> 'a t = fun a -> #{ x = 1; y = a }
[%%expect{|
Line 1, characters 33-59:
1 | let f : ('a : any). 'a -> 'a t = fun a -> #{ x = 1; y = a }
                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This definition has type "'b -> 'b t" which is less general than
         "('a : any). 'a -> 'a t"
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because we must know concretely how to pass a function argument.
|}]


(************************************************************)

(* This is a regression test. Previously, we hit a fatal error because the [any]
   annotation obscured the fact that the unboxed record is a product, breaking
   an invariant assumed by [Ctype.constrain_type_jkind].

   [a] was necessary to trigger the error because it called
   [check_representable] on [b], constraining its kind to be a sort variable. *)
type a = B of b
and b : any = #{ i : int ; j : int }
[%%expect{|
type a = B of b
and b = #{ i : int; j : int; }
|}]
type a = B of b_portable
and b_portable : any mod portable = #{ i : int ; j : int }
[%%expect{|
type a = B of b_portable
and b_portable = #{ i : int; j : int; }
|}]
type a = B of b
and b : any & any & any = #{ i : int ; j : int }
[%%expect{|
Line 2, characters 0-48:
2 | and b : any & any & any = #{ i : int ; j : int }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "b" is value non_pointer & value non_pointer
         because it is an unboxed record.
       But the layout of type "b" must be a sublayout of any & any & any
         because of the annotation on the declaration of the type b.
       Note: The layout of immediate is value non_pointer.
|}]

type q : any mod portable = #{ x : int -> int; y : int -> q }

[%%expect{|
Line 1, characters 0-61:
1 | type q : any mod portable = #{ x : int -> int; y : int -> q }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type definition does not satisfy its kind annotation
         any mod portable & any mod portable,
       because functions are not mod portable.
|}]
(* CR layouts v2.8: That error message is incomprehensible without
   with-bounds. Internal ticket 5096. *)
