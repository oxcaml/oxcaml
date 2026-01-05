(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type ('a : immutable_data) t
type 'a u = 'a t
[%%expect {|
type ('a : immutable_data) t
type ('a : immutable_data) u = 'a t
|}]

type ('a : immutable_data) t
type ('a : value) u = 'a t
[%%expect {|
type ('a : immutable_data) t
type ('a : immutable_data) u = 'a t
|}]

type ('a : immutable_data) t
type 'a u = ('a * int) t
[%%expect {|
type ('a : immutable_data) t
Line 2, characters 13-21:
2 | type 'a u = ('a * int) t
                 ^^^^^^^^
Error: This type "'a * int" should be an instance of type "('b : immutable_data)"
       The kind of 'a * int is immutable_data with 'a
         because it's a tuple type.
       But the kind of 'a * int must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-28.
|}, Principal{|
type ('a : immutable_data) t
Line 2, characters 13-21:
2 | type 'a u = ('a * int) t
                 ^^^^^^^^
Error: This type "'a * int" should be an instance of type "('b : immutable_data)"
       The kind of 'a * int is immutable_data with 'a with int
         because it's a tuple type.
       But the kind of 'a * int must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-28.
|}]

(* Function types *)
type t : immutable_data = int -> int
[%%expect {|
Line 1, characters 0-36:
1 | type t : immutable_data = int -> int
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int -> int" is value mod aliased immutable non_float
         because it's a function type.
       But the kind of type "int -> int" must be a subkind of immutable_data
         because of the definition of t at line 1, characters 0-36.
|}]

type t : value mod contended
external f : unit -> t @ contended = "%identity"
let _ : unit -> t = f
[%%expect {|
type t : value mod contended
external f : unit -> t @ contended = "%identity"
Line 3, characters 20-21:
3 | let _ : unit -> t = f
                        ^
Error: This expression has type "unit -> t @ contended"
       but an expression was expected of type "unit -> t"
|}]

type t : value mod contended portable = { bar : exn * bool option * int option * t }
type q : value mod contended portable = { foo : t }
[%%expect{|
type t = { bar : exn * bool option * int option * t; }
type q = { foo : t; }
|}]

type ('a : any) t : value mod contended =
  { foo : ('b : any). 'b -> int }
[%%expect{|
type ('a : any) t = { foo : ('b : any). 'b -> int; }
|}]

type 'witness wrap : value mod portable with 'witness =
  { dummy : int }
[@@unsafe_allow_any_mode_crossing]

module type Derived1 = sig
  type 'cmp_a comparator_witness : value mod portable with 'cmp_a

  val comparator : 'cmp_a wrap -> 'cmp_a comparator_witness wrap
end

module T1 : Derived1 = struct
  type 'cmp_a comparator_witness : value mod portable with 'cmp_a

  let comparator a = { dummy = a.dummy }
end

module type S_portable = sig
  type comparator_witness : value mod portable

  val comparator : comparator_witness wrap
end

module type S = sig
  type comparator_witness

  val comparator : comparator_witness wrap
end

module Make_nonportable (A : S) = struct
  type comparator_witness = A.comparator_witness T1.comparator_witness

  let comparator = T1.comparator A.comparator
end

module type Result = sig
  type comparator_witness : value mod portable

  val comparator : comparator_witness wrap @@ portable
end

module Test (A : S_portable) : Result = struct
  include Make_nonportable (A)
end
[%%expect{|
type 'witness wrap : value mod portable with 'witness = { dummy : int; }
[@@unsafe_allow_any_mode_crossing]
module type Derived1 =
  sig
    type 'cmp_a comparator_witness : value mod portable with 'cmp_a
    val comparator : 'cmp_a wrap -> 'cmp_a comparator_witness wrap
  end
module T1 : Derived1
module type S_portable =
  sig
    type comparator_witness : value mod portable
    val comparator : comparator_witness wrap
  end
module type S =
  sig type comparator_witness val comparator : comparator_witness wrap end
module Make_nonportable :
  functor (A : S) ->
    sig
      type comparator_witness = A.comparator_witness T1.comparator_witness
      val comparator : A.comparator_witness T1.comparator_witness wrap
    end
module type Result =
  sig
    type comparator_witness : value mod portable
    val comparator : comparator_witness wrap @@ portable
  end
module Test : functor (A : S_portable) -> Result
|}]

module Id(A : sig type t : value end) : sig type t : value = A.t end = struct
  type t = A.t
end

module M = struct
  type t : value mod portable
end

module S : sig type t : value mod portable end = Id(M)
[%%expect{|
module Id : functor (A : sig type t end) -> sig type t = A.t end
module M : sig type t : value mod portable end
module S : sig type t : value mod portable end
|}]

(* Another example from Ben Peters *)

type 'a t : value mod portable with 'a @@ portable
type t2 = A of int | B of t2 t
type t3 : value mod portable = A of int | B of t2 t
type t4 : value mod portable = t2
[%%expect{|
type 'a t : value mod portable
type t2 = A of int | B of t2 t
type t3 = A of int | B of t2 t
type t4 = t2
|}]
