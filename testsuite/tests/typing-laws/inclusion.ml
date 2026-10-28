(* TEST
 flags = "-extension laws";
 expect;
*)

(* A structure matches a signature with a law when it states the law too.
   Its extra laws are dropped, like extra values. *)

module N : sig
  val x : int
  law? x_pos : x > 0
end = struct
  let x = 1
  law? x_pos : x > 0
  law? x_small : x < 10
end
[%%expect {|
module N : sig val x : int law? x_pos : x > (0) end
|}]

(* A law of the signature that the structure does not state is reported
   as missing. *)

module Missing : sig
  val x : int
  law? x_pos : x > 0
end = struct
  let x = 1
end
[%%expect {|
Lines 4-6, characters 6-3:
4 | ......struct
5 |   let x = 1
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : int end
       is not included in
         sig val x : int law? x_pos : x > (0) end
       The law "x_pos" is required but not provided
|}]

(* The laws must have the same number of parameters. *)

module Wrong_arity : sig
  law? p (x : int) : x = x
end = struct
  law? p (x : int) (y : int) : x = y
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p (x : int) (y : int) : x = y
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p (x : int) (y : int) : x = y end
       is not included in
         sig law? p (x : int) : x = x end
       Laws do not match:
         law? p (x : int) (y : int) : x = y
       is not included in
         law? p (x : int) : x = x
       The first law has 2 parameters, but the second has 1.
|}]

(* The parameters must have compatible types. *)

module Wrong_type : sig
  law? p (x : int) : x = x
end = struct
  law? p (x : string) : x = x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p (x : string) : x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p (x : string) : x = x end
       is not included in
         sig law? p (x : int) : x = x end
       Laws do not match:
         law? p (x : string) : x = x
       is not included in
         law? p (x : int) : x = x
       The parameter "x" has type "string" but it is expected to have type "int"
|}]

(* As for values, the parameter types of the structure's law may be more
   general than those of the signature's law, but not less general. *)

module More_general : sig
  law? p (x : int) (f : int -> int) : f x = f x
end = struct
  law? p (x : 'a) (f : 'a -> 'b) : f x = f x
end
[%%expect {|
module More_general :
  sig law? p (x : int) (f : int -> int) : (f x) = (f x) end
|}]

module Less_general : sig
  law? p (x : 'a) (f : 'a -> 'b) : f x = f x
end = struct
  law? p (x : int) (f : int -> int) : f x = f x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p (x : int) (f : int -> int) : f x = f x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p (x : int) (f : int -> int) : (f x) = (f x) end
       is not included in
         sig law? p (x : 'a) (f : 'a -> 'b) : (f x) = (f x) end
       Laws do not match:
         law? p (x : int) (f : int -> int) : (f x) = (f x)
       is not included in
         law? p (x : 'a) (f : 'a -> 'b) : (f x) = (f x)
       The parameter "x" has type "int" but it is expected to have type "'a"
|}]

(* The parameter types are instantiated together: the structure's law must
   be more general for one instance of its type variables. *)

module Shared_variable : sig
  law? p (x : int) (y : string) : 1 = 1
end = struct
  law? p (x : 'a) (y : 'a) : 1 = 1
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p (x : 'a) (y : 'a) : 1 = 1
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p (x : 'a) (y : 'a) : (1) = (1) end
       is not included in
         sig law? p (x : int) (y : string) : (1) = (1) end
       Laws do not match:
         law? p (x : 'a) (y : 'a) : (1) = (1)
       is not included in
         law? p (x : int) (y : string) : (1) = (1)
       The parameter types do not match: the type "int"
       is not compatible with the type "string"
|}]

(* The kinds of the parameter types are compared too: a law over any ['a]
   is a law over [('a : immutable_data)], but not the reverse. *)

module Kinded : sig
  law? p (x : ('a : immutable_data)) : 1 = 1
end = struct
  law? p (x : 'a) : 1 = 1
end
[%%expect {|
module Kinded : sig law? p (x : ('a : immutable_data)) : (1) = (1) end
|}]

module Kinded_inverse : sig
  law? p (x : 'a) : 1 = 1
end = struct
  law? p (x : ('a : immutable_data)) : 1 = 1
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p (x : ('a : immutable_data)) : 1 = 1
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p (x : ('a : immutable_data)) : (1) = (1) end
       is not included in
         sig law? p (x : 'a) : (1) = (1) end
       Laws do not match:
         law? p (x : ('a : immutable_data)) : (1) = (1)
       is not included in
         law? p (x : 'a) : (1) = (1)
       The parameter "x" has type "('a : immutable_data)"
       but it is expected to have type "('b : value)"
       The layout of 'a is value
         because it's a fresh unification variable,
         chosen to have layout value.
       But the layout of 'a must be a sublayout of value non_float
         because of the annotation on the type variable 'a.
       Note: The kinds mutable_data, immutable_data, and sync_data have
       the layout value non_float.
|}]

(* The types of a law stand for type variables, with their kinds. *)

module Types : sig
  law? p (x : 'a) (y : 'b) : 1 = 1
end = struct
  law? p (type a) (x : a) (y : 'b) : 1 = 1
end
[%%expect {|
module Types : sig law? p (x : 'a) (y : 'b) : (1) = (1) end
|}]

module Types_nullable : sig
  law? p (type a) (x : a) : 1 = 1
end = struct
  law? p (type (a : value_or_null)) (x : a) : 1 = 1
end
[%%expect {|
module Types_nullable : sig law? p (type a) (x : a) : (1) = (1) end
|}]

(* Two laws of a signature cannot have the same name. *)

module type Dup = sig
  law? p : 1 = 1
  law? p : 1 = 0
end
[%%expect {|
Line 3, characters 2-16:
3 |   law? p : 1 = 0
      ^^^^^^^^^^^^^^
Error: Multiple definition of the law name "p".
       Names must be unique in a given structure or signature.
|}]

(* The clauses are compared up to the renaming of the parameters. *)

module Renamed_parameters : sig
  val f : int -> int
  law? p (x : int) : f x = x
end = struct
  let f x = x
  law? p (n : int) : f n = n
end
[%%expect {|
module Renamed_parameters :
  sig val f : int -> int law? p (x : int) : (f x) = x end
|}]

(* Clauses that differ otherwise are a mismatch, even if equivalent. *)

module Different_clauses : sig
  law? p (x : int) : x = x
end = struct
  law? p (x : int) : x = x + 0
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p (x : int) : x = x + 0
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p (x : int) : x = (x + (0)) end
       is not included in
         sig law? p (x : int) : x = x end
       Laws do not match:
         law? p (x : int) : x = (x + (0))
       is not included in
         law? p (x : int) : x = x
       The clauses of the two laws differ.
|}]

(* Floating-point literals are compared by their representation: [0.] and
   [-0.] differ, although they are equal as floats. *)

module Zeros : sig
  law? p : 1. /. 0. > 0.
end = struct
  law? p : 1. /. -0. > 0.
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p : 1. /. -0. > 0.
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p : ((1.) /. (-0.)) > (0.) end
       is not included in
         sig law? p : ((1.) /. (0.)) > (0.) end
       Laws do not match:
         law? p : ((1.) /. (-0.)) > (0.)
       is not included in
         law? p : ((1.) /. (0.)) > (0.)
       The clauses of the two laws differ.
|}]

(* The clauses are compared up to the renaming of the parameters and of the
   variables bound within them. *)

module Renamed : sig
  type t = A | B of int
  val f : t -> int
  law? p (x : int) : (match B x with A -> 0 | B y -> y) = f (B x)
  law? q (x : int) : (let rec g y = if y > 0 then g (y - 1) else y in g x) = 0
end = struct
  type t = A | B of int
  let f = function A -> 0 | B n -> n
  law? p (n : int) : (match B n with A -> 0 | B m -> m) = f (B n)
  law? q (n : int) : (let rec h m = if m > 0 then h (m - 1) else m in h n) = 0
end
[%%expect {|
module Renamed :
  sig
    type t = A | B of int
    val f : t -> int
    law? p (x : int) :
      (match (B x : t) with | (A : t) -> (0) | (B y : t) -> y) =
        (f (B x : t))
    law? q (x : int) :
      (let rec g = fun y -> if y > (0) then (g (y - (1))) else y in g x) =
        (0)
  end
|}]

(* The assumptions are compared as well. *)

module Different_assumptions : sig
  law? p (x : int) : x > 0 ===> x = x
end = struct
  law? p (x : int) : x > 1 ===> x = x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p (x : int) : x > 1 ===> x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p (x : int) : x > (1) ===> x = x end
       is not included in
         sig law? p (x : int) : x > (0) ===> x = x end
       Laws do not match:
         law? p (x : int) : x > (1) ===> x = x
       is not included in
         law? p (x : int) : x > (0) ===> x = x
       The clauses of the two laws differ.
|}]
