(* TEST
 flags = "-extension laws";
 expect;
*)

(* Laws in signatures and structures are matched up. Extra laws in the
   implementation are allowed. *)

module Matching_clauses : sig
  val x : int
  law_? x_pos : x > 0
end = struct
  let x = 1
  law_? x_pos : x > 0
  law_? x_small : x < 10
end
[%%expect {|
module Matching_clauses : sig val x : int law_? x_pos : x > (0) end
|}]

module Different_clauses : sig
  val x : int
  law_? x_pos : x > 0
end = struct
  let x = 1
  law_? x_pos : x >= 0
end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   let x = 1
6 |   law_? x_pos : x >= 0
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : int law_? x_pos : x >= (0) end
       is not included in
         sig val x : int law_? x_pos : x > (0) end
       Laws do not match:
         law_? x_pos : x >= (0)
       is not included in
         law_? x_pos : x > (0)
       The clauses of the two laws differ: "x >= (0)" is not the same as
       "x > (0)".
|}]

(* The assumptions are compared as well. *)

module Different_assumptions : sig
  law_? p (x : int) : x > 0 implies_ x = x
end = struct
  law_? p (x : int) : x > 1 implies_ x = x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law_? p (x : int) : x > 1 implies_ x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law_? p (x : int) : x > (1) implies_ x = x end
       is not included in
         sig law_? p (x : int) : x > (0) implies_ x = x end
       Laws do not match:
         law_? p (x : int) : x > (1) implies_ x = x
       is not included in
         law_? p (x : int) : x > (0) implies_ x = x
       The clauses of the two laws differ: "x > (1)" is not the same as
       "x > (0)".
|}]

(* Laws in the signature that are missing in the implementation are reported. *)

module Missing : sig
  val x : int
  law_? x_pos : x > 0
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
         sig val x : int law_? x_pos : x > (0) end
       The law "x_pos" is required but not provided
|}]

(* The laws must have the same number of parameters. *)

module Wrong_arity : sig
  law_? p (x : int) : x = x
end = struct
  law_? p (x : int) (y : int) : x = y
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law_? p (x : int) (y : int) : x = y
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law_? p (x : int) (y : int) : x = y end
       is not included in
         sig law_? p (x : int) : x = x end
       Laws do not match:
         law_? p (x : int) (y : int) : x = y
       is not included in
         law_? p (x : int) : x = x
       The first law has 2 parameters, but the second has 1.
|}]

(* The parameters must have compatible types. *)

module Wrong_type : sig
  law_? p (x : int) : x = x
end = struct
  law_? p (x : string) : x = x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law_? p (x : string) : x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law_? p (x : string) : x = x end
       is not included in
         sig law_? p (x : int) : x = x end
       Laws do not match:
         law_? p (x : string) : x = x
       is not included in
         law_? p (x : int) : x = x
       The parameter "x" has type "string" but it is expected to have type "int"
|}]

(* As for values, the parameter types of the structure's law may be more
   general than those of the signature's law, but not less general. *)

module More_general : sig
  law_? p (x : int) (f : int -> int) : f x = f x
end = struct
  law_? p (x : 'a) (f : 'a -> 'b) : f x = f x
end
[%%expect {|
module More_general :
  sig law_? p (x : int) (f : int -> int) : (f x) = (f x) end
|}]

module Less_general : sig
  law_? p (x : 'a) (f : 'a -> 'b) : f x = f x
end = struct
  law_? p (x : int) (f : int -> int) : f x = f x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law_? p (x : int) (f : int -> int) : f x = f x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law_? p (x : int) (f : int -> int) : (f x) = (f x) end
       is not included in
         sig law_? p (x : 'a) (f : 'a -> 'b) : (f x) = (f x) end
       Laws do not match:
         law_? p (x : int) (f : int -> int) : (f x) = (f x)
       is not included in
         law_? p (x : 'a) (f : 'a -> 'b) : (f x) = (f x)
       The parameter "x" has type "int" but it is expected to have type "'a"
|}]

(* The parameter types are instantiated together: the structure's law must be
   more general than the signature for one instantiation of its type variables.
   *)

module Shared_variable : sig
  law_? p (x : int) (y : string) : 1 = 1
end = struct
  law_? p (x : 'a) (y : 'a) : 1 = 1
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law_? p (x : 'a) (y : 'a) : 1 = 1
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law_? p (x : 'a) (y : 'a) : (1) = (1) end
       is not included in
         sig law_? p (x : int) (y : string) : (1) = (1) end
       Laws do not match:
         law_? p (x : 'a) (y : 'a) : (1) = (1)
       is not included in
         law_? p (x : int) (y : string) : (1) = (1)
       The parameter types do not match: the type "int"
       is not compatible with the type "string"
|}]

(* The kinds of the parameter types are compared too: a law over any ['a]
   (implicitly [value] kind) is a law over [('a : immutable_data)], but not
   the reverse. *)

module Kinded : sig
  law_? p (x : ('a : immutable_data)) : 1 = 1
end = struct
  law_? p (x : 'a) : 1 = 1
end
[%%expect {|
module Kinded : sig law_? p (x : ('a : immutable_data)) : (1) = (1) end
|}]

module Kinded_inverse : sig
  law_? p (x : 'a) : 1 = 1
end = struct
  law_? p (x : ('a : immutable_data)) : 1 = 1
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law_? p (x : ('a : immutable_data)) : 1 = 1
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law_? p (x : ('a : immutable_data)) : (1) = (1) end
       is not included in
         sig law_? p (x : 'a) : (1) = (1) end
       Laws do not match:
         law_? p (x : ('a : immutable_data)) : (1) = (1)
       is not included in
         law_? p (x : 'a) : (1) = (1)
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

(* Two laws of a signature cannot have the same name. *)

module type Dup = sig
  law_? p : 1 = 1
  law_? p : 1 = 0
end
[%%expect {|
Line 3, characters 2-17:
3 |   law_? p : 1 = 0
      ^^^^^^^^^^^^^^^
Error: Multiple definition of the law name "p".
       Names must be unique in a given structure or signature.
|}]

(* The clauses are compared up to the renaming of the parameters. *)

module Renamed_parameters : sig
  val f : int -> int
  law_? p (x : int) : f x = x
end = struct
  let f x = x
  law_? p (n : int) : f n = n
end
[%%expect {|
module Renamed_parameters :
  sig val f : int -> int law_? p (x : int) : (f x) = x end
|}]
