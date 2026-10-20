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

(* Unlike the types of values, the parameter types of the structure's law
   must be equal to those of the signature's. *)
(* CR-someday sspies: In the future, we should relax this restriction to allow
   generalization, since we want to generate tests based on the .ml file. *)

module More_general : sig
  law? p (x : int) (f : int -> int) : f x = f x
end = struct
  law? p (x : 'a) (f : 'a -> 'b) : f x = f x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p (x : 'a) (f : 'a -> 'b) : f x = f x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p (x : 'a) (f : 'a -> 'b) : (f x) = (f x) end
       is not included in
         sig law? p (x : int) (f : int -> int) : (f x) = (f x) end
       Laws do not match:
         law? p (x : 'a) (f : 'a -> 'b) : (f x) = (f x)
       is not included in
         law? p (x : int) (f : int -> int) : (f x) = (f x)
       The parameter "x" has type "'a" but it is expected to have type "int"
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

(* The parameter types are compared together, with one renaming of the
   type variables for all of them. Thus, even once we support generalizing,
   the following example should still fail, since int and string don't unify. *)

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
       The parameter "x" has type "'a" but it is expected to have type "int"
|}]

(* The kinds of the parameter types are compared too: a law over any ['a]
   is not a law over [('a : immutable_data)]. *)

module Kinded : sig
  law? p (x : ('a : immutable_data)) : 1 = 1
end = struct
  law? p (x : 'a) : 1 = 1
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p (x : 'a) : 1 = 1
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p (x : 'a) : (1) = (1) end
       is not included in
         sig law? p (x : ('a : immutable_data)) : (1) = (1) end
       Laws do not match:
         law? p (x : 'a) : (1) = (1)
       is not included in
         law? p (x : ('a : immutable_data)) : (1) = (1)
       The parameter "x" has type "('a : value_or_null)"
       but it is expected to have type "('a0 : immutable_data)"
       because the layouts of their variables are different.
       The layout of 'a is value_or_null
         because it's the type of a law parameter.
       The layout of 'a0 is immutable_data
         because of the annotation on the type variable 'a.
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
       but it is expected to have type "('a0 : value_or_null)"
       because the layouts of their variables are different.
       The layout of 'a is immutable_data
         because of the annotation on the type variable 'a.
       The layout of 'a0 is value_or_null
         because it's the type of a law parameter.
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
