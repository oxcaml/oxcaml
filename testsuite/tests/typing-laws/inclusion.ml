(* TEST
 flags = "-extension laws";
 expect;
*)

(* A structure matches a signature with a law when it states the law too.
   Its extra laws are dropped, like extra values. *)
(* CR sspies: Should we really allow laws that do not show up in
   interfaces? *)

module N : sig
  val x : int
  law? x_pos : x > 0
end = struct
  let x = 1
  law? x_pos : x > 0
  law? x_small : x < 10
end
[%%expect {|
module N : sig val x : int law? x_pos : x > 0 end
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
         sig val x : int law? x_pos : x > 0 end
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
       The first has 2 parameters, but the second has 1.
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

(* The parameter types of the structure's law may be more general than
   those of the signature's, as for values. *)

module More_general : sig
  law? p (x : int) (f : int -> int) : f x = f x
end = struct
  law? p (x : 'a) (f : 'a -> 'b) : f x = f x
end
[%%expect {|
module More_general :
  sig law? p (x : int) (f : int -> int) : (f x) = (f x) end
|}]

(* Only the types of the parameters are compared, annotated or inferred:
   the interface may annotate and the implementation infer, or the
   reverse. *)

module Inferred : sig
  law? p (x : int) (y : int) : x + y = y + x
  law? q x y : x + y = y + x
end = struct
  law? p x y : x + y = y + x
  law? q (x : int) (y : int) : x + y = y + x
end
[%%expect {|
module Inferred :
  sig
    law? p (x : int) (y : int) : (x + y) = (y + x)
    law? q (x : int) (y : int) : (x + y) = (y + x)
  end
|}]

(* Two laws of a signature cannot have the same name. *)

module type Dup = sig
  law? p : true
  law? p : false
end
[%%expect {|
Line 3, characters 2-16:
3 |   law? p : false
      ^^^^^^^^^^^^^^
Error: Multiple definition of the law name "p".
       Names must be unique in a given structure or signature.
|}]

(* The clauses are compared up to the renaming of the parameters and of
   the variables bound within them. *)

module Renamed : sig
  type t = A | B of int
  val f : t -> int
  law? p (x : int) : (match B x with A -> 0 | B y -> y) = f (B x)
end = struct
  type t = A | B of int
  let f = function A -> 0 | B n -> n
  law? p (n : int) : (match B n with A -> 0 | B m -> m) = f (B n)
end
[%%expect {|
module Renamed :
  sig
    type t = A | B of int
    val f : t -> int
    law? p (x : int) :
      (match (B x : t) with | (A : t) -> 0 | (B y : t) -> y) = (f (B x : t))
  end
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
         sig law? p (x : int) : x = (x + 0) end
       is not included in
         sig law? p (x : int) : x = x end
       Laws do not match:
         law? p (x : int) : x = (x + 0)
       is not included in
         law? p (x : int) : x = x
       The clauses of the laws differ.
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
         sig law? p : (1. /. (-0.)) > 0. end
       is not included in
         sig law? p : (1. /. 0.) > 0. end
       Laws do not match:
         law? p : (1. /. (-0.)) > 0.
       is not included in
         law? p : (1. /. 0.) > 0.
       The clauses of the laws differ.
|}]

(* The assumptions are compared as well. *)

module Different_assumptions : sig
  law? p (x : int) : x > 0 ===> x = x
end = struct
  law? p (x : int) : x = x
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p (x : int) : x = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p (x : int) : x = x end
       is not included in
         sig law? p (x : int) : x > 0 ===> x = x end
       Laws do not match:
         law? p (x : int) : x = x
       is not included in
         law? p (x : int) : x > 0 ===> x = x
       The clauses of the laws differ.
|}]

(* When a single parameter has an incompatible type, it is named in the
   error. *)

module Which_parameter : sig
  law? p (x : int) (y : string) : x = 1 && y = ""
end = struct
  law? p (x : int) (y : int) : x = 1 && y = 0
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   law? p (x : int) (y : int) : x = 1 && y = 0
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig law? p (x : int) (y : int) : (x = 1) && (y = 0) end
       is not included in
         sig law? p (x : int) (y : string) : (x = 1) && (y = "") end
       Laws do not match:
         law? p (x : int) (y : int) : (x = 1) && (y = 0)
       is not included in
         law? p (x : int) (y : string) : (x = 1) && (y = "")
       The parameter "y" has type "int" but it is expected to have type "string"
|}]
