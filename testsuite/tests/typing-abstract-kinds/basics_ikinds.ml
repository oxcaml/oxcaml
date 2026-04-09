(* TEST
   flags = "-ikinds";
   expect;
*)

(* Basic tests for abstract kinds. No with-kind substutitions yet. *)

(****************************************************)
(* Test: Abstract kinds allowed in sigs and structs *)

kind_ k

module type S = sig
  kind_ k
  type t : k
end

module M = struct
  kind_ k
  type t : k
end

module M' : S = M

[%%expect{|
kind_ k
module type S = sig kind_ k type t : k end
module M : sig kind_ k type t : k end
module M' : S
|}]

(**************************************************)
(* Test: Kind aliases allowed in sigs and structs *)

kind_ k = immutable_data

module type S = sig
  kind_ k = float64
  type t : k
end

module M = struct
  kind_ k = float64
  type t : k
end

module M' : S = M

[%%expect{|
kind_ k = immutable_data
module type S = sig kind_ k = float64 type t : float64 end
module M : sig kind_ k = float64 type t : float64 end
module M' : S
|}]

(***********************************************************)
(* Test: Abstract kinds are no concrete kind in particular *)

kind_ k
[%%expect{|
kind_ k
|}]

type t : k = int
[%%expect{|
Line 1, characters 0-16:
1 | type t : k = int
    ^^^^^^^^^^^^^^^^
Error: The kind of type "int" is immediate
         because it is the primitive type int.
       But the kind of type "int" must be a subkind of k
         because of the definition of t at line 1, characters 0-16.
|}]

type t : k = float#
[%%expect{|
Line 1, characters 0-19:
1 | type t : k = float#
    ^^^^^^^^^^^^^^^^^^^
Error: The kind of type "float#" is float64 mod everything
         because it is the unboxed version of the primitive type float.
       But the kind of type "float#" must be a subkind of k
         because of the definition of t at line 1, characters 0-19.
|}]

type t : k = int64#
[%%expect{|
Line 1, characters 0-19:
1 | type t : k = int64#
    ^^^^^^^^^^^^^^^^^^^
Error: The kind of type "int64#" is bits64 mod everything
         because it is the unboxed version of the primitive type int64.
       But the kind of type "int64#" must be a subkind of k
         because of the definition of t at line 1, characters 0-19.
|}]

type t : k = #(float# * int64#)
[%%expect{|
Line 1, characters 0-31:
1 | type t : k = #(float# * int64#)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "#(float# * int64#)" is
           float64 mod everything & bits64 mod everything
         because it is an unboxed tuple.
       But the kind of type "#(float# * int64#)" must be a subkind of k
         because of the definition of t at line 1, characters 0-31.
|}]

(**********************************************)
(* Test: Abstract kinds are not representable *)

kind_ k

type t : k

let f (x : t) = x
[%%expect{|
kind_ k
type t : k
Line 5, characters 6-13:
5 | let f (x : t) = x
          ^^^^^^^
Error: This pattern matches values of type "t"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_1)"
       The kind of t is k
         because of the definition of t at line 3, characters 0-10.
       But the kind of t must be representable
         because we must know concretely how to pass a function argument.
|}]

let _ = let x : ('a : k) = assert false in ()
[%%expect{|
Line 1, characters 12-13:
1 | let _ = let x : ('a : k) = assert false in ()
                ^
Error: This pattern matches values of type "('a : k)"
       but a pattern was expected which matches values of type
         "('b : '_representable_layout_2)"
       The layout of 'a is '_representable_layout_2
         because it's the type of a variable bound by a `let`.
       But the layout of 'a must overlap with the abstract kind k
         because of the annotation on the type variable 'a.
|}]

(*********************************************)
(* Test: Abstract kinds are a subkind of max *)

kind_ k
type t : k

type s : any = t
[%%expect{|
kind_ k
type t : k
type s = t
|}]

type s : bits32 = t
[%%expect{|
Line 1, characters 0-19:
1 | type s : bits32 = t
    ^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is k
         because of the definition of t at line 2, characters 0-10.
       But the kind of type "t" must be a subkind of bits32
         because of the definition of s at line 1, characters 0-19.
|}]

kind_ k_portable = k mod portable

type t_portable : k_portable
type s_portable : any mod portable = t_portable
[%%expect{|
kind_ k_portable = k mod portable
type t_portable : k mod portable
type s_portable = t_portable
|}]

(****************************************************)
(* Test: kind aliases work like the kind they alias *)

kind_ k = value & value mod portable

type t : k

type s1 : value & value = t
type s2 : value & value mod portable = t
type s3 : any mod portable = t
type s4 : any = t

let require_portable (_x : t @ portable) = ()
let cross_portable : t -> unit = fun x -> require_portable x
[%%expect{|
kind_ k = value mod portable & value mod portable
type t : value mod portable & value mod portable
type s1 = t
type s2 = t
type s3 = t
type s4 = t
val require_portable : t @ portable -> unit = <fun>
val cross_portable : t -> unit = <fun>
|}]

type s5 : value & value mod global = t
[%%expect{|
Line 1, characters 0-38:
1 | type s5 : value & value mod global = t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is value mod portable & value mod portable
         because of the definition of t at line 3, characters 0-10.
       But the kind of type "t" must be a subkind of
           value mod global & value mod global
         because of the definition of s5 at line 1, characters 0-38.
|}]

let does_not_cross_local (x : t @ local) : t @ global = x
[%%expect{|
Line 1, characters 56-57:
1 | let does_not_cross_local (x : t @ local) : t @ global = x
                                                            ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(****************************************************************)
(* Test: expanding through aliases takes the meet of the bounds *)

module A = struct
  kind_ ka = float64
end

module B = struct
  kind_ kb = A.ka mod portable
end

module C = struct
  kind_ kc = B.kb
end

module D = struct
  kind_ kd = C.kc mod global
end

type t : D.kd

type s1 : float64 mod portable global = t
type s2 : any mod portable global = t
[%%expect{|
module A : sig kind_ ka = float64 end
module B : sig kind_ kb = float64 mod portable end
module C : sig kind_ kc = float64 mod portable end
module D : sig kind_ kd = float64 mod global portable end
type t : float64 mod global portable
type s1 = t
type s2 = t
|}]

type s3 : any mod contended = t
[%%expect{|
Line 1, characters 0-31:
1 | type s3 : any mod contended = t
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is float64 mod global portable
         because of the definition of t at line 17, characters 0-13.
       But the kind of type "t" must be a subkind of any mod contended
         because of the definition of s3 at line 1, characters 0-31.
|}]

(************************)
(* Test: no with bounds *)

type t

kind_ k = immutable_data with t
[%%expect{|
type t
Line 3, characters 30-31:
3 | kind_ k = immutable_data with t
                                  ^
Error: 'with' syntax is not allowed in kind declarations.
|}]

(* This one could be allowed because the concrete with bound could be expanded
   away, but for simplicity we choose not to. *)
kind_ k = immutable_data with int
[%%expect{|
Line 1, characters 30-33:
1 | kind_ k = immutable_data with int
                                  ^^^
Error: 'with' syntax is not allowed in kind declarations.
|}]

(**********************)
(* Test: no recursion *)

kind_ k_rec = k_rec
[%%expect{|
Line 1, characters 14-19:
1 | kind_ k_rec = k_rec
                  ^^^^^
Error: Unbound kind "k_rec"
|}]

(***************************************)
(* Test: you can shadow built-in kinds *)

module Spicy = struct
  kind_ value = float64 & float64

  type t : value = #(float# * float#)
end
[%%expect{|
module Spicy :
  sig kind_ value = float64 & float64 type t = #(float# * float#) end
|}]

type t : Spicy.value = string
[%%expect{|
Line 1, characters 0-29:
1 | type t : Spicy.value = string
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "string" is value non_float
         because it is the primitive type string.
       But the layout of type "string" must be a sublayout of float64 & float64
         because of the definition of t at line 1, characters 0-29.
|}]

(***********************************)
(* Test: Abstracting in signatures *)

(* You can abstract away an alias *)
module M : sig
  kind_ k

  type t : k

  val f : unit -> t
end = struct
  kind_ k = value

  type t = string

  let f () = "hi mom"
end
[%%expect{|
module M : sig kind_ k type t : k val f : unit -> t end
|}]

(* But be careful - now we don't know enough to call f! *)
let _ = M.f ()
[%%expect{|
Line 1, characters 8-14:
1 | let _ = M.f ()
            ^^^^^^
Error: This expression has type "M.t" but an expression was expected of type
         "('a : '_representable_layout_3)"
       The kind of M.t is M.k
         because of the definition of t at line 4, characters 2-12.
       But the kind of M.t must be representable
         because it's the type of a variable bound by a `let`.
|}]

(* If you expose the definition, you have to be exact. *)
module M : sig
  kind_ k = value
end = struct
  kind_ k = immediate
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   kind_ k = immediate
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = immediate end
       is not included in
         sig kind_ k = value end
       Kind declarations do not match:
         kind_ k = immediate
       is not included in
         kind_ k = value
       Their definitions are not equal.
|}]

module M : sig
  kind_ k = immediate
end = struct
  kind_ k = value
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   kind_ k = value
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = value end
       is not included in
         sig kind_ k = immediate end
       Kind declarations do not match:
         kind_ k = value
       is not included in
         kind_ k = immediate
       Their definitions are not equal.
|}]

module M : sig
  kind_ k = value
end = struct
  kind_ k
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   kind_ k
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k end
       is not included in
         sig kind_ k = value end
       Kind declarations do not match:
         kind_ k
       is not included in
         kind_ k = value
       The the first is abstract, but the second is not.
|}]

module M : sig
  kind_ k = any
end = struct
  kind_ k
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   kind_ k
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k end
       is not included in
         sig kind_ k = any end
       Kind declarations do not match:
         kind_ k
       is not included in
         kind_ k = any
       The the first is abstract, but the second is not.
|}]

(*********************************)
(* Test: Recursive module basics *)

(* Kind declarations are supported in recursive module signatures. *)
module rec M : sig
  kind_ k
end = struct
  kind_ k
end
[%%expect{|
module rec M : sig kind_ k end
|}]

(* And in recursive structures. *)
module rec M : sig
  type t : value
end = struct
  kind_ k = value
  type t : k
end
[%%expect{|
module rec M : sig type t end
|}]

(* Kind declarations also work in nested modules and functors *)
module rec M : sig
  module N : sig
    kind_ k
  end
end = struct
  module N = struct
    kind_ k
  end
end
[%%expect{|
module rec M : sig module N : sig kind_ k end end
|}]

module rec M : sig
  module F (Arg : sig end) : sig
    kind_ k
  end
end = struct
  module F (Arg : sig end) = struct
    kind_ k
  end
end
[%%expect{|
module rec M : sig module F : functor (Arg : sig end) -> sig kind_ k end end
|}]

(* Kind alias is preserved in a recursive module. *)
module rec M : sig
  kind_ k = value
end = struct
  kind_ k = value
end
[%%expect{|
module rec M : sig kind_ k = value end
|}]

(* Limitation: kind declaration manifests are erased during recursive module
   type approximation, so you can't depend on the RHS of a kind alias within a
   recursive group. *)
module rec M : sig
  kind_ k = value
  type s : k
end = struct
  kind_ k = value
  type s = int
end
and N : sig
  type ('a : value) t
  type r = M.s t
end = struct
  type 'a t = 'a list
  type r = M.s t
end
[%%expect{|
Line 10, characters 11-14:
10 |   type r = M.s t
                ^^^
Error: This type "M.s" should be an instance of type "('a : value)"
       The kind of M.s is M.k
         because of the annotation on the declaration of the type s.
       But the kind of M.s must be a subkind of value
         because of the definition of t at line 9, characters 2-21.
|}]

(*********************************)
(* Test: Recursive module cycles *)

(* A kind alias that refers to a kind from the same recursive module is
   rejected because it would form a cycle. *)
module rec M : sig
  kind_ k = M.k
end = struct
  kind_ k = M.k
end
[%%expect{|
Lines 1-5, characters 0-3:
1 | module rec M : sig
2 |   kind_ k = M.k
3 | end = struct
4 |   kind_ k = M.k
5 | end
Error: The kind "M.k" is cyclic:
         "M.k" = "M.k"
|}]

(* Cross-module kind cycle: M.k -> N.j -> M.k *)
module rec M : sig
  kind_ k = N.j
end = struct
  kind_ k = N.j
end
and N : sig
  kind_ j = M.k
end = struct
  kind_ j = M.k
end
[%%expect{|
Lines 1-5, characters 0-3:
1 | module rec M : sig
2 |   kind_ k = N.j
3 | end = struct
4 |   kind_ k = N.j
5 | end
Error: The kind "M.k" is cyclic:
         "M.k" = "N.j",
         "N.j" = "M.k"
|}]

(* Kind cycle with mod bounds *)
module rec M : sig
  kind_ k = M.k mod global
end = struct
  kind_ k = M.k mod global
end
[%%expect{|
Lines 1-5, characters 0-3:
1 | module rec M : sig
2 |   kind_ k = M.k mod global
3 | end = struct
4 |   kind_ k = M.k mod global
5 | end
Error: The kind "M.k" is cyclic:
         "M.k" = "M.k mod global",
         "M.k mod global" contains "M.k"
|}]

(* A kind alias to a kind defined outside the recursive group is fine. *)
module K : sig
  kind_ j
end = struct
  kind_ j
end

module rec M : sig
  kind_ k = K.j
end = struct
  kind_ k = K.j
end
[%%expect{|
module K : sig kind_ j end
module rec M : sig kind_ k = K.j end
|}]

(* A kind alias to a non-cyclic kind within the same recursive group
   is fine. *)
module rec M : sig
  kind_ k = value
end = struct
  kind_ k = value
end
and N : sig
  kind_ j = M.k
end = struct
  kind_ j = M.k
end
[%%expect{|
module rec M : sig kind_ k = value end
and N : sig kind_ j = M.k end
|}]

(* A longer kind cycle. *)
module rec A : sig
  kind_ ka = D.kd
end = struct
  kind_ ka = D.kd
end
and B : sig
  kind_ kb = A.ka mod global
end = struct
  kind_ kb = A.ka mod global
end
and C : sig
  kind_ kc = B.kb
end = struct
  kind_ kc = B.kb
end
and D : sig
  kind_ kd = C.kc mod contended many
end = struct
  kind_ kd = C.kc mod contended many
end
[%%expect{|
Lines 1-5, characters 0-3:
1 | module rec A : sig
2 |   kind_ ka = D.kd
3 | end = struct
4 |   kind_ ka = D.kd
5 | end
Error: The kind "A.ka" is cyclic:
         "A.ka" = "D.kd",
         "D.kd" = "C.kc mod many contended",
         "C.kc mod many contended" contains "C.kc",
         "C.kc" = "B.kb",
         "B.kb" = "A.ka mod global",
         "A.ka mod global" contains "A.ka"
|}]

(* Kind cycle through a functor application *)
module F (X : sig kind_ k end) : sig
  kind_ fk = X.k
end = struct
  kind_ fk = X.k
end

module rec A : sig
  kind_ k = F(A).fk mod portable
end = struct
  kind_ k = F(A).fk mod portable
end
[%%expect{|
module F : functor (X : sig kind_ k end) -> sig kind_ fk = X.k end
Lines 7-11, characters 0-3:
 7 | module rec A : sig
 8 |   kind_ k = F(A).fk mod portable
 9 | end = struct
10 |   kind_ k = F(A).fk mod portable
11 | end
Error: The kind "A.k" is cyclic:
         "A.k" = "F(A).fk mod portable",
         "F(A).fk mod portable" contains "F(A).fk",
         "F(A).fk" = "A.k"
|}]

(* Kind declarations that come from signatures which are defined before the
   recursive group are fine, since they can't be use to introduce recursive
   kinds. *)
module K : sig
  kind_ k
end = struct
  kind_ k
end

module rec M : sig
  module M1 = K
  module M2 : module type of K
end = struct
  module M1 = K
  module M2 = K
end
[%%expect{|
module K : sig kind_ k end
module rec M : sig module M1 = K module M2 : sig kind_ k end end
|}]

(***********************)
(* Test: Strengthening *)

module type S = sig
  module type T = sig kind_ k end
  module M : T
  module N : T
  module Q : T with M
end
[%%expect{|
module type S =
  sig
    module type T = sig kind_ k end
    module M : T
    module N : T
    module Q : sig kind_ k = M.k end
  end
|}]

(* Q.k is known to be M.k *)
module F(X : S) = struct
  type t : X.Q.k
  type s : X.M.k = t
end
[%%expect{|
module F : functor (X : S) -> sig type t : X.M.k type s = t end
|}]

(* N.k is not *)
module F(X : S) = struct
  type t : X.N.k
  type s : X.M.k = t
end
[%%expect{|
Line 3, characters 2-20:
3 |   type s : X.M.k = t
      ^^^^^^^^^^^^^^^^^^
Error: The kind of type "t" is X.N.k
         because of the definition of t at line 2, characters 2-16.
       But the kind of type "t" must be a subkind of X.M.k
         because of the definition of s at line 3, characters 2-20.
|}]

(*********************************)
(* Test: arrays and separability *)

(* Since fully abstract kinds aren't separable, you can't have an array of
   them. But you can have an array of a kind with abstract base and the
   appropriate mod bound. *)
kind_ k
type t1 : k
type s1 = t1 array
[%%expect{|
kind_ k
type t1 : k
Line 3, characters 10-12:
3 | type s1 = t1 array
              ^^
Error: This type "t1" should be an instance of type "('a : any separable)"
       The kind of t1 is k
         because of the definition of t1 at line 2, characters 0-11.
       But the kind of t1 must be a subkind of any separable
         because it's the type argument to the array type.
|}]

(* CR layouts-scannable: support scannable axes on abstract kinds *)
type t2 : k mod separable
type s2 = t2 array
[%%expect{|
Line 1, characters 16-25:
1 | type t2 : k mod separable
                    ^^^^^^^^^
Error: Abstract kinds with kind modifiers are not yet supported.
|}]

type ('a : k mod separable) s3 = 'a array
[%%expect{|
Line 1, characters 17-26:
1 | type ('a : k mod separable) s3 = 'a array
                     ^^^^^^^^^
Error: Abstract kinds with kind modifiers are not yet supported.
|}]

kind_ k' = k mod separable
type t4 : k'
type s4 = t4 array
[%%expect{|
Line 1, characters 17-26:
1 | kind_ k' = k mod separable
                     ^^^^^^^^^
Error: Abstract kinds with kind modifiers are not yet supported.
|}]

(******************************)
(* Test: Bad product behavior *)

(* We reject this for now. See internal ticket 5769. *)
kind_ k
kind_ k_prod = k & k

[%%expect{|
kind_ k
Line 2, characters 15-20:
2 | kind_ k_prod = k & k
                   ^^^^^
Error: Abstract kinds are not yet supported in products.
|}]

(************************************)
(* Test: include functor and nondep *)

(* By testing include functor, this also tests [sig_make_manifest] and
   [nondep_*] for jkinds. Note the functor parameter appears in a kind
   in three key positions handled separately by nondep:
   - kind decl manifest
   - type decl kind annotation
   - type decl parameter
   And this test checks that after the functor application, the typechecker can
   see the relationship between the result and the "parameter". *)
module type S = sig
  kind_ k1
  type t1 : k1
end

module F(X : S) = struct
  kind_ k2 = X.k1
  type ('a : X.k1) t1' : X.k1
end

kind_ k1
type t1 : k1

include functor F

type ('a : k2) s
type r = t1 s
type t2 : k2
type q : k2 = t2 t1'

[%%expect{|
module type S = sig kind_ k1 type t1 : k1 end
module F :
  functor (X : S) -> sig kind_ k2 = X.k1 type ('a : X.k1) t1' : X.k1 end
kind_ k1
type t1 : k1
kind_ k2 = k1
type ('a : k1) t1' : k1
type ('a : k2) s
type r = t1 s
type t2 : k1
type q = t2 t1'
|}]


(****************************)
(* Test: Nondep error cases *)

(* We can't always round up jkinds with abstract bases, so we may need to error
   because of a type that can't be erased in the with bounds. *)
module F(X : sig type t end) = struct
  kind_ k
  type t : k with X.t
end

module M = F(struct type t end)
    [%%expect{|
module F :
  functor (X : sig type t end) -> sig kind_ k type t : k with X.t end
Line 6, characters 11-31:
6 | module M = F(struct type t end)
               ^^^^^^^^^^^^^^^^^^^^
Error: This functor has type
       "functor (X : sig type t end) -> sig kind_ k type t : k with X.t end"
       The parameter cannot be eliminated in the result type.
       Please bind the argument to a module identifier.
|}]

(* Appears in signature (similar to tests in [generalized-open/gpr1506.ml]) *)
include struct
  open struct
    kind_ k
    type t : k
  end
  type s = t
end
[%%expect{|
Lines 2-5, characters 2-5:
2 | ..open struct
3 |     kind_ k
4 |     type t : k
5 |   end
Error: The kind "k" introduced by this open appears in the signature.
Line 6, characters 2-12:
6 |   type s = t
      ^^^^^^^^^^
  The type "s" has no valid kind if "k" is hidden.
|}]

include struct
  open struct
    kind_ k
  end
  kind_ k' = k
end
[%%expect{|
Lines 2-4, characters 2-5:
2 | ..open struct
3 |     kind_ k
4 |   end
Error: The kind "k" introduced by this open appears in the signature.
Line 5, characters 2-14:
5 |   kind_ k' = k
      ^^^^^^^^^^^^
  The kind "k'" has no valid definition if "k" is hidden.
|}]

(* Illegal shadowing (similar to first test in [typing-sigsubst/sigsubst.ml]) *)
module type S1 = sig
  kind_ k
  type t1 : k
end
module type S2 = sig
  kind_ k
  type t2 : k
end
module type Combined = sig
  include S1
  include S2
end
[%%expect{|
module type S1 = sig kind_ k type t1 : k end
module type S2 = sig kind_ k type t2 : k end
Line 11, characters 2-12:
11 |   include S2
       ^^^^^^^^^^
Error: Illegal shadowing of included kind "k/2" by "k/1".
Line 10, characters 2-12:
10 |   include S1
       ^^^^^^^^^^
  Kind "k/2" came from this include.
Line 3, characters 2-13:
3 |   type t1 : k
      ^^^^^^^^^^^
  The type "t1" has no valid kind if "k/2" is shadowed.
|}]


(*****************************************************)
(* Test: Kinds in generative vs applicative functors *)

(* This is an applicative functor - we check that [F(X).k] gives the same [k]
   for the same [X] (and different [k]s for different [X]s. *)
module F (X : sig type t end) = struct
  kind_ k
end

module M1 = struct type t end
module M2 = struct type t end

module F_M1 = F(M1)
module F_M1' = F(M1)
module F_M2 = F(M2)

module M : sig kind_ k = F_M1.k end = F_M1'
module M : sig kind_ k = F_M1'.k end = F_M1

[%%expect{|
module F : functor (X : sig type t end) -> sig kind_ k end
module M1 : sig type t end
module M2 : sig type t end
module F_M1 : sig kind_ k = F(M1).k end
module F_M1' : sig kind_ k = F(M1).k end
module F_M2 : sig kind_ k = F(M2).k end
module M : sig kind_ k = F_M1.k end
module M : sig kind_ k = F_M1'.k end
|}]

module M : sig kind_ k = F_M2.k end = F_M1

[%%expect{|
Line 1, characters 38-42:
1 | module M : sig kind_ k = F_M2.k end = F_M1
                                          ^^^^
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = F(M1).k end
       is not included in
         sig kind_ k = F_M2.k end
       Kind declarations do not match:
         kind_ k = F(M1).k
       is not included in
         kind_ k = F_M2.k
       Their definitions are not equal.
|}]

(* This is a generative functor - any two applications of it give different [k]s
   *)
module F () = struct
  kind_ k
end

module M1 = F ()
module M2 = F ()

module M : sig kind_ k = M1.k end = M2

[%%expect{|
module F : functor () -> sig kind_ k end
module M1 : sig kind_ k end
module M2 : sig kind_ k end
Line 8, characters 36-38:
8 | module M : sig kind_ k = M1.k end = M2
                                        ^^
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = M2.k end
       is not included in
         sig kind_ k = M1.k end
       Kind declarations do not match:
         kind_ k = M2.k
       is not included in
         kind_ k = M1.k
       Their definitions are not equal.
|}]

(* As with types, an applicative functor can't include the output of a
   generative functor that creates a kind. *)

module F_gen () = struct kind_ k end

module F_app (X : sig type s end) = struct
  module M = F_gen ()
end

[%%expect{|
module F_gen : functor () -> sig kind_ k end
Line 4, characters 13-21:
4 |   module M = F_gen ()
                 ^^^^^^^^
Error: This expression creates fresh kinds.
       It is not allowed inside applicative functors.
|}]

module F_gen (X : sig type s end) () = struct kind_ k end

module F_app (X : sig type s end) = struct
  include X
  include functor F_gen
end

[%%expect{|
module F_gen : functor (X : sig type s end) () -> sig kind_ k end
Line 5, characters 18-23:
5 |   include functor F_gen
                      ^^^^^
Error: This functor creates fresh kinds when applied.
       Including it is not allowed inside applicative functors.
|}]

(*******************)
(* Test: Shadowing *)

(* As with types, you can't shadow a kind declared in this structure or
   signature. *)
module M = struct
  kind_ k
  kind_ k
end

[%%expect{|
Line 3, characters 2-9:
3 |   kind_ k
      ^^^^^^^
Error: Multiple definition of the kind name "k".
       Names must be unique in a given structure or signature.
|}]

module type S = sig
  kind_ k
  kind_ k
end

[%%expect{|
Line 3, characters 2-9:
3 |   kind_ k
      ^^^^^^^
Error: Multiple definition of the kind name "k".
       Names must be unique in a given structure or signature.
|}]

(* You can't get around this with an include *)
module M = struct kind_ k end
module M' = struct
  kind_ k
  include M
end

[%%expect{|
module M : sig kind_ k end
Line 4, characters 2-11:
4 |   include M
      ^^^^^^^^^
Error: Multiple definition of the kind name "k".
       Names must be unique in a given structure or signature.
|}]

module type S = sig kind_ k end
module type S' = sig
  kind_ k
  include S
end

[%%expect{|
module type S = sig kind_ k end
Line 4, characters 2-11:
4 |   include S
      ^^^^^^^^^
Error: Multiple definition of the kind name "k".
       Names must be unique in a given structure or signature.
|}]

(* Nor an include functor. *)
module F(X : sig kind_ k end) = X

module M = struct
  kind_ k

  include functor F
end

[%%expect{|
module F : functor (X : sig kind_ k end) -> sig kind_ k = X.k end
Line 6, characters 2-19:
6 |   include functor F
      ^^^^^^^^^^^^^^^^^
Error: Multiple definition of the kind name "k".
       Names must be unique in a given structure or signature.
|}]

(* But you are allowed to shadow something that is itself from an include. *)
module M = struct kind_ k end
module M' = struct
  include M
  kind_ k
end

[%%expect{|
module M : sig kind_ k end
module M' : sig kind_ k end
|}]

module type S = sig kind_ k end
module type S' = sig
  include S
  kind_ k
end

[%%expect{|
module type S = sig kind_ k end
module type S' = sig kind_ k end
|}]

(* Or an include functor. *)
module F(X : sig kind_ k end) = struct kind_ k2 = X.k end

module M = struct
  kind_ k

  include functor F

  kind_ k2 = value
end

[%%expect{|
module F : functor (X : sig kind_ k end) -> sig kind_ k2 = X.k end
module M : sig kind_ k kind_ k2 = value end
|}]

(**************************)
(* Test: [module type of] *)

module M = struct
  kind_ k1
  kind_ k1' = k1

  kind_ k2 = float64
  kind_ k2' = k2 mod global
end

module type S = module type of M

[%%expect{|
module M :
  sig
    kind_ k1
    kind_ k1' = k1
    kind_ k2 = float64
    kind_ k2' = float64 mod global
  end
module type S =
  sig
    kind_ k1
    kind_ k1' = k1
    kind_ k2 = float64
    kind_ k2' = float64 mod global
  end
|}]

(***************************************************)
(* Test: GADTs, exhaustiveness, and abstract kinds *)

(* Basic GADT with abstract kind - exhaustiveness warning expected.  When a type
   has an abstract kind, we can't rule out that it might equal another type, so
   pattern matching must be conservative. *)
module M1 : sig
  kind_ k
  type t_k : k
  type ('a : any) t = Int : int t | K : t_k t
  val v : int t
end = struct
  kind_ k = value
  type t_k : k = int
  type ('a : any) t = Int : int t | K : t_k t
  let v = K
end

let f1 (x : int M1.t) = match x with
  | M1.Int -> "int"

[%%expect{|
module M1 :
  sig
    kind_ k
    type t_k : k
    type ('a : any) t = Int : int t | K : t_k t
    val v : int t
  end
Lines 13-14, characters 24-19:
13 | ........................match x with
14 |   | M1.Int -> "int"
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
K

val f1 : int M1.t -> string = <fun>
|}]

(* Exhaustive match with all constructors - no warning *)
let f2 (x : int M1.t) = match x with
  | M1.Int -> "int"
  | M1.K -> "k"

[%%expect{|
val f2 : int M1.t -> string = <fun>
|}]

(* When the kind is known (not abstract), exhaustiveness works normally *)
module M4 : sig
  kind_ k = float64
  type t_k : k
  type ('a : any) t = Int : int t | K : t_k t
end = struct
  kind_ k = float64
  type t_k : k = float#
  type ('a : any) t = Int : int t | K : t_k t
end

let f4 (x : int M4.t) = match x with
  | M4.Int -> "int"

[%%expect{|
module M4 :
  sig
    kind_ k = float64
    type t_k : float64
    type ('a : any) t = Int : int t | K : t_k t
  end
val f4 : int M4.t -> string = <fun>
|}]

(*********************************************************************)
(* Test: Incompleteness when GADT implies equality of abstract kinds *)

(* When a GADT match implies two types are equal, we might expect to learn their
   kinds are equal, or really that the kinds of both types are the intersection
   of the original kinds of both types. We don't have a way to do this now when
   an abstract kind is involved, so type inference is incomplete in the sense
   that some things the user might expect to be true under a gadt match aren't.

   Type inference around gadts was already incomplete for a bunch of reasons, so
   this isn't surprising or concerning - this test just documents the current
   behavior. *)
module type S = sig
  kind_ k
  type t : k
end

(* This bit works - we can still learn relevant type equations. *)
module F (X : S) (Y : S) = struct
  type ('a : any) repr =
    | X : X.t repr
    | Y : Y.t repr

  let use_y (_ : Y.t repr) : unit = ()
  let use_x (_ : X.t repr) : unit = ()

  let f (x1 : X.t repr) (x2 : X.t repr) (y : Y.t repr) : unit =
    match x1 with
    | X -> ()
    | Y -> (use_y x2; use_x y)
end

[%%expect{|
module type S = sig kind_ k type t : k end
module F :
  functor (X : S) (Y : S) ->
    sig
      type ('a : any) repr = X : X.t repr | Y : Y.t repr
      val use_y : Y.t repr -> unit
      val use_x : X.t repr -> unit
      val f : X.t repr -> X.t repr -> Y.t repr -> unit
    end
|}]

(* But we can't directly that [X.t] must have kind [Y.k]. *)
module F2 (X : S) (Y : S) = struct
  type ('a : any) repr =
    | X_val : X.t repr
    | Y_val : Y.t repr

  let f (x : X.t repr) : unit =
    match x with
    | X_val -> ()
    | Y_val ->
      let module M = struct
        type t : Y.k = X.t
      end in
      ()
end

[%%expect{|
Line 11, characters 8-26:
11 |         type t : Y.k = X.t
             ^^^^^^^^^^^^^^^^^^
Error: The kind of type "X.t" is X.k
         because of the definition of t at line 3, characters 2-12.
       But the kind of type "X.t" must be a subkind of Y.k
         because of the definition of t at line 11, characters 8-26.
|}]

(***********************************)
(* Test: With kinds and subkinding *)

kind_ k1
kind_ k2
type t1 : k1
type t2 : k2
type ('a : k1) require_k1
[%%expect{|
kind_ k1
kind_ k2
type t1 : k1
type t2 : k2
type ('a : k1) require_k1
|}]

(* In the [-ikinds] variant this is accepted: [t1]'s kind is [k1], so the
   with bound does not change the required kind. *)
type a : k1 with t1
type b = a require_k1
[%%expect{|
type a : k1 with t1
type b = a require_k1
|}]

(* This one should always be rejected. *)
type a : k1 with t2
type b = a require_k1

[%%expect{|
type a : k1 with t2
Line 2, characters 9-10:
2 | type b = a require_k1
             ^
Error: This type "a" should be an instance of type "('a : k1)"
       The kind of a is k1 with t2
         because of the definition of a at line 1, characters 0-19.
       But the kind of a must be a subkind of k1
         because of the definition of require_k1 at line 5, characters 0-25.
|}]

(* should be rejected *)
type a : k2 with t1 with t2
type b = a require_k1
[%%expect{|
type a : k2 with t1 with t2
Line 2, characters 9-10:
2 | type b = a require_k1
             ^
Error: This type "a" should be an instance of type "('a : k1)"
       The kind of a is k2 with t1 with t2
         because of the definition of a at line 1, characters 0-27.
       But the kind of a must be a subkind of k1
         because of the definition of require_k1 at line 5, characters 0-25.
|}]

(* should be accepted *)
module M : sig
  type a : immutable_data with t1 with t2
end = struct
  type a : immutable_data with t1 with t2
end
[%%expect{|
module M : sig type a : immutable_data with t1 with t2 end
|}]

(***************************************)
(* Test: Lack of short paths for kinds *)

(* built-in jkinds get printed as their nice names *)
type t : mutable_data mod contended
[%%expect{|
type t : sync_data
|}]

(* But there's no similar facility for user-defined names *)
kind_ my_kind = value mod portable
type t : value mod portable
[%%expect{|
kind_ my_kind = value mod portable
type t : value mod portable
|}]

(*********************************************************)
(* Test: You can't omit a kind in a functor application. *)
module F(X : sig kind_ k end) = struct end [@@warning "-191"]

module M = F(struct end)
[%%expect{|
module F : functor (X : sig kind_ k end) -> sig end
Line 3, characters 11-24:
3 | module M = F(struct end)
               ^^^^^^^^^^^^^
Error: Modules do not match: sig end is not included in sig kind_ k end
     The kind "k" is required but not provided
|}]

(*************************************************)
(* Test: substitutions happen on jkinds in tvars *)

(* For the below to pass the module inclusion check, we must subst the abstract
   kind in one of the signatures to be the same as the other. If we failed to do
   that in tvars, the below will fail when the inclusion check compares the
   parameters of the typedecls. *)
module M : sig
  kind_ k
  type ('a : k) t
end = struct
  kind_ k
  type ('a : k) t
end
[%%expect{|
module M : sig kind_ k type ('a : k) t end
|}]

(*********************************************************)
(* Test: Hidden recursive-module cycles through abstraction *)

(* This cycle is hidden by an abstract recursive signature. *)
module rec Hidden_a : sig
  kind_ ka = Hidden_b.kb
end = struct
  kind_ ka = Hidden_b.kb
end
and Hidden_b : sig
  kind_ kb
end = struct
  kind_ kb = Hidden_a.ka
end
[%%expect{|
module rec Hidden_a : sig kind_ ka = Hidden_b.kb end
and Hidden_b : sig kind_ kb end
|}]

(* This cycle is rejected when both aliases carry the same bound. *)
module rec Hidden_bounds_a : sig
  kind_ ka = Hidden_bounds_b.kb mod global
end = struct
  kind_ ka = Hidden_bounds_b.kb mod global
end
and Hidden_bounds_b : sig
  kind_ kb = Hidden_bounds_a.ka mod global
end = struct
  kind_ kb = Hidden_bounds_a.ka mod global
end
[%%expect{|
Lines 1-5, characters 0-3:
1 | module rec Hidden_bounds_a : sig
2 |   kind_ ka = Hidden_bounds_b.kb mod global
3 | end = struct
4 |   kind_ ka = Hidden_bounds_b.kb mod global
5 | end
Error: The kind "Hidden_bounds_a.ka" is cyclic:
         "Hidden_bounds_a.ka" = "Hidden_bounds_b.kb mod global",
         "Hidden_bounds_b.kb mod global" contains "Hidden_bounds_b.kb",
         "Hidden_bounds_b.kb" = "Hidden_bounds_a.ka mod global",
         "Hidden_bounds_a.ka mod global" contains "Hidden_bounds_a.ka"
|}]

module rec Hidden_nested : sig
  module A : sig
    kind_ k
  end
  kind_ k = A.k
end = struct
  module A = struct
    kind_ k = Hidden_nested.k
  end
  kind_ k = A.k
end
[%%expect{|
module rec Hidden_nested : sig module A : sig kind_ k end kind_ k = A.k end
|}]

(* This self alias is hidden by an abstract recursive signature. *)
module rec Hidden_self : sig
  kind_ k
end = struct
  kind_ k = Hidden_self.k
end
[%%expect{|
module rec Hidden_self : sig kind_ k end
|}]

module rec Hidden_local_mty_a : sig
  module type T = sig
    kind_ k
  end
  module X : T
  kind_ ka = X.k
end = struct
  module type T = sig
    kind_ k
  end
  module X = struct
    kind_ k = Hidden_local_mty_b.kb
  end
  kind_ ka = X.k
end
and Hidden_local_mty_b : sig
  kind_ kb = Hidden_local_mty_a.ka
end = struct
  kind_ kb = Hidden_local_mty_a.ka
end
[%%expect{|
module rec Hidden_local_mty_a :
  sig module type T = sig kind_ k end module X : T kind_ ka = X.k end
and Hidden_local_mty_b : sig kind_ kb = Hidden_local_mty_a.ka end
|}]

(* This cycle is rejected when the local module type exposes the alias. *)
module rec Visible_local_mty_a : sig
  module type T = sig
    kind_ k = Visible_local_mty_b.kb
  end
  module X : T
  kind_ ka = X.k
end = struct
  module type T = sig
    kind_ k = Visible_local_mty_b.kb
  end
  module X = struct
    kind_ k = Visible_local_mty_b.kb
  end
  kind_ ka = X.k
end
and Visible_local_mty_b : sig
  kind_ kb = Visible_local_mty_a.ka
end = struct
  kind_ kb = Visible_local_mty_a.ka
end
[%%expect{|
Lines 1-15, characters 0-3:
 1 | module rec Visible_local_mty_a : sig
 2 |   module type T = sig
 3 |     kind_ k = Visible_local_mty_b.kb
 4 |   end
 5 |   module X : T
...
12 |     kind_ k = Visible_local_mty_b.kb
13 |   end
14 |   kind_ ka = X.k
15 | end
Error: The kind "Visible_local_mty_a.X.k" is cyclic:
         "Visible_local_mty_a.X.k" = "Visible_local_mty_b.kb",
         "Visible_local_mty_b.kb" = "Visible_local_mty_a.ka",
         "Visible_local_mty_a.ka" = "Visible_local_mty_a.X.k"
|}]

(************************************************************)
(* Test: Asymmetric GADT refinement of abstract-kind equalities *)
(* Upshot: GADT pattern matching refines the kinds of abstract types,
   but it does currently *not* refine the kind variables.
   Doing so in the future would be safe, but we would have to be
   very careful in combination with the above hidden recursive cycles:
   if GADT matching refined kind variables, we could expose the
   hidden cycles, and the typechecker could then go into an infinite
   loop if it is not carefully rewritten to handle cycles.
   This is probably doable (after all, it works for rectypes),
   but not worth the effort right now. *)

(* Set up two abstract kinds with equal concrete definitions. *)
type ('a : any, 'b : any) branch_eq = Refl : ('x : any). ('x, 'x) branch_eq
module type Branch_empty = sig end

module Branch_kinds : sig
  kind_ k1
  kind_ k2
  type t1 : k1
  type t2 : k2
  val eq12 : (t1, t2) branch_eq
end = struct
  kind_ k1 = float64
  kind_ k2 = float64
  type t1 = float#
  type t2 = float#
  let eq12 = Refl
end
[%%expect{|
type ('a : any, 'b : any) branch_eq = Refl : ('x : any). ('x, 'x) branch_eq
module type Branch_empty = sig end
module Branch_kinds :
  sig
    kind_ k1
    kind_ k2
    type t1 : k1
    type t2 : k2
    val eq12 : (t1, t2) branch_eq
  end
|}]

type ('a : Branch_kinds.k1) branch_needs_k1
type ('a : Branch_kinds.k2) branch_needs_k2
[%%expect{|
type ('a : Branch_kinds.k1) branch_needs_k1
type ('a : Branch_kinds.k2) branch_needs_k2
|}]

(* Without a match, t2 does not satisfy k1. *)
let _ =
  (module struct
    type bad = Branch_kinds.t2 branch_needs_k1
  end : Branch_empty)
[%%expect{|
Line 3, characters 15-30:
3 |     type bad = Branch_kinds.t2 branch_needs_k1
                   ^^^^^^^^^^^^^^^
Error: This type "Branch_kinds.t2" should be an instance of type
         "('a : Branch_kinds.k1)"
       The kind of Branch_kinds.t2 is Branch_kinds.k2
         because of the definition of t2 at line 8, characters 2-14.
       But the kind of Branch_kinds.t2 must be a subkind of Branch_kinds.k1
         because of the definition of branch_needs_k1 at line 1, characters 0-43.
|}]

(* Inside the match, t2 satisfies k1 but t1 still does not satisfy k2. *)
let _ =
  match Branch_kinds.eq12 with
  | Refl ->
    (module struct
      type now_ok_1 = Branch_kinds.t2 branch_needs_k1
      type now_ok_2 = Branch_kinds.t2 branch_needs_k2
      type still_bad = Branch_kinds.t1 branch_needs_k2
    end : Branch_empty)
[%%expect{|
Line 7, characters 23-38:
7 |       type still_bad = Branch_kinds.t1 branch_needs_k2
                           ^^^^^^^^^^^^^^^
Error: This type "Branch_kinds.t1" should be an instance of type
         "('a : Branch_kinds.k2)"
       The kind of Branch_kinds.t1 is Branch_kinds.k1
         because of the definition of t1 at line 7, characters 2-14.
       But the kind of Branch_kinds.t1 must be a subkind of Branch_kinds.k2
         because of the definition of branch_needs_k2 at line 2, characters 0-43.
|}]

module Branch_kinds_extra : sig
  kind_ k1
  kind_ k2
  type t1 : k1
  type t1' : k1
  type t2 : k2
  type t2' : k2
  val eq12 : (t1, t2) branch_eq
end = struct
  kind_ k1 = float64
  kind_ k2 = float64
  type t1 = float#
  type t1' = float#
  type t2 = float#
  type t2' = float#
  let eq12 = Refl
end
[%%expect{|
module Branch_kinds_extra :
  sig
    kind_ k1
    kind_ k2
    type t1 : k1
    type t1' : k1
    type t2 : k2
    type t2' : k2
    val eq12 : (t1, t2) branch_eq
  end
|}]

(* Add more types at the same abstract kinds. *)
type ('a : Branch_kinds_extra.k1) branch_needs_k1_extra
[%%expect{|
type ('a : Branch_kinds_extra.k1) branch_needs_k1_extra
|}]

(* The refinement does *not* apply to a different type at k2. *)
let _ =
  match Branch_kinds_extra.eq12 with
  | Refl ->
    (module struct
      type test_t2 = Branch_kinds_extra.t2 branch_needs_k1_extra
      type test_t2' = Branch_kinds_extra.t2' branch_needs_k1_extra
    end : Branch_empty)
[%%expect{|
Line 6, characters 22-44:
6 |       type test_t2' = Branch_kinds_extra.t2' branch_needs_k1_extra
                          ^^^^^^^^^^^^^^^^^^^^^^
Error: This type "Branch_kinds_extra.t2'" should be an instance of type
         "('a : Branch_kinds_extra.k1)"
       The kind of Branch_kinds_extra.t2' is Branch_kinds_extra.k2
         because of the definition of t2' at line 7, characters 2-15.
       But the kind of Branch_kinds_extra.t2' must be a subkind of
           Branch_kinds_extra.k1
         because of the definition of branch_needs_k1_extra at line 1, characters 0-55.
|}]
